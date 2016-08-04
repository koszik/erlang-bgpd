-module(prefix_store).
-export([prefix_store/1, show/1, show/2]).
-record(store, {vrf, peer, prefixes, out_prefixes, maximum_prefixes, vrf_store}).
-include("peer.hrl").

get_prefix(Prefixes, Key) ->
    case gb_trees:is_defined(Key, Prefixes) of
	true -> gb_trees:get(Key, Prefixes);
	false -> notfound
    end.


vrfstore_send(Store, Msg) when is_pid(Store#store.vrf_store) ->
    Store#store.vrf_store ! Msg;
vrfstore_send(_Store, _Msg) ->
    ok.


add_prefix(Store, Prefix, Attribs) ->
    case catch(gb_trees:insert(Prefix, Attribs, Store#store.prefixes)) of
	{'EXIT', _} ->
	    vrfstore_send(Store, {update, self(), Prefix, Attribs}),
	    gb_trees:enter(Prefix, Attribs, Store#store.prefixes);
	Tree ->
	    vrfstore_send(Store, {announce, self(), Prefix, Attribs}),
	    Tree
    end.


remove_prefix(Store, Prefix) ->
    case gb_trees:is_defined(Prefix, Store#store.prefixes) of
	true ->  vrfstore_send(Store, {withdraw, self(), Prefix}),
		 gb_trees:delete(Prefix, Store#store.prefixes);
	false -> log:err("prefix not found in tree! ~w~n",[Prefix]), Store#store.prefixes
    end.


add_out_prefix(Store, Prefix, Attribs) ->
    peer:send_update(update, Store#store.peer, Prefix, Attribs),
    Store#store{out_prefixes = case catch(gb_trees:insert(Prefix, Store#store.vrf_store, Store#store.out_prefixes)) of
	{'EXIT', _} ->
	    gb_trees:enter(Prefix, Attribs, Store#store.out_prefixes);
	Tree ->
	    Tree
    end}.

update_out_prefix(Store, Prefix, Attribs) ->
    peer:send_update(update, Store#store.peer, Prefix, Attribs),
    Store#store{out_prefixes = gb_trees:enter(Prefix, Store#store.vrf_store, Store#store.out_prefixes)}.


remove_out_prefix(Store, Prefix) ->
    Store#store{out_prefixes = case gb_trees:is_defined(Prefix, Store#store.prefixes) of
	true ->  peer:send_update(withdraw, Store#store.peer, Prefix),
		 gb_trees:delete(Prefix, Store#store.out_prefixes);
	false -> log:err("prefix not found in out tree! ~w~n",[Prefix]), Store#store.out_prefixes
    end}.


remove_old_prefixes(Store) ->
    P = Store#store.vrf_store,
    Store#store{out_prefixes=gb_trees:filtermap(
	fun({Prefix, Value}) ->
		case Value of
		    P -> ok;
		    _ -> peer:send_update(withdraw, Store#store.peer, Prefix)
		end
	    end,    Store#store.out_prefixes)}.


store_size(Prefixes) ->
    gb_trees:size(Prefixes).

store_init() ->
    gb_trees:empty().

gb_enumerate(Fun, Acc, {Key, Value, Iter}) ->
    gb_enumerate(Fun, Fun(Key, Value, Acc), gb_trees:next(Iter));
gb_enumerate(_Fun, Acc, none) ->
    Acc.

store_iter(Fun, Acc, Prefixes) ->
    gb_enumerate(Fun, Acc, gb_trees:next(gb_trees:iterator(Prefixes))).


prefix_store(VRF) when is_list(VRF) ->
    receive
	{peer_state, Peer} -> prefix_store(#store{vrf=VRF, peer=Peer, prefixes=store_init(), out_prefixes=store_init()})
    end;

prefix_store(Store) when is_record(Store, store) ->
    ?MODULE:prefix_store(
    receive
	{announce, Prefix, Attribs} ->
	    Size = store_size(Store#store.prefixes),
	    if Store#store.maximum_prefixes /= undefined, Size >= Store#store.maximum_prefixes ->
		log:err("~p maximum prefixes (~p) reached, exiting~n", [self(), Store#store.maximum_prefixes]),
		exit(maximum_prefixes);
		true -> Store#store{prefixes = add_prefix(Store, Prefix, Attribs)}
	    end;
	{withdraw, Prefix, []} ->
	    Store#store{prefixes = remove_prefix(Store, Prefix)};
	{vrf_store_init, Pid} ->
	    store_iter(fun(Prefix, Attribs, _) -> Pid ! {announce, self(), Prefix, Attribs} end, [], Store#store.prefixes),
	    Store#store{vrf_store = Pid};
	{announce, _Pid, Prefix, Attribs} -> add_out_prefix(Store, Prefix, Attribs);
	{update, _Pid, Prefix, Attribs} -> update_out_prefix(Store, Prefix, Attribs);
	{withdraw, _Pid, Prefix} -> remove_out_prefix(Store, Prefix);
	{eof, _Pid} -> remove_old_prefixes(Store);
	{get_prefix, Prefix, Pid} ->
	    Pid ! {transfer_store, {Prefix, get_prefix(Store#store.prefixes, Prefix)}},
	    Pid ! eof,
	    Store;
	{get_store_size, Pid} ->
	    Pid ! {store_size, store_size(Store#store.prefixes)},
	    Store;
	{get_store, Pid} ->
	    store_iter(fun(Key, Value, _) -> Pid ! {transfer_store, {Key, Value}} end, 0, Store#store.prefixes),
	    Pid ! eof,
	    Store;
	{maximum_prefixes, Maximum_Prefixes} ->
	    Store#store{maximum_prefixes = Maximum_Prefixes};
	Unknown ->
	    log:err("unknown message to ~p: ~p~n", [self(), Unknown]),
	    Store
    end).


print_prefix(Prefix, Attrib) ->
    io:format("~w => ~w ~w ~w ~w~n",
	    [Prefix, Attrib#attrib.next_hop, Attrib#attrib.local_pref, Attrib#attrib.as_path, Attrib#attrib.origin]).

show_prefix(N) ->
    receive
	{transfer_store, {_Prefix, notfound}} -> io:format("prefix not found~n"), show_prefix(N);
	{transfer_store, {Prefix, Attrib}} -> print_prefix(Prefix, Attrib), show_prefix(N+1);
	eof -> io:format("~p prefixes total~n", [N]);
	Unknown -> io:format("unknown message to ~p: ~p~n", [self(), Unknown])
    after 5000 -> timeout
    end.

show(Pid, {Prefix, Length}) ->
    Pid ! {get_prefix, {Prefix, Length}, self()},
    io:format("BGP table:~n", []),
    show_prefix(0);
show(Pid, verbose) ->
    Pid ! {get_store, self()},
    io:format("BGP table:~n", []),
    show_prefix(0).
show(Pid) ->
    Pid ! {get_store_size, self()},
    receive
	{store_size, Size} -> io:format("BGP table size: ~p~n", [Size])
    after 5000 -> timeout
    end.
