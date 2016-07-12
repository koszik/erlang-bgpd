-module(prefix_store).
-export([prefix_store/0, prefix_store/1, show/1, show/2]).
-define(STORE_RECORD, prefixes, maximum_prefixes).
-record(store, {?STORE_RECORD}).
-include("peer.hrl").

add_prefix(Prefixes, Prefix, Length, Attribs) ->
    gb_trees:enter({Prefix, Length}, Attribs, Prefixes).

remove_prefix(Prefixes, Prefix, Length) ->
    case gb_trees:is_defined({Prefix, Length}, Prefixes) of
	true -> gb_trees:delete({Prefix, Length}, Prefixes);
	false -> io:format("prefix not found in tree! ~w~n",[{Prefix, Length}]), Prefixes
    end.

get_prefix(Prefixes, Key) ->
    case gb_trees:is_defined(Key, Prefixes) of
	true -> gb_trees:get(Key, Prefixes);
	false -> notfound
    end.

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


% TODO: multiple prefixes not allowed with same prefix
prefix_store(Store) ->
    ?MODULE:prefix_store(
	receive
	{announce, Prefix, Length, Attribs} ->
	    Size = store_size(Store#store.prefixes),
	    if Store#store.maximum_prefixes /= undefined, Size >= Store#store.maximum_prefixes ->
		io:format("~p maximum prefixes (~p) reached, exiting~n", [self(), Store#store.maximum_prefixes]),
		exit(maximum_prefixes);
		true -> Store#store{prefixes=add_prefix(Store#store.prefixes, Prefix, Length, Attribs)}
	    end;
	{withdraw, Prefix, Length, _Attribs} ->
	    Store#store{prefixes=remove_prefix(Store#store.prefixes, Prefix, Length)};
	{get_prefix, Prefix, Pid} ->
	    Pid ! {transfer_store, {Prefix, get_prefix(Store#store.prefixes, Prefix)}},
	    Pid ! {transfer_eof, undefined},
	    Store;
	{get_store_size, Pid} ->
	    Pid ! {store_size, store_size(Store#store.prefixes)},
	    Store;
	{get_store, Pid} ->
	    N = store_iter(fun(Key, Value, AccIn) -> Pid ! {transfer_store, {Key, Value}}, AccIn+1 end, 0, Store#store.prefixes),
	    Pid ! {transfer_eof, N},
	    Store;
	{maximum_prefixes, Maximum_Prefixes} ->
	    Store#store{maximum_prefixes = Maximum_Prefixes};
	M ->
	    io:format("unknown message to ~p: ~p~n", [self(), M]),
	    Store
    end).

prefix_store() ->
    prefix_store(#store{prefixes=store_init()}).

print_prefix(Prefix, Attrib) ->
    io:format("~w => ~w ~w ~w ~w~n",
	    [Prefix, Attrib#attrib.next_hop, Attrib#attrib.local_pref, Attrib#attrib.as_path, Attrib#attrib.origin]).

show_prefix(N) ->
    receive
	{transfer_store, {_Prefix, notfound}} -> io:format("prefix not found~n"), show_prefix(N);
	{transfer_store, {Prefix, Attrib}} -> print_prefix(Prefix, Attrib), show_prefix(N+1);
	{transfer_eof, X} -> io:format("~p/~p prefixes total~n", [N, X]);
	M ->
	    io:format("unknown message to ~p: ~p~n", [self(), M])
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
    receive {store_size, Size} -> io:format("BGP table size: ~p~n", [Size]) after 5000 -> timeout end.
