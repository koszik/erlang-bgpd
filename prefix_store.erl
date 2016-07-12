-module(prefix_store).
-export([prefix_store/0, prefix_store/1, show/1, show/2]).
-define(STORE_RECORD, prefixes, maximum_prefixes).
-record(store, {?STORE_RECORD}).
-include("peer.hrl").

add_prefix(Prefixes, Prefix, Length, Attribs) ->
    dict:store({Prefix, Length}, Attribs, Prefixes).

remove_prefix(Prefixes, Prefix, Length) ->
    dict:erase({Prefix, Length}, Prefixes).


% TODO: multiple prefixes not allowed with same prefix
prefix_store(Store) ->
    ?MODULE:prefix_store(
	receive
	{announce, Prefix, Length, Attribs} ->
	    Size = dict:size(Store#store.prefixes),
	    if Store#store.maximum_prefixes /= undefined, Size >= Store#store.maximum_prefixes ->
		io:format("~p maximum prefixes (~p) reached, exiting~n", [self(), Store#store.maximum_prefixes]),
		exit(self(), maximum_prefixes);
		true -> Store#store{prefixes=add_prefix(Store#store.prefixes, Prefix, Length, Attribs)}
	    end;
	{withdraw, Prefix, Length, _Attribs} ->
	    Store#store{prefixes=remove_prefix(Store#store.prefixes, Prefix, Length)};
	{get_store_size, Pid} ->
	    Pid ! {store_size, dict:size(Store#store.prefixes)},
	    Store;
	{get_store, Pid} ->
	    dict:fold(fun(Key, Value, _AccIn) -> Pid ! {transfer_store, {Key, Value}} end, 0, Store#store.prefixes),
	    Pid ! {transfer_eof},
	    Store;
	{maximum_prefixes, Maximum_Prefixes} ->
	    Store#store{maximum_prefixes = Maximum_Prefixes};
	M ->
	    io:format("unknown message to ~p: ~p~n", [self(), M]),
	    Store
    end).

prefix_store() ->
    prefix_store(#store{prefixes=dict:new()}).

print_prefix(Prefix, Attrib) ->
    io:format("~w => ~w ~w ~w ~w~n",
	    [Prefix, Attrib#attrib.next_hop, Attrib#attrib.local_pref, Attrib#attrib.as_path, Attrib#attrib.origin]).
%    lists:foldl(fun(Elem, _Acc) -> io:format("        ~p ~p~n", [Elem#attrib.type_decoded, Elem#attrib.data_decoded]) end, 0, Attrib).

show_prefix(N) ->
    receive
	{transfer_store, {Prefix, Attrib}} -> print_prefix(Prefix, Attrib),
			    show_prefix(N+1);
	{transfer_eof} -> io:format("~p prefixes total~n", [N])
    after 5000 -> timeout
    end.

show(Pid, verbose) ->
    Pid ! {get_store, self()},
    io:format("BGP table:~n", []),
    show_prefix(0).
show(Pid) ->
    Pid ! {get_store_size, self()},
    receive {store_size, Size} -> io:format("BGP table size: ~p~n", [Size]) after 5000 -> timeout end.
