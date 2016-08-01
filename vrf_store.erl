% Calculate/announce best paths for a VRF.
-module(vrf_store).
-export([init/1, vrf_store/1, show/1, show/2]).
-record(store, {vrf, active, inactive, filtered}).
-include("peer.hrl").
% Store#store -> {active, inactive} = routes
% routes = [#route, ...]
% route -> {attribs, pid}
-record(route, {attributes, pid}).


store_set(Store, Key, {Active, Inactive}) ->
    S1 = Store#store{active = gb_trees:enter(Key, Active, Store#store.active)},
    S1#store{inactive = gb_trees:enter(Key, Inactive, S1#store.inactive)}.

store_get_all_routes(Store, Key) ->
    {
    case gb_trees:is_defined(Key, Store#store.active) of
	true -> gb_trees:get(Key, Store#store.active);
	false -> []
    end,
    case gb_trees:is_defined(Key, Store#store.inactive) of
	true -> gb_trees:get(Key, Store#store.inactive);
	false -> []
    end
    }.

store_size(Prefixes) ->
    gb_trees:size(Prefixes).

store_init() ->
    gb_trees:empty().

gb_fold(_Fun, Acc, none) ->
    Acc;
gb_fold(Fun, Acc, {Key, Value, Iter}) ->
    gb_fold(Fun, Fun(Key, Value, Acc), gb_trees:next(Iter));
gb_fold(Fun, Acc, Prefixes) ->
    gb_fold(Fun, Acc, gb_trees:next(gb_trees:iterator(Prefixes))).


select(_Store, Elem, {[], []}) -> {[Elem], []};
select(Store, Elem, {Active, []}) ->
    [Best|_] = Active,
    case bestpath:compare_routes(Store, Best#route.attributes, Elem#route.attributes) of
	first -> {Active, [Elem]};
	multipath_first -> {Active ++ [Elem], []}
    end;
select(_Store, Elem, {Active, Inactive}) -> {Active, Inactive ++ [Elem]}.


del_route(Store, Pid, {OldActive, OldInactive}) ->
    Routes = lists:filter(fun(R) -> R#route.pid /= Pid end, OldActive ++ OldInactive),
    lists:foldl(fun(E, A) ->  select(Store, E, A) end, {[], []}, Routes).

%% TODO: don't re-sort
add_route(Store, Pid, {OldActive, OldInactive}, Attributes) ->
    Routes = lists:sort(
	fun(A,B) -> case bestpath:compare_routes(Store, A#route.attributes, B#route.attributes) of
			first -> true;
			{multipath, first} -> true;
			second -> false;
			{multipath, second} -> false
		    end
	end,
	[#route{pid=Pid, attributes=Attributes}|OldActive] ++ OldInactive),
    lists:foldl(fun(E, A) ->  select(Store, E, A) end, {[], []}, Routes).



send_updates(announce, _Store, Prefix, Attributes) ->
    log:info("announce: ~w [~w]~n", [Prefix, Attributes]).
send_updates(withdraw, _Store, Prefix) ->
    log:info("withdraw: ~w~n", [Prefix]).

%% TODO: multipath is not advertised to rib

remove_prefix(Store, Pid, Prefix) ->
    OldRoutes = store_get_all_routes(Store, Prefix),
    {[OldBest|_], _} = OldRoutes,
    NewRoutes = del_route(Store, Pid, OldRoutes),
    NewBest = case NewRoutes of
		{[], []} -> undefined;
		{[Best|_], _} -> Best
	      end,
    if  OldBest =/= NewBest ->
		case NewBest of
		    undefined -> send_updates(withdraw, Store, Prefix);
		    NewBest when is_record(NewBest, route) -> send_updates(announce, Store, Prefix, NewBest#route.attributes);
		    NewBest -> io:format("????????????? ~w~n~w~n", [NewBest, NewRoutes]), exit(err)
		end;
	OldBest =:= NewBest -> true
    end,
    store_set(Store, Prefix, NewRoutes).


add_prefix(Store, Pid, Prefix, Attributes) ->
    OldRoutes = store_get_all_routes(Store, Prefix),
    OldBest = case OldRoutes of
		{[], []} -> undefined;
		{[Best|_], _} -> Best
	      end,
    NewRoutes = add_route(Store, Pid, OldRoutes, Attributes),
    {[NewBest|_], _} = NewRoutes,
    if  OldBest =/= NewBest -> send_updates(announce, Store, Prefix, Attributes);
	OldBest =:= NewBest -> true
    end,
    store_set(Store, Prefix, NewRoutes).


replace_prefix(Store, Pid, Prefix, Attributes) ->
    OldRoutes = store_get_all_routes(Store, Prefix),
    {[OldBest|_], _} = OldRoutes,
    RoutesAfterDel = del_route(Store, Pid, OldRoutes),
    NewRoutes = add_route(Store, Pid, RoutesAfterDel, Attributes),
    {[NewBest|_], _} = NewRoutes,
    if  OldBest =/= NewBest -> send_updates(announce, Store, Prefix, Attributes);
	OldBest =:= NewBest -> true
    end,
    store_set(Store, Prefix, NewRoutes).



vrf_store(Store) ->
	X = receive A -> A end,
	log:info("rcv: ~w~n", [X]),
    ?MODULE:vrf_store(
	case X of
	{announce, Pid, Prefix, Attribs} ->
	    add_prefix(Store, Pid, Prefix, Attribs);
	{update, Pid, Prefix, Attribs} ->
	    replace_prefix(Store, Pid, Prefix, Attribs);
	{withdraw, Pid, Prefix} ->
	    remove_prefix(Store, Pid, Prefix);
	{get_prefix, Prefix, Pid} ->
	    Pid ! {transfer_store, {Prefix, store_get_all_routes(Store, Prefix)}},
	    Pid ! {transfer_eof, undefined},
	    Store;
	{get_store_size, Pid} ->
	    Pid ! {store_size, store_size(Store#store.active)},
	    Store;
	{get_store, _Pid} ->
%	    N = store_iter(fun(Key, Value, AccIn) -> Pid ! {transfer_store, {Key, Value}}, AccIn+1 end, 0, Store#store.prefixes),
%	    Pid ! {transfer_eof, N},
	    Store;
	M ->
	    log:err("unknown message to ~p: ~p~n", [self(), M]),
	    Store
    end).

init(VRF) ->
    register(list_to_atom("vrf_store_"++VRF), spawn(?MODULE, vrf_store, [#store{active=store_init(), inactive=store_init()}])).

print_prefix(_, _, []) -> ok;
print_prefix(Prefix, C, [Route|Rest]) ->
    io:format("~s~w => ~w ~w ~w ~w ~w~n",
	    [C, Prefix,
	    Route#route.attributes#attrib.next_hop, Route#route.attributes#attrib.local_pref, Route#route.attributes#attrib.as_path, Route#route.attributes#attrib.origin, Route#route.attributes#attrib.community]),
    print_prefix(Prefix, C, Rest).

show_prefix(N) ->
    receive
	{transfer_store, {Prefix, {[], []}}} -> io:format("prefix not found~n"), show_prefix(N);
	{transfer_store, {Prefix, {Active, Inactive}}} -> print_prefix(Prefix, "*", Active), print_prefix(Prefix, " ", Inactive), show_prefix(N+1);
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
