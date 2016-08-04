% Calculate/announce best paths for a VRF.
-module(vrf_store).
-export([init/1, vrf_store/1, show/1, show/2, new_peer/2, start/1]).
-record(store, {vrf, prefixes, outpids=[], wait_for_store_pids=[]}).
-include("peer.hrl").
% Store#store -> {active, inactive} = routes
% routes = [#route, ...]
% route -> {attribs, pid}
-record(route, {attributes, pid}).


store_set(Store, Prefix, {[], []}) ->
    Store#store{prefixes = case gb_trees:is_defined(Prefix, Store#store.prefixes) of
        true ->  gb_trees:delete(Prefix, Store#store.prefixes);
        false -> log:err("prefix not found in tree when setting to [] []! ~w~n",[Prefix]), Store#store.prefixes
    end};
store_set(Store, Prefix, Data) ->
    Store#store{prefixes = gb_trees:enter(Prefix, Data, Store#store.prefixes)}.

% {active, inactive}
store_get_all_routes(Store, Key) ->
    case gb_trees:is_defined(Key, Store#store.prefixes) of
	true -> gb_trees:get(Key, Store#store.prefixes);
	false -> {[], []}
    end.

store_size(Prefixes) ->
    gb_trees:size(Prefixes).

store_init() ->
    gb_trees:empty().


select(_Store, Elem, {[], []}) -> {[Elem], []};
select(Store, Elem, {Active, []}) ->
    [Best|_] = Active,
    case bestpath:compare_routes(Store, Best#route.attributes, Elem#route.attributes) of
	first -> {Active, [Elem]};
	{multipath, first} -> {Active ++ [Elem], []}
    end;
select(_Store, Elem, {Active, Inactive}) -> {Active, Inactive ++ [Elem]}.


del_route(Store, Pid, {OldActive, OldInactive}) ->
    Routes = [R || R <- OldActive ++ OldInactive, R#route.pid /= Pid],
    lists:foldl(fun(E, A) ->  select(Store, E, A) end, {[], []}, Routes).

remove_peer_route(Prefix, {Active, Inactive}, {Store, Prefixes, Pid}) ->
    Update =  (hd(Active))#route.pid == Pid,
    New = del_route(Store, Pid, {Active, Inactive}),
    case New of
	{[], []} -> if Update -> send_updates(withdraw, Store, Pid, Prefix); true -> true end, {Store, Prefixes, Pid};
	{[NewBest|_], _} -> if Update -> send_updates(update, Store, Prefix, NewBest#route.attributes); true -> true end, {Store, gb_trees:insert(Prefix, New, Prefixes), Pid}
    end.

remove_peer(Store, Pid) ->
    {_, NewPrefixes, _} = gb_util:fold(fun remove_peer_route/3, {Store, gb_trees:empty(), Pid}, Store#store.prefixes),
    Store#store{prefixes = NewPrefixes}.

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



send_updates(Type, Store, Pid, Prefix, Attributes) when Type == announce ; Type == update ->
    log:info("~s: ~w [~w]~n", [atom_to_list(Type), Prefix, Attributes]),
    [P ! {Type, self(), Prefix, Attributes} || P <- Store#store.outpids, P /= Pid].
send_updates(withdraw, Store, Pid, Prefix) ->
    log:info("withdraw: ~w~n", [Prefix]),
    [P ! {withdraw, self(), Prefix} || P <- Store#store.outpids, P /= Pid].

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
		    undefined -> send_updates(withdraw, Store, Pid, Prefix);
		    NewBest   -> send_updates(update, Store, Pid, Prefix, NewBest#route.attributes)
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
    if  OldBest =/= NewBest -> send_updates(announce, Store, Pid, Prefix, Attributes);
	OldBest =:= NewBest -> true
    end,
    store_set(Store, Prefix, NewRoutes).


replace_prefix(Store, Pid, Prefix, Attributes) ->
    OldRoutes = store_get_all_routes(Store, Prefix),
    {[OldBest|_], _} = OldRoutes,
    RoutesAfterDel = del_route(Store, Pid, OldRoutes),
    NewRoutes = add_route(Store, Pid, RoutesAfterDel, Attributes),
    {[NewBest|_], _} = NewRoutes,
    if  OldBest =/= NewBest ->
	    if OldBest#route.pid /= NewBest#route.pid -> NewBest#route.pid ! {withdraw, self(), Prefix};
	       true -> ok
	    end,
	    send_updates(update, Store, Pid, Prefix, NewBest#route.attributes);
	OldBest =:= NewBest -> true
    end,
    store_set(Store, Prefix, NewRoutes).


announce_all(Store, Pid) ->
	    gb_util:fold(fun(Prefix, {[Best|_], _}, _) -> Pid ! {announce, self(), Prefix, Best#route.attributes} end, [], Store#store.prefixes).

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
	{eof, Pid} ->
	    WaitPids = [X || X <- Store#store.wait_for_store_pids, X /= Pid],
	    case WaitPids of
		[] -> [X ! {eof, self()} || X <- Store#store.outpids];
		_ -> ok
	    end,
	    Store#store{wait_for_store_pids = WaitPids};
	{'DOWN', _Ref, process, Pid, Reason} ->
	    log:err("~p died: ~p~n", [Pid, Reason]),
	    NewStore = remove_peer(Store, Pid),
	    NewStore#store{ outpids = [X || X <- NewStore#store.outpids, X /= Pid],
			    wait_for_store_pids = [X || X <- NewStore#store.wait_for_store_pids, X /= Pid]};
	{new_peer, Pid} ->
	    Pid ! {vrf_store_init, self()},
	    erlang:monitor(process, Pid),
	    announce_all(Store, Pid),
	    Store#store{outpids = [Pid|Store#store.outpids]};
	{get_prefix, Prefix, Pid} ->
	    Pid ! {transfer_store, {Prefix, store_get_all_routes(Store, Prefix)}},
	    Pid ! {transfer_eof, undefined},
	    Store;
	{get_store_size, Pid} ->
	    Pid ! {store_size, store_size(Store#store.prefixes)},
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
    ok = process_manager:register({vrf_store, {VRF}}),
    PeerPids = lists:flatten(ets:match(process_store, {{prefix_store, {VRF, '_'}}, '$1'})) ++
	       lists:flatten(ets:match(process_store, {{rib, {VRF}}, '$1'})),
    lists:foldl(fun(Pid, _) -> Pid ! {vrf_store_init, self()}, erlang:monitor(process, Pid) end, [], PeerPids),
    vrf_store(#store{prefixes=store_init(), outpids=PeerPids, wait_for_store_pids=PeerPids}).

start(VRF) ->
    spawn(?MODULE, init, [VRF]).

new_peer(VRF, Pid) ->
    process_manager:cast({vrf_store, {VRF}}, {new_peer, Pid}).

print_prefix(_, _, []) -> ok;
print_prefix(Prefix, C, [Route|Rest]) ->
    io:format("~s~s => ~s ~w ~w ~w ~w ~w~n",
	    [C, util:prefix_to_list(1, Prefix),
	    util:ip_to_list(1, Route#route.attributes#attrib.next_hop),
	    Route#route.attributes#attrib.local_pref, Route#route.attributes#attrib.as_path, Route#route.attributes#attrib.origin, Route#route.attributes#attrib.community,
	    Route#route.attributes#attrib.received_at]),
    print_prefix(Prefix, C, Rest).

show_prefix(N) ->
    receive
	{transfer_store, {_Prefix, {[], []}}} -> io:format("prefix not found~n"), show_prefix(N);
	{transfer_store, {Prefix, {Active, Inactive}}} -> print_prefix(Prefix, "*", Active), print_prefix(Prefix, " ", Inactive), show_prefix(N+1);
	{transfer_eof, X} -> io:format("~p/~p prefixes total~n", [N, X]);
	M ->
	    io:format("unknown message to ~p: ~p~n", [self(), M])
    after 5000 -> timeout
    end.

show(VRF, {Prefix, Length}) ->
    process_manager:getpid({vrf_store, {VRF}}) ! {get_prefix, {Prefix, Length}, self()},
    io:format("BGP table:~n", []),
    io:format(" Network => Nexthop local_pref             as_path          communities         received_at~n", []),
    show_prefix(0);
show(Pid, verbose) ->
    Pid ! {get_store, self()},
    io:format("BGP table:~n", []),
    show_prefix(0).
show(Pid) ->
    Pid ! {get_store_size, self()},
    receive {store_size, Size} -> io:format("BGP table size: ~p~n", [Size]) after 5000 -> timeout end.
