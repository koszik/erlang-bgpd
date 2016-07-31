-module(peer_manager).
-define(PEERS_RECORD, as, hold_time, router_id).
-record(peers, {?PEERS_RECORD}).
-define(PEERS_LOCAL_RECORD, pid, restart).
-record(peers_local, {?PEERS_LOCAL_RECORD}).
% peer -> {pid, config}
-define(STATE_RECORD, peers)
-record(state, {?STATE_RECORD}).
-export([loop/0, loop/1, init/0]).


find_peer(_State, _VRF, _IP) ->
    false.

find_peer_by_pid(_State, _Pid) ->
    false.

add_peer(State, VRF, IP, AS, Hold_Time, Router_ID) ->
    spawn_link(peer, connect, [VRF, IP, AS, Hold_Time, Router_ID]),
    State.

peer_exit(State, _Peer, _Reason) ->
    State.

loop(State) ->
    ?MODULE:loop(receive
	{add_peer, VRF, IP, AS, Hold_Time, Router_ID} when is_list(VRF), AS > 0, AS < 4294967296, Hold_Time == 0 ; Hold_Time >= 3, byte_size(Router_ID) == 4 -> % TODO: IP, Router_ID, AS?
	    add_peer(State, VRF, IP, AS, Hold_Time, Router_ID);
	{remove_peer, _VRF, _IP} -> State;
	{'EXIT', Pid, Reason} when Pid /= self() ->
	    case find_peer_by_pid(State, Pid) of
		false -> log:err("exit from unknown pid received ~p ~w~n", [Pid, Reason]),
		    State;
		Peer -> peer_exit(State, Peer, Reason)
	    end;
	X ->
	    log:err("unknown message received: ~w~n", [X]),
	    State
    end).
loop() ->
    register(peer_manager, self()),
    process_flag(trap_exit, true),
    loop([]).


init() ->
    spawn(?MODULE, loop, []).
