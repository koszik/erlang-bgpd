-module(linux_rib).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start/0]).
-include("peer.hrl").
-define(PROTO, "42").

handle_call(Msg, From, State) ->
    log:err("unknown call received from ~p: ~w~n", [From, Msg]),
    {noreply, State}.


handle_info({announce, _Pid, Prefix, Attribs}, State) ->
    log:debug("announce: ~w~n", [[Prefix, Attribs]]),
    os:cmd("ip ro add "++util:prefix_to_list(1, Prefix)++" via "++util:ip_to_list(1, Attribs#attrib.next_hop)++" proto "++?PROTO),
    {noreply, State};

handle_info({update, _Pid, Prefix, Attribs}, State) ->
    log:debug("update: ~w~n", [[Prefix, Attribs]]),
    os:cmd("ip ro change "++util:prefix_to_list(1, Prefix)++" via "++util:ip_to_list(1, Attribs#attrib.next_hop)++" proto "++?PROTO),
    {noreply, State};

handle_info({withdraw, _Pid, Prefix}, State) ->
    log:debug("withdraw: ~w~n", [Prefix]),
    os:cmd("ip ro del "++util:prefix_to_list(1, Prefix)++" proto "++?PROTO),
    {noreply, State};

handle_info({vrf_store_init, _Pid}, State) ->
    log:debug("store init~n", []),
    log:debug("route del: ~s~n", [os:cmd("ip ro ls proto "++?PROTO++"|sed 's/^/ip ro del /'|sh -")]),
    {noreply, State};

handle_info(Msg, State) ->
    log:err("unknown message received: ~w~n", [Msg]),
    {noreply, State}.


handle_cast(Msg, State) ->
    log:err("unknown cast received: ~w~n", [Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    log:debug("route del: ~s~n", [os:cmd("ip ro ls proto "++?PROTO++"|sed 's/^/ip ro del /'|sh -")]),
    ok.


init([]) ->
    VRF="global",
    process_manager:register({rib,{VRF}}),
    process_manager:cast({vrf_store, {VRF}}, {new_peer, self()}),
    log:debug("route del: ~s~n", [os:cmd("ip ro ls proto "++?PROTO++"|sed 's/^/ip ro del /'|sh -")]),
    {ok, []}.


%%%

start() ->
    gen_server:start(?MODULE, [], []).
