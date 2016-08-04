-module(process_manager).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start/0, register/1, register/2, cast/2, show/1]).

handle_call({register, ID, Pid}, _From, State) ->
    case ets:insert_new(process_store, {ID, Pid}) of
	false -> {reply, already_exists, State};
	true -> erlang:monitor(process, Pid), {reply, ok, State}
    end;

handle_call(Msg, From, State) ->
    log:err("unknown call received from ~p: ~w~n", [From, Msg]),
    {noreply, State}.


handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(process_store, {'_', Pid}),
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
    ok.


init([]) ->
    ets:new(process_store, [set, named_table, {keypos, 1}]),
    {ok, []}.


%%%

register(ID) ->
    gen_server:call(?MODULE, {register, ID, self()}).

register(ID, Pid) ->
    gen_server:call(?MODULE, {register, ID, Pid}).

cast(Selector, Msg) ->
    case ets:match(process_store, {Selector, '$1'}) of
	[] -> notfound;
	[[Pid]] -> Pid ! Msg, ok
    end.

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

show(all) ->
    ets:match(process_store, '$1').
