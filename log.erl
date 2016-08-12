-module(log).
-export([debug/2, info/2, err/2]).

lookup() ->
    case ets:match(process_store, {'$1', self()}) of
	[[Pid]] -> Pid;
	[] -> self()
    end.


debug(Format, Params) ->
    ok.
%    X = lookup(),
%    io:format("DEBUG: ~p: "++Format, [X|Params]).

info(Format, Params) ->
    X = lookup(),
%    ok.
    io:format("INFO: ~p: "++Format, [X|Params]).


err(Format, Params) ->
    X = lookup(),
    io:format("ERR: ~p: "++Format, [X|Params]).


