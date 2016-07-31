-module(log).
-export([debug/2, info/2, err/2]).

lookup() ->
    self().
%    ets:lookup(self(), ..


debug(Format, Params) ->
    X = lookup(),
    io:format("DEBUG: ~p: "++Format, [X|Params]).

info(Format, Params) ->
    X = lookup(),
    io:format("INFO: ~p: "++Format, [X|Params]).


err(Format, Params) ->
    X = lookup(),
    io:format("ERR: ~p: "++Format, [X|Params]).


