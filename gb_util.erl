-module(gb_util).
-export([fold/3]).


fold(_Fun, Acc, none) ->
    Acc;
fold(Fun, Acc, {Key, Value, Iter}) ->
    fold(Fun, Fun(Key, Value, Acc), gb_trees:next(Iter));
fold(Fun, Acc, Prefixes) ->
    fold(Fun, Acc, gb_trees:next(gb_trees:iterator(Prefixes))).
