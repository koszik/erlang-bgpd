-module(gb_util).
-export([fold/3, filtermap/2]).


fold(_Fun, Acc, none) ->
    Acc;
fold(Fun, Acc, {Key, Value, Iter}) ->
    fold(Fun, Fun(Key, Value, Acc), gb_trees:next(Iter));
fold(Fun, Acc, Tree) ->
    fold(Fun, Acc, gb_trees:next(gb_trees:iterator(Tree))).


filtermap(Key, Value, {Fun, Acc}) ->
    {Fun, case Fun({Key, Value}) of
	false -> Acc;
	true -> gb_trees:insert(Key, Value, Acc);
	{true, NewValue} -> gb_trees:insert(Key, NewValue, Acc)
    end}.
filtermap(Fun, Tree) ->
    fold(fun filtermap/3, {Fun, gb_store:empty()}, Tree).
