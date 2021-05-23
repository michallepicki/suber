-module(suber_bindings).
-export([new/0, get/2, insert/3 , unwind/2, in_child_scope/2]).

new() ->
  Table = ets:new(suber_bindings, [set, private]),
  Changes = [],
  Length = 0,
  {Table, Changes, Length}.

get(Bindings, Key) ->
  {Table, _, _} = Bindings,
  case ets:lookup(Table, Key) of
    [] -> nil;
    [{_, Value}] -> Value
  end.

insert(Bindings, Key, Value) ->
  OldValue = get(Bindings, Key),
  {Table, Changes, Length} = Bindings,
  ets:insert(Table, {Key, Value}),
  NewChanges = [{Key, OldValue} | Changes],
  {Table, NewChanges, Length + 1}.

unwind(Bindings, ToLength) ->
  {Table, Changes, Length} = Bindings,
  case Length of
    ToLength -> Bindings;
    _ ->
      [{Key, OldValue} | NewChanges] = Changes,
      case OldValue of
        nil -> ets:delete(Table, Key);
        _ -> ets:insert(Table, {Key, OldValue})
      end,
      unwind({Table, NewChanges, Length - 1}, ToLength)
  end.

in_child_scope(Bindings, CallbackFun) ->
  {_, _, Length} = Bindings,
  {ChangedBindings, CallbackResult} = CallbackFun(Bindings),
  RestoredBindings = unwind(ChangedBindings, Length),
  {RestoredBindings, CallbackResult}.
