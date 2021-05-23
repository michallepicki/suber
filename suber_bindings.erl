-module(suber_bindings).
-export([new/0, get/2, insert/3 , unwind/2, in_child_scope/2]).

new() ->
  Table = ets:new(suber_bindings, [set, private]),
  Changes = ets:new(suber_bindings, [set, private]),
  Length = counters:new(1, []),
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
  ets:insert(Changes, {counters:get(Length, 1), {Key, OldValue}}),
  counters:add(Length, 1, 1),
  ok.

unwind(Bindings, ToLength) ->
  {Table, Changes, Length} = Bindings,
  case counters:get(Length, 1) of
    ToLength -> ok;
    _ ->
      counters:sub(Length, 1, 1),
      NewLength = counters:get(Length, 1),
      {Key, OldValue} = ets:lookup_element(Changes, NewLength, 2),
      ets:delete(Changes, NewLength),
      case OldValue of
        nil -> ets:delete(Table, Key);
        _ -> ets:insert(Table, {Key, OldValue})
      end,
      unwind({Table, Changes, Length}, ToLength)
  end.

in_child_scope(Bindings, CallbackFun) ->
  {_, _, Length} = Bindings,
  BeforeLength = counters:get(Length, 1),
  CallbackResult = CallbackFun(),
  ok = unwind(Bindings, BeforeLength),
  CallbackResult.
