-module(suber_reachability).
-export([new/0, add_node/1, add_edge/3]).

new() ->
  NextIdRef = counters:new(1, []),
  Upsets = ets:new(suber_reachability, [set, private]),
  Downsets = ets:new(suber_reachability, [set, private]),
  {NextIdRef, Upsets, Downsets}.

add_node(Reachability) ->
  {NextIdRef, Upsets, Downsets} = Reachability,
  Id = counters:get(NextIdRef, 1),
  counters:add(NextIdRef, 1, 1),
  ets:insert(Upsets, {Id, ets:new(suber_reachability, [ordered_set, private])}),
  ets:insert(Downsets, {Id, ets:new(suber_reachability, [ordered_set, private])}),
  Id.

add_edge(Reachability, Lhs, Rhs) ->
  do_add_edge(Reachability, [{Lhs, Rhs}], []).

do_add_edge(Reachability, Work, Out) ->
  case Work of
    [] -> lists:reverse(Out);
    [{Lhs, Rhs} | RemainingWork] ->
      {_, Upsets, Downsets} = Reachability,
      DownsetsLhs = ets:lookup_element(Downsets, Lhs, 2),
      {NewWork, NewOut} = case ets:insert_new(DownsetsLhs, {Rhs}) of
        false -> {RemainingWork, Out};
        true ->
          UpsetsRhs = ets:lookup_element(Upsets, Rhs, 2),
          ets:insert(UpsetsRhs, {Lhs}),
          UpsetsLhs = ets:lookup_element(Upsets, Lhs, 2),
          WorkWithUpsets = ets:foldl(fun ({Lhs2}, Acc) -> [{Lhs2, Rhs} | Acc] end, Work, UpsetsLhs),
          DownsetsRhs = ets:lookup_element(Downsets, Rhs, 2),
          WorkWithUpsetsDownsets = ets:foldl(fun ({Rhs2}, Acc) -> [{Lhs, Rhs2} | Acc] end, WorkWithUpsets, DownsetsRhs),
          {WorkWithUpsetsDownsets, [{Lhs, Rhs} | Out]}
      end,
      do_add_edge(Reachability, NewWork, NewOut)
  end.
