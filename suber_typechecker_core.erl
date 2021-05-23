-module(suber_typechecker_core).
-export([new/0, var/1, bool/1, bool_use/1, func/3, func_use/3, obj/2, obj_use/2, case_/2, case_use/2, flow/3]).

new() ->
  Reachability = suber_reachability:new(),
  TypesTable = ets:new(suber_typechecker_core, [ordered_set, private]),
  {Reachability, TypesTable}.

bool(Core) ->
  new_val(Core, vbool).

bool_use(Core) ->
  new_use(Core, vbool).

func(Core, Arg, Ret) ->
  new_val(Core, {vfunc, Arg, Ret}).

func_use(Core, Arg, Ret) ->
  new_use(Core, {ufunc, Arg, Ret}).

obj(Core, Fields) ->
  new_val(Core, {vobj, Fields}).

obj_use(Core, Field) ->
  new_use(Core, {uobj, Field}).

case_(Core, Case) ->
  new_val(Core, {vcase, Case}).

case_use(Core, Cases) ->
  new_use(Core, {ucase, Cases}).

new_val(Core, ValueType) ->
  Id = new_type(Core, {value, ValueType}),
  {value, Id}.

new_use(Core, Constraint) ->
  Id = new_type(Core, {use, Constraint}),
  {use, Id}.

var(Core) ->
  Id = new_type(Core, var),
  {{value, Id}, {use, Id}}.

new_type(Core, Type) ->
  {Reachability, TypesTable} = Core,
  Id = suber_reachability:add_node(Reachability),
  ets:insert(TypesTable, {Id, Type}),
  Id.

flow(Core, ValueLhs, UseRhs) ->
  {value, Lhs} = ValueLhs,
  {use, Rhs} = UseRhs,
  do_flow(Core, [{Lhs, Rhs}], []).

do_flow(Core, PendingEdges, TypePairsToCheck) ->
  {Reachability, TypesTable} = Core,
  case {PendingEdges, TypePairsToCheck} of
    {[], []} -> ok;
    {[{Lhs, Rhs} | RemainingPendingEdges], []} ->
      NewTypePairsToCheck = suber_reachability:add_edge(Reachability, Lhs, Rhs),
      do_flow(Core, RemainingPendingEdges, NewTypePairsToCheck);
    {_, [{Lhs, Rhs} | RemainingTypePairsToCheck]} ->
      NewPendingEdges =
        case {ets:lookup_element(TypesTable, Lhs, 2), ets:lookup_element(TypesTable, Rhs, 2)} of
          {var, _} -> PendingEdges;
          {_, var} -> PendingEdges;
          {{value, LhsHead}, {use, RhsHead}} -> check_heads(LhsHead, RhsHead, PendingEdges)
        end,
      do_flow(Core, NewPendingEdges, RemainingTypePairsToCheck)
  end.

check_heads(Lhs, Rhs, Out) ->
  case {Lhs, Rhs} of
    {vbool, ubool} -> Out;
    {{vfunc, {use, Arg1}, {value, Ret1}}, {ufunc, {value, Arg2}, {use, Ret2}}} ->
        [{Arg2, Arg1}, {Ret1, Ret2} | Out];
    {{vobj, Fields}, {uobj, {Name, {use, Rhs2}}}} ->
      case maps:get(Name, Fields, nil) of
        nil -> throw({missing_obj_field, Name});
        {value, Lhs2} -> [{Lhs2, Rhs2} | Out]
      end;
    {{vcase, {Name, {value, Lhs2}}}, {ucase, Cases}} ->
      case maps:get(Name, Cases, nil) of
        nil -> throw({unhandled_case, Name});
        {use, Rhs2} -> [{Lhs2, Rhs2} | Out]
      end;
    T -> throw({unexpected_types, T})
  end.