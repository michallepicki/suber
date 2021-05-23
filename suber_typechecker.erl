-module(suber_typechecker).
-export([new/0, check_script/3]).

new() ->
  Core = suber_typechecker_core:new(),
  Bindings = suber_bindings:new(),
  {Core, Bindings}.

check_script(Core, Bindings, Parsed) ->
  {T, _, _} = lists:foldl(fun (ToplevelItem, BindingsAcc) ->
    check_toplevel(Core, BindingsAcc, ToplevelItem)
  end, Bindings, Parsed),
  {Core, {T, [], 0}}.

check_toplevel(Core, Bindings, ToplevelItem) ->
  case ToplevelItem of
    {top_level_let_def, {var_definition, Name, VarExpr}} ->
      {Bindings1, VarType} = check_expr(Core, Bindings, VarExpr),
      suber_bindings:insert(Bindings1, Name, VarType);
    {top_level_let_rec_def, Defs} ->
      {Bindings1, TempBoundsWithExprs} = lists:foldl(fun ({var_definition, Name, VarExpr}, {BindingsAcc, TempBoundsWithExprsAcc}) ->
        {TempType, TempBound} = suber_typechecker_core:var(Core),
        {suber_bindings:insert(BindingsAcc, Name, TempType), [{TempBound, VarExpr} | TempBoundsWithExprsAcc]}
      end, {Bindings, []}, Defs),
      lists:foldl(fun ({Bound, VarExpr}, BindingsAcc) ->
        {ResultingBindings, VarType} = check_expr(Core, BindingsAcc, VarExpr),
        suber_typechecker_core:flow(Core, VarType, Bound),
        ResultingBindings
      end, Bindings1, TempBoundsWithExprs)
  end.

check_expr(Core, Bindings, Expr) ->
  case Expr of
    {call_expr, FuncExpr, ArgExpr} ->
      {Bindings1, FuncType} = check_expr(Core, Bindings, FuncExpr),
      {Bindings2, ArgType} = check_expr(Core, Bindings1, ArgExpr),
      {RetType, RetBound} = suber_typechecker_core:var(Core),
      Bound = suber_typechecker_core:func_use(Core, ArgType, RetBound),
      suber_typechecker_core:flow(Core, FuncType, Bound),
      {Bindings2, RetType};
    {case_expr, Tag, ValExpr} ->
      {Bindings1, ValType} = check_expr(Core, Bindings, ValExpr),
      {Bindings1, suber_typechecker_core:case_(Core, {Tag, ValType})};
    {field_access_expr, LhsExpr, Name} ->
      {Bindings, LhsType} = check_expr(Core, Bindings, LhsExpr),
      {FieldType, FieldBound} = suber_typechecker_core:var(Core),
      Bound = suber_typechecker_core:obj_use(Core, {Name, FieldBound}),
      suber_typechecker_core:flow(Core, LhsType, Bound),
      {Bindings, FieldType};
    {func_def_expr, ArgName, BodyExpr} ->
      {ArgType, ArgBound} = suber_typechecker_core:var(Core),
      {Bindings1, BodyType} = suber_bindings:in_child_scope(Bindings, fun (BindingsAcc) ->
        BindingsAcc2 = suber_bindings:insert(BindingsAcc, ArgName, ArgType),
        check_expr(Core, BindingsAcc2, BodyExpr)
      end),
      {Bindings1, suber_typechecker_core:func(Core, ArgBound, BodyType)};
    {if_expr, CondExpr, ThenExpr, ElseExpr} ->
      {Bindings1, CondType} = check_expr(Core, Bindings, CondExpr),
      Bound = suber_typechecker_core:bool_use(Core),
      suber_typechecker_core:flow(Core, CondType, Bound),
      {Bindings2, ThenType} = check_expr(Core, Bindings1, ThenExpr),
      {Bindings3, ElseType} = check_expr(Core, Bindings2, ElseExpr),
      {Merged, MergedBound} = suber_typechecker_core:var(Core),
      suber_typechecker_core:flow(Core, ThenType, MergedBound),
      suber_typechecker_core:flow(Core, ElseType, MergedBound),
      {Bindings3, Merged};
    {let_expr, {_, Name, VarExpr}, RestExpr} ->
      {Bindings1, VarType} = check_expr(Core, Bindings, VarExpr),
      suber_bindings:in_child_scope(Bindings1, fun (BindingsAcc) ->
        BindingsAcc2 = suber_bindings:insert(BindingsAcc, Name, VarType),
        check_expr(Core, BindingsAcc2, RestExpr)
      end);
    {literal_expr, Val} ->
      case Val of
        {bool, _} -> {Bindings, suber_typechecker_core:bool(Core)}
      end;
    {match_expr, MatchExpr, Cases} ->
      {Bindings1, MatchType} = check_expr(Core, Bindings, MatchExpr),
      {ResultType, ResultBound} = suber_typechecker_core:var(Core),
      {Bindings2, CaseTypePairs} = lists:foldl(fun ({{Tag, Name}, RhsExpr}, {BindingsAcc, CaseTypePairs}) ->
        case maps:get(Tag, CaseTypePairs, nil) of
          nil ->
            {WrappedType, WrappedBound} = suber_typechecker_core:var(Core),
            CaseTypePairs1 = maps:put(Tag, WrappedBound, CaseTypePairs),
            {BindingsAcc4, RhsType} = suber_bindings:in_child_scope(BindingsAcc, fun (BindingsAcc2) ->
              BindingsAcc3 = suber_bindings:insert(BindingsAcc2, Name, WrappedType),
              check_expr(Core, BindingsAcc3, RhsExpr)
            end),
            suber_typechecker_core:flow(Core, RhsType, ResultBound),
            {BindingsAcc4, CaseTypePairs1};
          _ -> throw({repeated_match_case, Tag})
        end
      end, {Bindings1, maps:new()}, Cases),
      Bound = suber_typechecker_core:case_use(Core, CaseTypePairs),
      suber_typechecker_core:flow(Core, MatchType, Bound),
      {Bindings2, ResultType};
    {record_expr, Fields} ->
      {Bindings1, FieldTypePairs} = lists:foldl(fun ({Name, FieldExpr}, {BindingsAcc, FieldTypePairs}) ->
        case maps:get(Name, FieldTypePairs, nil) of
          nil ->
            {BindingsAcc1, Type} = check_expr(Core, BindingsAcc, FieldExpr),
            {BindingsAcc1, maps:put(Name, Type, FieldTypePairs)};
          _ -> throw({repeated_field_name, Name})
        end
      end, {Bindings, maps:new()}, Fields),
      {Bindings1, suber_typechecker_core:obj(Core, FieldTypePairs)};
    {variable_expr, Name} ->
      case suber_bindings:get(Bindings, Name) of
        nil -> throw({undefined_variable, Name});
        Type -> {Bindings, Type}
      end
  end.
