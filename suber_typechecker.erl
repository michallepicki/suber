-module(suber_typechecker).
-export([new/0, check_script/3]).

new() ->
  Core = suber_typechecker_core:new(),
  Bindings = suber_bindings:new(),
  {Core, Bindings}.

check_script(Core, Bindings, Parsed) ->
  lists:foreach(fun (ToplevelItem) ->
    ok = check_toplevel(Core, Bindings, ToplevelItem)
  end, Parsed),
  {Core, Bindings}.

check_toplevel(Core, Bindings, ToplevelItem) ->
  case ToplevelItem of
    {top_level_let_def, {var_definition, Name, VarExpr}} ->
      VarType = check_expr(Core, Bindings, VarExpr),
      ok = suber_bindings:insert(Bindings, Name, VarType);
    {top_level_let_rec_def, Defs} ->
      TempBoundsWithExprs = lists:foldl(fun ({var_definition, Name, VarExpr}, TempBoundsWithExprsAcc) ->
        {TempType, TempBound} = suber_typechecker_core:var(Core),
        ok = suber_bindings:insert(Bindings, Name, TempType),
        [{TempBound, VarExpr} | TempBoundsWithExprsAcc]
      end, [], Defs),
      lists:foreach(fun ({Bound, VarExpr}) ->
        VarType = check_expr(Core, Bindings, VarExpr),
        suber_typechecker_core:flow(Core, VarType, Bound)
      end, TempBoundsWithExprs),
      ok
  end.

check_expr(Core, Bindings, Expr) ->
  case Expr of
    {call_expr, FuncExpr, ArgExpr} ->
      FuncType = check_expr(Core, Bindings, FuncExpr),
      ArgType = check_expr(Core, Bindings, ArgExpr),
      {RetType, RetBound} = suber_typechecker_core:var(Core),
      Bound = suber_typechecker_core:func_use(Core, ArgType, RetBound),
      suber_typechecker_core:flow(Core, FuncType, Bound),
      RetType;
    {case_expr, Tag, ValExpr} ->
      ValType = check_expr(Core, Bindings, ValExpr),
      suber_typechecker_core:case_(Core, {Tag, ValType});
    {field_access_expr, LhsExpr, Name} ->
      LhsType = check_expr(Core, Bindings, LhsExpr),
      {FieldType, FieldBound} = suber_typechecker_core:var(Core),
      Bound = suber_typechecker_core:obj_use(Core, {Name, FieldBound}),
      suber_typechecker_core:flow(Core, LhsType, Bound),
      FieldType;
    {func_def_expr, ArgName, BodyExpr} ->
      {ArgType, ArgBound} = suber_typechecker_core:var(Core),
      BodyType = suber_bindings:in_child_scope(Bindings, fun () ->
        ok = suber_bindings:insert(Bindings, ArgName, ArgType),
        check_expr(Core, Bindings, BodyExpr)
      end),
      suber_typechecker_core:func(Core, ArgBound, BodyType);
    {if_expr, CondExpr, ThenExpr, ElseExpr} ->
      CondType = check_expr(Core, Bindings, CondExpr),
      Bound = suber_typechecker_core:bool_use(Core),
      suber_typechecker_core:flow(Core, CondType, Bound),
      ThenType = check_expr(Core, Bindings, ThenExpr),
      ElseType = check_expr(Core, Bindings, ElseExpr),
      {Merged, MergedBound} = suber_typechecker_core:var(Core),
      suber_typechecker_core:flow(Core, ThenType, MergedBound),
      suber_typechecker_core:flow(Core, ElseType, MergedBound),
      Merged;
    {let_expr, {_, Name, VarExpr}, RestExpr} ->
      VarType = check_expr(Core, Bindings, VarExpr),
      suber_bindings:in_child_scope(Bindings, fun () ->
        ok = suber_bindings:insert(Bindings, Name, VarType),
        check_expr(Core, Bindings, RestExpr)
      end);
    {literal_expr, Val} ->
      case Val of
        {bool, _} -> suber_typechecker_core:bool(Core)
      end;
    {match_expr, MatchExpr, Cases} ->
      MatchType = check_expr(Core, Bindings, MatchExpr),
      {ResultType, ResultBound} = suber_typechecker_core:var(Core),
      CaseTypePairs = lists:foldl(fun ({{Tag, Name}, RhsExpr}, CaseTypePairs) ->
        case maps:get(Tag, CaseTypePairs, nil) of
          nil ->
            {WrappedType, WrappedBound} = suber_typechecker_core:var(Core),
            CaseTypePairs1 = maps:put(Tag, WrappedBound, CaseTypePairs),
            RhsType = suber_bindings:in_child_scope(Bindings, fun () ->
              ok = suber_bindings:insert(Bindings, Name, WrappedType),
              check_expr(Core, Bindings, RhsExpr)
            end),
            suber_typechecker_core:flow(Core, RhsType, ResultBound),
            CaseTypePairs1;
          _ -> throw({repeated_match_case, Tag})
        end
      end,  maps:new(), Cases),
      Bound = suber_typechecker_core:case_use(Core, CaseTypePairs),
      suber_typechecker_core:flow(Core, MatchType, Bound),
      ResultType;
    {record_expr, Fields} ->
      FieldTypePairs = lists:foldl(fun ({Name, FieldExpr}, FieldTypePairs) ->
        case maps:get(Name, FieldTypePairs, nil) of
          nil ->
            Type = check_expr(Core, Bindings, FieldExpr),
            maps:put(Name, Type, FieldTypePairs);
          _ -> throw({repeated_field_name, Name})
        end
      end, maps:new(), Fields),
      suber_typechecker_core:obj(Core, FieldTypePairs);
    {variable_expr, Name} ->
      case suber_bindings:get(Bindings, Name) of
        nil -> throw({undefined_variable, Name});
        Type -> Type
      end
  end.
