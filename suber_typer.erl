-module(suber_typer).
-export([infer_types/1]).

instantiate(Typer, Ts, Lvl) ->
  case Ts of
    {polymorphic_type, Level, Body} ->
      Freshened = ets:new(suber_freshened, [set, private]),
      Ret = freshen_above(Typer, Level, Body, Lvl, Freshened),
      ets:delete(Freshened),
      Ret;
    _ -> Ts
  end.

freshen_above(Typer, Limit, Ty, Lvl, Freshened) ->
  case level(Typer, Ty) =< Limit of
    true -> Ty;
    false ->
      case Ty of
        {variable, Uid, OldLevel} ->
          case ets:lookup(Freshened, Uid) of
            [{_, FreshenedVariable}] -> FreshenedVariable;
            [] ->
              {variable, V, Lvl} = fresh_var(Typer, Lvl),
              ets:insert(Freshened, {Uid, {variable, V, Lvl}}),
              {Uid, OldLevel, LowerBounds, UpperBounds} = get_var(Typer, Uid),
              NewLowerBounds = lists:reverse(lists:map(fun (T) -> freshen_above(Typer, Limit, T, Lvl, Freshened) end, lists:reverse(LowerBounds))),
              NewUpperBounds = lists:reverse(lists:map(fun (T) -> freshen_above(Typer, Limit, T, Lvl, Freshened) end, lists:reverse(UpperBounds))),
              {_NextIdRef, TypesTable} = Typer,
              ets:insert(TypesTable, {V, Lvl, NewLowerBounds, NewUpperBounds}),
              {variable, V, Lvl}
          end;
        {function, L, R} -> 
          {
            function,
            freshen_above(Typer, Limit, L, Lvl, Freshened),
            freshen_above(Typer, Limit, R, Lvl, Freshened)
          };
        {record, Fields} -> 
          {
            record,
            lists:map(fun ({Name, FieldType}) -> {Name, freshen_above(Typer, Limit, FieldType, Lvl, Freshened)} end, Fields)
          };
        {primitive, _} -> Ty
      end
  end.

fresh_var(Typer, Lvl) ->
  {NextIdRef, TypesTable} = Typer,
  Uid = counters:get(NextIdRef, 1),
  counters:add(NextIdRef, 1, 1),
  ets:insert(TypesTable, {Uid, Lvl, [], []}),
  {variable, Uid, Lvl}.

get_var(Typer, Uid) ->
  {_NextIdRef, TypesTable} = Typer,
  [Var] = ets:lookup(TypesTable, Uid),
  Var.

level(Typer, Ts) ->
  case Ts of
    {polymorphic_type, Level, _Body} -> Level;
    {variable, _Uid, Level} -> Level;
    {function, Lhs, Rhs} -> max(level(Typer, Lhs), level(Typer, Rhs));
    {record, []} -> 0;
    {record, Fields} -> lists:max(lists:map(fun ({_Name, FieldType}) -> level(Typer, FieldType) end, Fields));
    {primitive, _Name} -> 0
  end.

infer_types(Pgrm) ->
  NextIdRef = counters:new(1, []),
  TypesTable = ets:new(suber_typer, [set, private]),
  Typer = {NextIdRef, TypesTable},
  Ctx = #{},
  BoolType = {primitive, "bool"},
  IntType = {primitive, "int"},
  Ctx2 = maps:put("true", BoolType, Ctx),
  Ctx3 = maps:put("false", BoolType, Ctx2),
  Ctx4 = maps:put("not", {function, BoolType, BoolType}, Ctx3),
  Ctx5 = maps:put("succ", {function, IntType, IntType}, Ctx4),
  Ctx6 = maps:put("add", {function, IntType, {function, IntType, IntType}}, Ctx5),
  V = fresh_var(Typer, 1),
  Ctx7 = maps:put("if", {polymorphic_type, 0, {function, BoolType, {function, V, {function, V, V}}}}, Ctx6),
  infer_types(Typer, Pgrm, Ctx7).

infer_types(Typer, Pgrm, Ctx) ->
  case Pgrm of
    [] -> Ctx;
    [{Isrec, Nme, Rhs} | Rest] ->
      TySch = type_let_rhs(Typer, Isrec, Nme, Rhs, Ctx, 0),
      Ctx2 = maps:put(Nme, TySch, Ctx),
      infer_types(Typer, Rest, Ctx2)
  end.

type_let_rhs(Typer, Isrec, Nme, Rhs, Ctx, Lvl) ->
  Res = case Isrec of
    false -> type_expr(Typer, Rhs, Ctx, Lvl + 1);
    true ->
      ETy = fresh_var(Typer, Lvl + 1),
      Ty = type_expr(Typer, Rhs, maps:put(Nme, ETy, Ctx), Lvl + 1),
      Cache = ets:new(suber_cache, [set, private]),
      constrain(Typer, Ty, ETy, Cache),
      ets:delete(Cache),
      ETy
    end,
  {polymorphic_type, Lvl, Res}.

type_expr(Typer, Expr, Ctx, Lvl) ->
  case Expr of
    {variable_expr, Name} -> instantiate(Typer, maps:get(Name, Ctx), Lvl);
    {fun_def_expr, Name, Body} ->
      Param = fresh_var(Typer, Lvl),
      BodyTy = type_expr(Typer, Body, maps:put(Name, Param, Ctx), Lvl),
      {function, Param, BodyTy};
    {call_expr, F, A} ->
      FTy = type_expr(Typer, F, Ctx, Lvl),
      ATy = type_expr(Typer, A, Ctx, Lvl),
      Res = fresh_var(Typer, Lvl),
      Cache = ets:new(suber_cache, [set, private]),
      constrain(Typer, FTy, {function, ATy, Res}, Cache),
      ets:delete(Cache),
      Res;
    {literal_int_expr, _N} -> {primitive, "int"};
    {field_access_expr, Obj, Name} ->
      ObjTy = type_expr(Typer, Obj, Ctx, Lvl),
      Res = fresh_var(Typer, Lvl),
      Cache = ets:new(suber_cache, [set, private]),
      constrain(Typer, ObjTy, {record, [{Name, Res}]}, Cache),
      ets:delete(Cache),
      Res;
    {record_expr, Fields} ->
      {record, lists:map(fun ({Name, B}) -> {Name, type_expr(Typer, B, Ctx, Lvl)} end, Fields)};
    {let_expr, Isrec, Nme, Rhs, Bod} ->
      NTy = type_let_rhs(Typer, Isrec, Nme, Rhs, Ctx, Lvl),
      type_expr(Typer, Bod, maps:put(Nme, NTy, Ctx), Lvl)
  end.

constrain(Typer, Lhs, Rhs, Cache) ->
  case Lhs of
    Rhs -> ok;
    Lhs ->
      AlreadyPerformed =
        case {Lhs, Rhs} of
          {{variable, _SomeLhsUid, _SomeLhsLevel}, _Rhs} ->
            case ets:lookup(Cache, {Lhs, Rhs}) of
              [_Found] -> true;
              [] ->
                ets:insert(Cache, {{Lhs, Rhs}}),
                false
            end;
          {_Lhs, {variable, _SomeRhsUid, _SomeRhsLevel}} ->
            case ets:lookup(Cache, {Lhs, Rhs}) of
              [_Found] -> true;
              [] ->
                ets:insert(Cache, {{Lhs, Rhs}}),
                false
            end;
          _ -> false
        end,
      case AlreadyPerformed of
        true -> ok;
        false ->
          case {Lhs, level(Typer, Lhs), Rhs, level(Typer, Rhs)} of
            {{function, L0, R0}, _LhsLevel, {function, L1, R1}, _RhsLevel} ->
              constrain(Typer, L1, L0, Cache),
              constrain(Typer, R0, R1, Cache);
            {{record, Fs0}, _LhsLevel, {record, Fs1}, _RhsLevel} ->
              lists:foreach(fun ({N1, T1}) ->
                case lists:keyfind(N1, 1, Fs0) of
                  false -> throw({missing_obj_field, N1, Fs0});
                  {_N0, T0} -> constrain(Typer, T0, T1, Cache)
                end end, Fs1);
            {{variable, LhsUid, LhsLevel}, LhsLevel, Rhs, RhsLevel} when RhsLevel =< LhsLevel ->
              {LhsUid, LhsLevel, LowerBounds, UpperBounds} = get_var(Typer, LhsUid),
              NewUpperBounds = [Rhs | UpperBounds],
              {_NextIdRef, TypesTable} = Typer,
              ets:insert(TypesTable, {LhsUid, LhsLevel, LowerBounds, NewUpperBounds}),
              lists:foreach(fun (T) -> constrain(Typer, T, Rhs, Cache) end, LowerBounds);
            {Lhs, LhsLevel, {variable, RhsUid, RhsLevel}, RhsLevel} when LhsLevel =< RhsLevel ->
              {RhsUid, RhsLevel, LowerBounds, UpperBounds} = get_var(Typer, RhsUid),
              NewLowerBounds = [Lhs | LowerBounds],
              {_NextIdRef, TypesTable} = Typer,
              ets:insert(TypesTable, {RhsUid, RhsLevel, NewLowerBounds, UpperBounds}),
              lists:foreach(fun (T) -> constrain(Typer, Lhs, T, Cache) end, UpperBounds);
            {{variable, _LhsUid, LhsLevel}, LhsLevel, Rhs, _RhsLevel} ->
              ExtrudeCache = ets:new(suber_extrude_cache, [set, private]),
              Rhs0 = extrude(Typer, Rhs, LhsLevel, ExtrudeCache),
              ets:delete(ExtrudeCache),
              constrain(Typer, Lhs, Rhs0, Cache);
            {Lhs, _LhsLevel, {variable, _RhsUid, RhsLevel}, RhsLevel} ->
              ExtrudeCache = ets:new(suber_extrude_cache, [set, private]),
              Lhs0 = extrude(Typer, Lhs, RhsLevel, ExtrudeCache),
              ets:delete(ExtrudeCache),
              constrain(Typer, Lhs0, Rhs, Cache);
            _ -> throw({cannot_constrain, Lhs, Rhs})
          end
      end
  end.

extrude(Typer, Ty, Lvl, ExtrudeCache) ->
  case level(Typer, Ty) =< Lvl of
    true -> Ty;
    false ->
      case Ty of
        {function, L, R} -> {function, extrude(Typer, L, Lvl, ExtrudeCache), extrude(Typer, R, Lvl, ExtrudeCache)};
        {record, Fs} -> {record, lists:map(fun ({Name, FTy}) -> {Name, extrude(Typer, FTy, Lvl, ExtrudeCache)} end, Fs)};
        {primitive, _N} -> Ty;
        {variable, Uid, OldLevel} ->
          case ets:lookup(ExtrudeCache, Uid) of
            [{Uid, Extruded}] -> Extruded;
            [] ->
              {variable, Nvs, Lvl} = fresh_var(Typer, Lvl),
              ets:insert(ExtrudeCache, {Uid, {variable, Nvs, Lvl}}),
              {Uid, OldLevel, LowerBounds, UpperBounds} = get_var(Typer, Uid),
              NewLowerBounds = lists:map(fun (T) -> extrude(Typer, T, Lvl, ExtrudeCache) end, LowerBounds),
              NewUpperBounds = lists:map(fun (T) -> extrude(Typer, T, Lvl, ExtrudeCache) end, UpperBounds),
              {_NextIdRef, TypesTable} = Typer,
              ets:insert(TypesTable, {Nvs, Lvl, NewLowerBounds, NewUpperBounds}),
              ets:insert(TypesTable, {Uid, OldLevel, NewLowerBounds ++ LowerBounds, NewUpperBounds ++ UpperBounds}),
              {variable, Nvs, Lvl}
          end
      end
  end.
