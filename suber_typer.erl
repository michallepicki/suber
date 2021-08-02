-module(suber_typer).

-export([new/0, new_bindings/1, type_top_level_items/3]).

new() ->
  NextIDRef = counters:new(1, []),
  TypesTable = ets:new(suber_types_table, [set, private]),
  {NextIDRef, TypesTable}.

new_bindings(TypesState) ->
  Bindings = #{},
  BoolType = {primitive, "bool"},
  IntType = {primitive, "int"},
  Bindings2 = maps:put("true", BoolType, Bindings),
  Bindings3 = maps:put("false", BoolType, Bindings2),
  Bindings4 = maps:put("not", {function, BoolType, BoolType}, Bindings3),
  Bindings5 = maps:put("succ", {function, IntType, IntType}, Bindings4),
  Bindings6 = maps:put("add", {function, IntType, {function, IntType, IntType}}, Bindings5),
  V = new_type_variable(TypesState, 1),
  Bindings7 =
    maps:put("if",
             {polymorphic_type, 0, {function, BoolType, {function, V, {function, V, V}}}},
             Bindings6),
  Bindings7.

type_top_level_items(TypesState, Bindings, Ast) ->
  case Ast of
    [] ->
      Bindings;
    [{top_level_let_expr, IsRec, Name, Expr} | Rest] ->
      Type = type_let_expr(TypesState, Bindings, IsRec, Name, Expr, 0),
      Bindings2 = maps:put(Name, Type, Bindings),
      type_top_level_items(TypesState, Bindings2, Rest)
  end.

type_let_expr(TypesState, Bindings, IsRec, Name, Expr, Level) ->
  Type =
    case IsRec of
      false ->
        type_expr(TypesState, Bindings, Expr, Level + 1);
      true ->
        OuterType = new_type_variable(TypesState, Level + 1),
        InnerType = type_expr(TypesState, maps:put(Name, OuterType, Bindings), Expr, Level + 1),
        Cache = ets:new(suber_cache, [set, private]),
        constrain(TypesState, InnerType, OuterType, Cache),
        ets:delete(Cache),
        OuterType
    end,
  {polymorphic_type, Level, Type}.

type_expr(TypesState, Bindings, Expr, Level) ->
  case Expr of
    {variable_expr, Name} ->
      instantiate(TypesState, maps:get(Name, Bindings), Level);
    {fun_def_expr, Name, Body} ->
      ArgType = new_type_variable(TypesState, Level),
      BodyType = type_expr(TypesState, maps:put(Name, ArgType, Bindings), Body, Level),
      {function, ArgType, BodyType};
    {call_expr, Fun, Arg} ->
      FunType = type_expr(TypesState, Bindings, Fun, Level),
      ArgType = type_expr(TypesState, Bindings, Arg, Level),
      BodyType = new_type_variable(TypesState, Level),
      Cache = ets:new(suber_cache, [set, private]),
      constrain(TypesState, FunType, {function, ArgType, BodyType}, Cache),
      ets:delete(Cache),
      BodyType;
    {literal_int_expr, _} ->
      {primitive, "int"};
    {field_access_expr, Obj, Name} ->
      ObjType = type_expr(TypesState, Bindings, Obj, Level),
      Res = new_type_variable(TypesState, Level),
      Cache = ets:new(suber_cache, [set, private]),
      constrain(TypesState, ObjType, {record, [{Name, Res}]}, Cache),
      ets:delete(Cache),
      Res;
    {record_expr, Fields} ->
      {record,
       lists:map(fun({Name, FieldType}) ->
                    {Name, type_expr(TypesState, Bindings, FieldType, Level)}
                 end,
                 Fields)};
    {let_expr, IsRec, Name, LetExpr, Body} ->
      ExprType = type_let_expr(TypesState, Bindings, IsRec, Name, LetExpr, Level),
      type_expr(TypesState, maps:put(Name, ExprType, Bindings), Body, Level)
  end.

instantiate(TypesState, Type, AtLevel) ->
  case Type of
    {polymorphic_type, Level, Body} ->
      LevelLimit = Level,
      freshen_above(TypesState, LevelLimit, Body, AtLevel);
    _ ->
      Type
  end.

freshen_above(TypesState, LevelLimit, Type, Level) ->
  Freshened = ets:new(suber_freshened, [set, private]),
  Ret = do_freshen_above(TypesState, LevelLimit, Type, Level, Freshened),
  ets:delete(Freshened),
  Ret.

do_freshen_above(TypesState, LevelLimit, Type, Level, Freshened) ->
  case level(TypesState, Type) =< LevelLimit of
    true ->
      Type;
    false ->
      case Type of
        {variable, ID, _} ->
          case ets:lookup(Freshened, ID) of
            [{_, FreshenedVariable}] ->
              FreshenedVariable;
            [] ->
              {variable, NewID, _} = new_type_variable(TypesState, Level),
              ets:insert(Freshened, {ID, {variable, NewID, Level}}),
              {_, _, LowerBounds, UpperBounds} = get_type_variable(TypesState, ID),
              NewLowerBounds =
                lists:reverse(
                  lists:map(fun(BoundType) ->
                               do_freshen_above(TypesState, LevelLimit, BoundType, Level, Freshened)
                            end,
                            lists:reverse(LowerBounds))),
              NewUpperBounds =
                lists:reverse(
                  lists:map(fun(BoundType) ->
                               do_freshen_above(TypesState, LevelLimit, BoundType, Level, Freshened)
                            end,
                            lists:reverse(UpperBounds))),
              put_type_variable(TypesState, NewID, Level, NewLowerBounds, NewUpperBounds)
          end;
        {function, ArgType, BodyType} ->
          {function,
           do_freshen_above(TypesState, LevelLimit, ArgType, Level, Freshened),
           do_freshen_above(TypesState, LevelLimit, BodyType, Level, Freshened)};
        {record, Fields} ->
          {record,
           lists:map(fun({Name, FieldType}) ->
                        {Name,
                         do_freshen_above(TypesState, LevelLimit, FieldType, Level, Freshened)}
                     end,
                     Fields)};
        {primitive, _} ->
          Type
      end
  end.

new_type_variable(TypesState, Level) ->
  {NextIDRef, _} = TypesState,
  ID = counters:get(NextIDRef, 1),
  counters:add(NextIDRef, 1, 1),
  put_type_variable(TypesState, ID, Level, [], []).

put_type_variable(TypesState, ID, Level, LowerBounds, UpperBounds) ->
  {_, TypesTable} = TypesState,
  ets:insert(TypesTable, {ID, Level, LowerBounds, UpperBounds}),
  {variable, ID, Level}.

get_type_variable(TypesState, ID) ->
  {_, TypesTable} = TypesState,
  [VarWithBounds] = ets:lookup(TypesTable, ID),
  VarWithBounds.

level(TypesState, TypeScheme) ->
  case TypeScheme of
    {polymorphic_type, Level, _} ->
      Level;
    {variable, _, Level} ->
      Level;
    {function, ArgType, BodyType} ->
      max(level(TypesState, ArgType), level(TypesState, BodyType));
    {record, []} ->
      0;
    {record, Fields} ->
      lists:max(
        lists:map(fun({_, FieldType}) -> level(TypesState, FieldType) end, Fields));
    {primitive, _} ->
      0
  end.

constrain(TypesState, Type0, Type1, Cache) ->
  case Type0 == Type1 of
    true ->
      ok;
    false ->
      EitherIsVariable =
        case {Type0, Type1} of
          {{variable, _, _}, _} ->
            true;
          {_, {variable, _, _}} ->
            true;
          _ ->
            false
        end,
      AlreadyPerformed =
        case EitherIsVariable of
          true ->
            case ets:lookup(Cache, {Type0, Type1}) of
              [_Found] ->
                true;
              [] ->
                ets:insert(Cache, {{Type0, Type1}}),
                false
            end;
          _ ->
            false
        end,
      case AlreadyPerformed of
        true ->
          ok;
        false ->
          case {Type0, level(TypesState, Type0), Type1, level(TypesState, Type1)} of
            {{function, ArgType0, BodyType0}, _, {function, ArgType1, BodyType1}, _} ->
              constrain(TypesState, ArgType1, ArgType0, Cache),
              constrain(TypesState, BodyType0, BodyType1, Cache);
            {{record, Fields0}, _, {record, Fields1}, _} ->
              lists:foreach(fun({Name1, FieldType1}) ->
                               case lists:keyfind(Name1, 1, Fields0) of
                                 false -> throw({missing_obj_field, Name1, Fields0});
                                 {_Name0, FieldType0} ->
                                   constrain(TypesState, FieldType0, FieldType1, Cache)
                               end
                            end,
                            Fields1);
            {{variable, Type0ID, _}, Type0Level, _, Type1Level} when Type1Level =< Type0Level ->
              {_, _, LowerBounds, UpperBounds} = get_type_variable(TypesState, Type0ID),
              NewUpperBounds = [Type1 | UpperBounds],
              put_type_variable(TypesState, Type0ID, Type0Level, LowerBounds, NewUpperBounds),
              lists:foreach(fun(BoundType) -> constrain(TypesState, BoundType, Type1, Cache) end,
                            LowerBounds);
            {_, Type0Level, {variable, Type1ID, _}, Type1Level} when Type0Level =< Type1Level ->
              {_, _, LowerBounds, UpperBounds} = get_type_variable(TypesState, Type1ID),
              NewLowerBounds = [Type0 | LowerBounds],
              put_type_variable(TypesState, Type1ID, Type1Level, NewLowerBounds, UpperBounds),
              lists:foreach(fun(BoundType) -> constrain(TypesState, Type0, BoundType, Cache) end,
                            UpperBounds);
            {{variable, _, _}, Type0Level, _, _} ->
              NewType1 = extrude(TypesState, Type1, Type0Level),
              constrain(TypesState, Type0, NewType1, Cache);
            {_, _, {variable, _, _}, Type1Level} ->
              NewType0 = extrude(TypesState, Type0, Type1Level),
              constrain(TypesState, NewType0, Type1, Cache);
            _ ->
              throw({cannot_constrain, Type0, Type1})
          end
      end
  end.

extrude(TypesState, Type, Level) ->
  ExtrudeCache = ets:new(suber_extrude_cache, [set, private]),
  Ret = do_extrude(TypesState, Type, Level, ExtrudeCache),
  ets:delete(ExtrudeCache),
  Ret.

do_extrude(TypesState, Type, Level, ExtrudeCache) ->
  case level(TypesState, Type) =< Level of
    true ->
      Type;
    false ->
      case Type of
        {function, ArgType, BodyType} ->
          {function,
           do_extrude(TypesState, ArgType, Level, ExtrudeCache),
           do_extrude(TypesState, BodyType, Level, ExtrudeCache)};
        {record, Fields} ->
          {record,
           lists:map(fun({Name, FieldType}) ->
                        {Name, do_extrude(TypesState, FieldType, Level, ExtrudeCache)}
                     end,
                     Fields)};
        {primitive, _} ->
          Type;
        {variable, ID, OldLevel} ->
          case ets:lookup(ExtrudeCache, ID) of
            [{_, Extruded}] ->
              Extruded;
            [] ->
              {variable, NewID, _} = new_type_variable(TypesState, Level),
              ets:insert(ExtrudeCache, {ID, {variable, NewID, Level}}),
              {_, _, LowerBounds, UpperBounds} = get_type_variable(TypesState, ID),
              NewLowerBounds =
                lists:map(fun(BoundType) -> do_extrude(TypesState, BoundType, Level, ExtrudeCache)
                          end,
                          LowerBounds),
              NewUpperBounds =
                lists:map(fun(BoundType) -> do_extrude(TypesState, BoundType, Level, ExtrudeCache)
                          end,
                          UpperBounds),
              UpdatedLowerBounds = NewLowerBounds ++ LowerBounds,
              UpdatedUpperBounds = NewUpperBounds ++ UpperBounds,
              put_type_variable(TypesState, ID, OldLevel, UpdatedLowerBounds, UpdatedUpperBounds),
              put_type_variable(TypesState, NewID, Level, NewLowerBounds, NewUpperBounds)
          end
      end
  end.
