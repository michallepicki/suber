
-module(suber).
-export([main/1]).

main(_) ->
  leex:file(suber_lexer),
  yecc:file(suber_parser),
  lists:foreach(fun (Name) ->
    {ok, _, Binary} = compile:file(Name, [binary]),
    code:load_binary(Name, [], Binary)
  end,
  [suber_lexer, suber_parser, suber_typer]),
  NextIdRef = counters:new(1, []),
  TypesTable = ets:new(suber_typer, [set, private]),
  Typer = {NextIdRef, TypesTable},
  Ctx = suber_typer:new_ctx(Typer),
  compile(Typer, "test_file.suber", Ctx).

compile(Typer, Filename, Ctx) ->
  {ok, SourceBinary} = file:read_file(Filename),
  Source = unicode:characters_to_list(SourceBinary),
  {ok, Tokens, _} = suber_lexer:string(Source),
  {ok, Ast} = suber_parser:parse(Tokens),
  _NewCtx = suber_typer:infer_types(Typer, Ast, Ctx),
  io:format("~ts", ["Success!\n"]).
  % generate_erlang_core
  % compile_forms
  % save module
