-module(suber).

-export([main/1]).

main(_) ->
  leex:file(suber_lexer),
  yecc:file(suber_parser),
  lists:foreach(fun(Name) ->
                   {ok, _, Binary} = compile:file(Name, [binary]),
                   code:load_binary(Name, [], Binary)
                end,
                [suber_lexer, suber_parser, suber_typer]),
  TypesState = suber_typer:new(),
  Bindings = suber_typer:new_bindings(TypesState),
  compile(TypesState, Bindings, "test_file.suber").

compile(TypesState, Bindings, Filename) ->
  {ok, SourceBinary} = file:read_file(Filename),
  Source = unicode:characters_to_list(SourceBinary),
  {ok, Tokens, _} = suber_lexer:string(Source),
  {ok, Ast} = suber_parser:parse(Tokens),
  _Module = suber_typer:type_top_level_items(TypesState, Bindings, Ast),
  io:format("~ts", ["no type errors found\n"]).  % generate_erlang_core
                                                 % compile_forms
                                                 % save module
