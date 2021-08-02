%
-module(suber).

-export([main/1, compile/3]).

main(Args) ->
  Filename =
    case Args of
      [Name] ->
        Name;
      _ ->
        "test_file.suber"
    end,
  leex:file(suber_lexer),
  yecc:file(suber_parser),
  lists:foreach(fun(Name) ->
                   {ok, _, Binary} = compile:file(Name, [binary]),
                   code:load_binary(Name, [], Binary)
                end,
                [suber_lexer, suber_parser, suber_typer]),
  TypesState = suber_typer:new(),
  Bindings = suber_typer:new_bindings(TypesState),
  io:format("~0tp~n", [compile(TypesState, Bindings, Filename)]).

compile(TypesState, Bindings, Filename) ->
  {ok, SourceBinary} = file:read_file(Filename),
  Source = unicode:characters_to_list(SourceBinary),
  {ok, Tokens, _} = suber_lexer:string(Source),
  {ok, Ast} = suber_parser:parse(Tokens),
  _Module = suber_typer:type_top_level_items(TypesState, Bindings, Ast),
  ok.  % generate_erlang_core
       % compile_forms
       % save module
