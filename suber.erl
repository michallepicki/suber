-module(suber).
-export([main/1, compile/3]).

main(_) ->
  leex:file(suber_lexer),
  yecc:file(suber_parser),
  lists:foreach(fun (Name) ->
    {ok, _, Binary} = compile:file(Name, [binary]),
    code:load_binary(Name, [], Binary)
  end, [suber_lexer, suber_parser, suber_bindings, suber_reachability, suber_typechecker_core, suber_typechecker]),
  {Core, Bindings} = suber_typechecker:new(),
  compile(Core, Bindings, "test_file.suber").

compile(Core, Bindings, Filename) ->
  {ok, SourceBinary} = file:read_file(Filename),
  Source = unicode:characters_to_list(SourceBinary),
  {ok, Tokens, _} = suber_lexer:string(Source),
  {ok, Ast} = suber_parser:parse(Tokens),
  suber_typechecker:check_script(Core, Bindings, Ast).
  % generate_erlang_core
  % compile_forms
  % save module
