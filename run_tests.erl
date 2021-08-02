-module(run_tests).

-export([main/1]).

main(_) ->
  leex:file(suber_lexer),
  yecc:file(suber_parser),
  lists:foreach(fun(Name) ->
                   {ok, _, Binary} = compile:file(Name, [binary]),
                   code:load_binary(Name, [], Binary)
                end,
                [suber_lexer, suber_parser, suber_typer, suber]),
  run_tests().

run_tests() ->
  {ok, Files} = file:list_dir("tests/success/"),
  Failed =
    lists:foldl(fun(TestFileName, ResultSoFar) ->
                   TypesState = suber_typer:new(),
                   Bindings = suber_typer:new_bindings(TypesState),
                   try suber:compile(TypesState, Bindings, "tests/success/" ++ TestFileName) of
                     ok -> ResultSoFar
                   catch
                     _:_ ->
                       io:format("~ts should compile!~n", [TestFileName]),
                       true
                   after
                     suber_typer:cleanup(TypesState)
                   end
                end,
                false,
                Files),

  {ok, Files2} = file:list_dir("tests/failure/"),
  Failed2 =
    lists:foldl(fun(TestFileName, ResultSoFar) ->
                   TypesState = suber_typer:new(),
                   Bindings = suber_typer:new_bindings(TypesState),
                   try suber:compile(TypesState, Bindings, "tests/failure/" ++ TestFileName) of
                     ok ->
                       io:format("~ts should not compile!~n", [TestFileName]),
                       true
                   catch
                     _:_ -> ResultSoFar
                   after
                     suber_typer:cleanup(TypesState)
                   end
                end,
                Failed,
                Files2),

  case Failed2 of
    true ->
      halt(1);
    false ->
      ok
  end.
