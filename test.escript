#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin"),
  {ok, Data} = file:consult("tests.erl"),
  test(Data, undefined).

test([], _) ->
  ok;
test([{rule, Rule} | Data], _) ->
  test(Data, Rule);
test([{test, Input, Expect} | Data], Rule) ->
  case pardec:parse(Input, Rule) of
    Expect ->
      test(Data, Rule);
    Output ->
      io:format("pardec:parse(~p, ~p)~n", [Input, Rule]),
      io:format("output: ~p~n", [Output]),
      io:format("expect: ~p~n", [Expect])
  end.
