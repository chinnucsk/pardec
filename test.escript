#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin"),
  {ok, Data} = file:consult("tests.erl"),
  test(Data, undefined, 0).

test([], _, N) ->
  io:format("~p tests passed~n", [N]);
test([{rule, Rule} | Data], _, N) ->
  test(Data, Rule, N);
test([{test, Input, Expect} | Data], Rule, N) ->
  case pardec:parse(Input, Rule) of
    Expect ->
      test(Data, Rule, N + 1);
    Output ->
      io:format("pardec:parse(~p, ~p)~n", [Input, Rule]),
      io:format("output: ~p~n", [Output]),
      io:format("expect: ~p~n", [Expect])
  end.
