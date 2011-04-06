An [experimental] declarative parser combinator library for Erlang.

Quick start:

  $ make
  ...
  $ erl -pa ebin
  ...
  1> pardec:parse(<<"">>, {'1*', 'ALPHA'}).
  nomatch
  2> pardec:parse(<<"hello world">>, {'1*', 'ALPHA'}).
  {"hello",<<" world">>}
  3> pardec:parse(<<"hello world">>, {tuple, [word, {skip, 'SP'}, word]}, [{word, {'1*', 'ALPHA'}}]).
  {{"hello","world"},<<>>}


A parser/grammar is defined declaratively as a list of "rules", each of which
is defined in terms of the builtin operators/combinators and/or other rules.

The first argument to pardec:parse is the input, which can either be a binary
or a string/list. The second argument is the top level rule, and the third
argument is a list of additional rules.

The atom 'nomatch' will be returned if the input does not match the top level
rule, or if it matches the return value will be a {Term, Remainder} tuple.

See tests.erl for more examples.
