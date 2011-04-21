#!/usr/bin/env escript

main([]) ->
  code:add_path("ebin")
, test(<<"true">>, true)
, test(<<"false">>, false)
, test(<<"null">>, null)
, test(<<"123">>, 123)
, test(<<"\"hello\"">>, "hello")
, test(<<"\"\\n\"">>, "\n")
, test(<<34,226,152,131,34>>, [226,152,131])
, test(<<"\"\\u2603\"">>, [9731])
, test(<<"{}">>, {object,[]})
, test(<<"[]">>, {array,[]})
, test(<<"[1, 2, 3, null, true, \"hello\"]">>, {array,[1,2,3,null,true,"hello"]})
, test(<<"{\"hello\": \"world\"}">>, {object,[{"hello","world"}]})
  .

test(Input, Output) ->
  {Output, <<>>} = tee(parse(Input)).

tee(Term) ->
  io:format("~p~n", [Term]), Term.

parse(Input) ->
  pardec:parse(Input, json_value, pardec_rules()).

comma_sep(Rule) ->
  {optional, {cons, Rule, {'*', {unseq, [{skip, json_ws}, {skip, $,}, Rule]}}}}.

% cf. http://json.org/
% cf. http://awwx.ws/combinator/17
% cf. http://hacks.catdancer.ws/parser-combinator-approach-to-json.html
% cf. http://sigusr2.net/2011/Apr/18/parser-combinators-made-simple.html

pardec_rules() ->
  [ {json_ws, {'*', {either, ['SP', 'HTAB', 'CR', 'LF']}}}
  , {json_true, {const, "true", true}}
  , {json_false, {const, "false", false}}
  , {json_null, {const, "null", null}}
  , {json_integer, {map, {'1*', 'DIGIT'}, {erlang, list_to_integer}}}
  , {json_scalar, {either, [json_true, json_false, json_null, json_integer, json_string]}}
  , {json_value, {unseq, [{skip, json_ws}, {either, [json_scalar, json_array, json_object]}]}}
  , {json_keyvalue, {tuple, [{skip, json_ws}, json_string, {skip, $:}, json_value]}}
  , {json_array, {tag, array, {item, 2, [$[, comma_sep(json_value), json_ws, $]]}}}
  , {json_object, {tag, object, {item, 2, [${, comma_sep(json_keyvalue), json_ws, $}]}}}
  , {json_string, {item, 2, [$", {'*', {either, json_string_escape, {'UCHAR', except, {either, $\\, $"}}}}, $"]}}
  , {json_string_escape, {either, [
      {item, 2, ["\\u", {map, {re, "^[0-9A-F]{4}"}, {erlang, list_to_integer, [16]}}]}
    , {const, "\\\"", $"}
    , {const, "\\\\", $\\}
    , {const, "\\/", $/}
    , {const, "\\b", $\b}
    , {const, "\\f", $\f}
    , {const, "\\n", $\n}
    , {const, "\\r", $\r}
    , {const, "\\t", $\t}
    ]}}
  ].
