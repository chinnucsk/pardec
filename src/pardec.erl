-module(pardec).

-export([parse/2, parse/3]).

parse(Input, Rule) ->
  parse(Input, Rule, []).

parse(Input, 'SP', _Rules) ->
  match(Input, 32);
parse(Input, 'HTAB', _Rules) ->
  match(Input, $\t);
parse(Input, 'CR', _Rules) ->
  match(Input, $\r);
parse(Input, 'LF', _Rules) ->
  match(Input, $\n);
parse(Input, 'DQUOTE', _Rules) ->
  match(Input, $");
parse(Input, 'ALPHA', _Rules) ->
  match(Input, [{16#41, 16#5A}, {16#61, 16#7A}]);
parse(Input, 'CTL', _Rules) ->
  match(Input, [{0, 31}, 127]);
parse(Input, 'DIGIT', _Rules) ->
  match(Input, {$0, $9});
parse(Input, 'HEXDIGIT', _Rules) ->
  match(Input, [{$0, $9}, {$A, $F}]);
parse(Input, 'CHAR', _Rules) ->
  match(Input, {16#01, 16#7F});
parse(Input, 'UCHAR', _Rules) ->
  match_uchar(Input);
parse(Input, Rule, _Rules) when is_integer(Rule) ->
  match(Input, Rule);
parse(Input, Key, Rules) when is_atom(Key) ->
  case proplists:lookup(Key, Rules) of
    {Key, Rule} ->
      parse(Input, Rule, Rules);
    none ->
      throw({rule_undefined, Key})
  end;
parse(Input, {char, Range={Min, Max}}, _Rules) when is_integer(Min) andalso is_integer(Max) andalso Max > Min ->
  match(Input, Range);
parse(Input, {re, Pattern}, _Rules) ->
  match_regexp(Input, Pattern);
parse(Input, {re, Pattern, capture, N}, _Rules) ->
  match_regexp(Input, Pattern, N);
parse(Input, {split, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of {N, R} when length(R) >= N -> lists:split(N, R); _ -> nomatch end;
parse(Input, {skip, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {_, R} -> {R}; Match -> Match end;
parse(Input, {const, Rule, Value}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {R} -> {Value, R}; {_, R} -> {Value, R} end;
parse(Input, {unseq, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {[Value], R} -> {Value, R}; Match -> Match end;
parse(Input, {tuple, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {Match, R} -> {list_to_tuple(Match), R} end;
parse(Input, {tag, Atom, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {Match, R} -> {{Atom, Match}, R} end;
parse(Input, {optional, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> {Input}; Match -> Match end;
parse(Input, {Rule, except, Exceptions}, Rules) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      nomatch;
    Match ->
      case parse(Input, Exceptions, Rules) of
        nomatch ->
          Match;
        _ ->
          nomatch
      end
  end;
parse(Input, {map, Rule, {M, F}}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {R} -> {R}; {Match, R} -> {apply(M, F, [Match]), R} end;
parse(Input, {map, Rule, {M, F, A}}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {R} -> {R}; {Match, R} -> {apply(M, F, [Match | A]), R} end;
parse(Input, Sequence, Rules) when is_list(Sequence) ->
  parse_sequence(Input, Sequence, Rules, []);
parse(Input, {cons, A, B}, Rules) ->
  parse_cons(Input, A, B, Rules);
parse(Input, {either, A, B}, Rules) ->
  parse_either(Input, [A, B], Rules);
parse(Input, {either, Alternatives}, Rules) ->
  parse_either(Input, Alternatives, Rules);
parse([], {'*', _Rule}, _Rules) ->
  {[], []};
parse(Input, {'*', Rule}, Rules) ->
  parse_many(Input, Rule, Rules, []);
parse(Input, {'1*', Rule}, Rules) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      nomatch;
    {Match, Remainder} ->
      {Matched, Remainder2} = parse_many(Remainder, Rule, Rules, []),
      {[Match | Matched], Remainder2}
  end.

match_regexp(Input, Pattern) ->
  case re:run(Input, Pattern, [anchored, {newline, anycrlf}]) of
    {match, [{0, N} | _]} ->
      lists:split(N, Input);
    nomatch ->
      nomatch
  end.

match_regexp(Input, Pattern, Capture) ->
  case re:run(Input, Pattern, [anchored, {newline, anycrlf}]) of
    {match, [{0, MatchLength} | Captures]} ->
      {Start, Length} = lists:nth(Capture, Captures),
      {lists:sublist(Input, Start + 1, Length), lists:nthtail(MatchLength, Input)};
    nomatch ->
      nomatch
  end.

parse_sequence(Input, [], _Rules, Acc) ->
  {lists:reverse(Acc), Input};
parse_sequence(Input, [Rule | Sequence], Rules, Acc) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      nomatch;
    {Match, Remainder} ->
      parse_sequence(Remainder, Sequence, Rules, [Match | Acc]);
    {Remainder} ->
      parse_sequence(Remainder, Sequence, Rules, Acc)
  end.

parse_cons(Input, A, B, Rules) ->
  case parse(Input, A, Rules) of
    nomatch ->
      nomatch;
    {MA, RA} ->
      case parse(RA, B, Rules) of
        nomatch ->
          nomatch;
        {MB, RB} ->
          {[MA | MB], RB}
      end
  end.

parse_either(_Input, [], _Rules) ->
  nomatch;
parse_either(Input, [Rule | Alternatives], Rules) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      parse_either(Input, Alternatives, Rules);
    Match ->
      Match
  end.

parse_many(Input, Rule, Rules, Acc) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      {lists:reverse(Acc), Input};
    {Match, Remainder} ->
      parse_many(Remainder, Rule, Rules, [Match | Acc])
  end.

match_uchar([]) ->
  nomatch;
match_uchar([Ch | Input]) ->
  case xmerl_ucs:is_unicode(Ch) of
    true ->
      {Ch, Input};
    false ->
      nomatch
  end.

match(Input, Rules) when is_list(Rules) ->
  match_any(Rules, Input);
match([Ch | Input], Ch) when is_integer(Ch) ->
  {Ch, Input};
match(_Input, Ch) when is_integer(Ch) ->
  nomatch;
match([Ch | Input], {Min, Max}) ->
  case Ch >= Min andalso Ch =< Max of
    true ->
      {Ch, Input};
    false ->
      nomatch
  end;
match(_Input, {_Min, _Max}) ->
  nomatch.

match_any([], _Input) ->
  nomatch;
match_any([Rule | Rules], Input) ->
  case match(Input, Rule) of
    nomatch ->
      match_any(Rules, Input);
    Match ->
      Match
  end.
