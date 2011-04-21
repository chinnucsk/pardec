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
  match_unicode(Input);
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
  case parse(Input, Rule, Rules) of {N, R} -> blist_split(N, R); _ -> nomatch end;
parse(Input, {skip, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {_, R} -> {R}; Match -> Match end;
parse(Input, {const, Rule, Value}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {R} -> {Value, R}; {_, R} -> {Value, R} end;
parse(Input, {unseq, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {[Value], R} -> {Value, R}; Match -> Match end;
parse(Input, {tuple, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {Match, R} -> {list_to_tuple(Match), R} end;
parse(Input, {item, N, Rule}, Rules) ->
  case parse(Input, Rule, Rules) of nomatch -> nomatch; {Match, R} -> {lists:nth(N, Match), R} end;
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
parse(Input, {'*', Rule, until, Lookahead}, Rules) ->
  parse_many(Input, Rule, Rules, Lookahead, []);
parse(Input, {'1*', Rule}, Rules) ->
  case parse(Input, Rule, Rules) of
    nomatch ->
      nomatch;
    {Match, Remainder} ->
      {Matched, Remainder2} = parse_many(Remainder, Rule, Rules, []),
      {[Match | Matched], Remainder2}
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

parse_many(Input, Rule, Rules, Lookahead, Acc) ->
  case parse(Input, Lookahead, Rules) of
    nomatch ->
      case parse(Input, Rule, Rules) of
        nomatch ->
          {lists:reverse(Acc), Input};
        {Match, Remainder} ->
          parse_many(Remainder, Rule, Rules, Lookahead, [Match | Acc])
      end;
    _Match ->
      {lists:reverse(Acc), Input}
  end.

match_unicode([]) ->
  nomatch;
match_unicode(<<>>) ->
  nomatch;
match_unicode([Char | Input]) ->
  match_unicode(Char, Input);
match_unicode(<<Char, Input/bytes>>) ->
  match_unicode(Char, Input).

match_unicode(Char, Input) ->
  case xmerl_ucs:is_unicode(Char) of
    true ->
      {Char, Input};
    false ->
      nomatch
  end.

match_regexp(Input, Pattern) ->
  case re:run(Input, Pattern, [anchored, {newline, anycrlf}]) of
    {match, [{0, N} | _]} ->
      blist_split(N, Input);
    nomatch ->
      nomatch
  end.

match_regexp(Input, Pattern, Capture) ->
  case re:run(Input, Pattern, [anchored, {newline, anycrlf}]) of
    {match, [{0, MatchLength} | Captures]} ->
      {blist_slice(Input, lists:nth(Capture, Captures)), blist_nthtail(MatchLength, Input)};
    nomatch ->
      nomatch
  end.

match(Input, {Min, Max}) ->
  match_char_range(Input, Min, Max);
match(Input, Char) when is_integer(Char) ->
  match_char(Input, Char);
match(Input, Rules) when is_list(Rules) ->
  match_any(Rules, Input).

match_char([Char | Input], Char) when is_integer(Char) ->
  {Char, Input};
match_char(<<Char, Input/bytes>>, Char) when is_integer(Char) ->
  {Char, Input};
match_char(_Input, Char) when is_integer(Char) ->
  nomatch.

match_char_range([Char | Input], Min, Max) when Char >= Min andalso Char =< Max ->
  {Char, Input};
match_char_range(<<Char, Input/bytes>>, Min, Max) when Char >= Min andalso Char =< Max ->
  {Char, Input};
match_char_range(_Input, _Min, _Max) ->
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

blist_slice(Input, PosLen) when is_binary(Input) ->
  binary_to_list(binary:part(Input, PosLen));
blist_slice(Input, {Pos, Len}) when is_list(Input) ->
  lists:sublist(Input, Pos + 1, Len).

blist_nthtail(N, Input) when is_binary(Input) ->
  element(2, split_binary(Input, N));
blist_nthtail(N, Input) when is_list(Input) ->
  lists:nthtail(N, Input).

blist_split(N, Input) when is_binary(Input) ->
  {H, T} = split_binary(Input, N),
  {binary_to_list(H), T};
blist_split(N, Input) when is_list(Input) ->
  lists:split(N, Input).
