{rule, $a}.
{test, "", nomatch}.
{test, "123", nomatch}.
{test, "ABC", nomatch}.
{test, "abc", {$a, "bc"}}.

{rule, 'DIGIT'}.
{test, "", nomatch}.
{test, "123", {$1, "23"}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.

{rule, 'ALPHA'}.
{test, "", nomatch}.
{test, "123", nomatch}.
{test, "ABC", {$A, "BC"}}.
{test, "abc", {$a, "bc"}}.

{rule, ['DIGIT', 'DIGIT']}.
{test, "", nomatch}.
{test, "123", {"12", "3"}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.

{rule, {char, {16#20, 16#7E}}}.
{test, "", nomatch}.
{test, "123", {$1, "23"}}.
{test, "ABC", {$A, "BC"}}.
{test, "abc", {$a, "bc"}}.
{test, "\t", nomatch}.
{test, [127], nomatch}.

{rule, {tuple, ['DIGIT', 'DIGIT']}}.
{test, "", nomatch}.
{test, "123", {{$1, $2}, "3"}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.

{rule, {'*', 'DIGIT'}}.
{test, "", {[], []}}.
{test, "123", {"123", []}}.
{test, "ABC", {[], "ABC"}}.
{test, "abc", {[], "abc"}}.

{rule, {'1*', 'DIGIT'}}.
{test, "", nomatch}.
{test, "123", {"123", []}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.

{rule, {either, 'ALPHA', 'DIGIT'}}.
{test, "", nomatch}.
{test, "123", {$1, "23"}}.
{test, "ABC", {$A, "BC"}}.
{test, "abc", {$a, "bc"}}.

{rule, {const, "true", true}}.
{test, "", nomatch}.
{test, "true", {true, []}}.
{test, "TRUE", nomatch}.
{test, "false", nomatch}.

{rule, {map, {'1*', 'DIGIT'}, {erlang, list_to_integer}}}.
{test, "", nomatch}.
{test, "123", {123, []}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.

{rule, {skip, 'SP'}}.
{test, "", nomatch}.
{test, "  123", {" 123"}}.

{rule, {re, "\\d{4}"}}.
{test, "", nomatch}.
{test, "123", nomatch}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.
{test, "2011", {"2011", []}}.

{rule, {cons, {'*', 'DIGIT'}, {'*', {unseq, [{skip, 'SP'}, {'*', 'DIGIT'}]}}}}.
{test, "123 456 789", {["123", "456", "789"], []}}.

{rule, {tag, number, {'1*', 'DIGIT'}}}.
{test, "", nomatch}.
{test, "123", {{number, "123"}, []}}.
{test, "ABC", nomatch}.
{test, "abc", nomatch}.
