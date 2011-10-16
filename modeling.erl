-module(modeling).

-export([test_ml/0, test_json/0, test_code/1, print/0]).

-record(token, {string, type}).

test_code(File) ->
  {ok, Data} = file:read_file("./" ++ File),
  binary:bin_to_list(Data).

print() ->
  lists:foreach(fun(Each) -> io:format("~p~n", [Each]) end, test_code("test1")).

grammar(Rule) -> ml_grammar:grammar(Rule).
primitive_rules(Type) -> ml_grammar:primitive_rules(Type).

test_json() ->
  {success, Tokens, _RestString} = scan(start, string=test_code("test.json")),
  ReversedTokens = lists:reverse(Tokens),
  io:format("tokens: ~p~n", [ReversedTokens]),
  FilteredTokens = filter_tokens(ReversedTokens, [whitespace, comma, colon, eof, double_quotes]),
  io:format("tokens filtered: ~p~n", [FilteredTokens]).

test_ml() ->
  {success, Tokens, _RestString} = scan(start, test_code("test.ml")),
  io:format("tokens: ~p~n", [Tokens]),
  FilteredTokens = filter_tokens(Tokens, [spaces, newline, comma, colon, left_bracket, right_bracket, eof]),
  io:format("tokens filtered: ~p~n", [FilteredTokens]).

filter_tokens(Tokens, FilterTypes) ->
  [Token || Token=#token{type=Type} <- Tokens, lists:member(Type, FilterTypes) == false].

scan(StartRule, String) ->
  case scan([StartRule], String, []) of
    {success, RestString, Tokens} -> {success, lists:reverse(Tokens), RestString};
    Error -> Error
  end.

scan([], String, Tokens) ->
  {success, String, Tokens};

scan([{any, []}|_Rest], _String, _Tokens) ->
  {error, "none matched in 'any' rule"};

scan([{any,[First|RestAny]}|Rest], String, Tokens) ->
  case scan([First], String, Tokens) of
    {success, RestString, NewTokens} -> scan(Rest, RestString, NewTokens);
    {error, _} -> scan([{any, RestAny}|Rest], String, Tokens)
  end;

scan([{optional, Rule}|Rest], String, Tokens) ->
  case scan_rule(Rule, String) of
    {success, RestString, NewTokens} -> scan(Rest, RestString, NewTokens++Tokens);
    {error, _} -> scan(Rest, String, Tokens)
  end;

scan([RequiredRule|Rest], String, Tokens) ->
  case scan_rule(RequiredRule, String) of
    {success, RestString, NewTokens} -> scan(Rest, RestString, NewTokens++Tokens);
    {error, Error} -> {error, Error}
  end.

scan_rule(Rule, String) ->
  case grammar(Rule) of
    undefined -> scan_primitive(Rule, String);
    ResolvedRules -> scan(ResolvedRules, String, [])
  end.

scan_primitive(Rule, String) -> scan_primitive(String, "", Rule).

scan_primitive([], Match, Type) ->
  type_matched([], Match, Type);

scan_primitive(String=[First|Rest], Match, Type) ->
  case member(First, Type) of
    true -> scan_primitive(Rest, [First|Match], Type);
    false -> type_matched(String, Match, Type)
  end.

type_matched(String, Match, Type) ->
  case string:len(Match)>0 of
    true -> {success, String, [token(lists:reverse(Match), Type)]};
    false -> {error, "primitive scan failed"}
  end.

token(String, Type) ->
  #token{string=String, type=Type}.

member(Char, Type) -> lists:member(Char, primitive_rules(Type)).