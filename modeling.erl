-module(modeling).

-export([test/0]).

-record(token, {string, type}).

testCode() ->
  {ok, Data} = file:read_file("./test.ml"),
  SourceString = binary:bin_to_list(Data).

test() ->
  lists:reverse(scan(start, false, testCode(), [])).

scan(LastState, MatchSucceeded, RestString, Tokens) ->
  case {LastState, MatchSucceeded} of
    {start, _} -> scan_entity_name(RestString, "", Tokens);
    {entity_name, true} -> scan_entity_type(RestString, "", Tokens);
    {entity_type, true} -> scan_attribute_name(RestString, "", Tokens);
    {attribute_name, true} -> scan_attribute_value(RestString, "", Tokens);
    {attribute_name, false} -> Tokens;
    {attribute_value, true} -> scan_attribute_name(RestString, "", Tokens)
  end.

scan_entity_name([First|Rest], TryMatch, Tokens) ->
  case [First] of
    "(" -> scan(entity_name, true, Rest, [entity_name(lists:reverse(TryMatch)) | Tokens]);
    Any -> scan_entity_name(Rest, [First|TryMatch], Tokens)
  end.

scan_entity_type([First|Rest], TryMatch, Tokens) ->
  case [First] of
    ")" -> scan(entity_type, true, Rest, [entity_type(lists:reverse(TryMatch)) | Tokens]);
    Any -> scan_entity_type(Rest, [First|TryMatch], Tokens)
  end.

scan_attribute_name([First|Rest], TryMatch, Tokens) ->
  case [First] of
    ":" -> scan(attribute_name, true, Rest, [attribute_name(lists:reverse(TryMatch)) | Tokens]);
    "." -> scan(attribute_name, false, Rest, Tokens);
    Any -> scan_attribute_name(Rest, [First|TryMatch], Tokens)
  end.

scan_attribute_value([First|Rest], TryMatch, Tokens) ->
  case [First] of
    "," -> scan(attribute_value, true, Rest, [attribute_value(lists:reverse(TryMatch)) | Tokens]);
    "." -> scan(attribute_value, true, [First|Rest], [attribute_value(lists:reverse(TryMatch)) | Tokens]);
    Any -> scan_attribute_value(Rest, [First|TryMatch], Tokens)
  end.

entity_name(String) ->
  #token{string=String, type="EntityName"}.

entity_type(String) ->
  #token{string=String, type="EntityType"}.

attribute_name(String) ->
  #token{string=String, type="AttributeName"}.

attribute_value(String) ->
  #token{string=String, type="AttributeValue"}.

ignorableChars() -> ["\n", " "].