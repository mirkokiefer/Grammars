-module(modeling).

-export([test/0]).

-record(token, {string, type}).

testCode() ->
  {ok, Data} = file:read_file("./test.ml"),
  SourceString = binary:bin_to_list(Data).

test() ->
  scan(testCode()).

scan(String) ->
  lists:reverse(scan_entity_name(String, "", [])).

scan_entity_name([First|Rest], TryMatch, Tokens) ->
  case [First] of
    "(" -> scan_entity_type(Rest, "", [entity_name(lists:reverse(TryMatch)) | Tokens]);
    Any -> scan_entity_name(Rest, [First|TryMatch], Tokens)
  end.

scan_entity_type([First|Rest], TryMatch, Tokens) ->
  case [First] of
    ")" -> scan_entity_attribute_name(Rest, "", [entity_type(lists:reverse(TryMatch)) | Tokens]);
    Any -> scan_entity_type(Rest, [First|TryMatch], Tokens)
  end.

scan_entity_attribute_name([First|Rest], TryMatch, Tokens) ->
  case [First] of
    ":" -> scan_entity_attribute_value(Rest, "", [entity_attribute_name(lists:reverse(TryMatch)) | Tokens]);
    "." -> Tokens;
    Any -> scan_entity_attribute_name(Rest, [First|TryMatch], Tokens)
  end.

scan_entity_attribute_value([First|Rest], TryMatch, Tokens) ->
  case [First] of
    "," -> scan_entity_attribute_name(Rest, "", [entity_attribute_value(lists:reverse(TryMatch)) | Tokens]);
    "." -> Tokens;
    Any -> scan_entity_attribute_value(Rest, [First|TryMatch], Tokens)
  end.

entity_name(String) ->
  #token{string=String, type="EntityName"}.

entity_type(String) ->
  #token{string=String, type="EntityType"}.

entity_attribute_name(String) ->
  #token{string=String, type="AttributeName"}.

entity_attribute_value(String) ->
  #token{string=String, type="AttributeValue"}.

remove_ignorable_chars(String) ->
  .

ignorableChars() -> ["\n", " "].