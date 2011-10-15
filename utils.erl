-module(utils).
-export([string_member/2, any_string_member/2, any_character_member/2, character_to_type/1,
  digits/0, letters/0, space/0, dot/0, comma/0]).

string_member(String, SubString) ->
  case string:str(String, SubString) of
    0 -> false;
    _Any -> true
  end.

any_string_member(_String, []) ->
  false;

any_string_member(String, [FirstSubString|Rest]) ->
  case string_member(String, FirstSubString) of
    true -> true;
    false -> any_string_member(String, Rest)
  end.

any_character_member(_String, []) ->
  false;

any_character_member(String, [FirstCharacter|Rest]) ->
  case lists:member(FirstCharacter, String) of
    true -> true;
    false -> any_character_member(String, Rest)
  end.

character_to_type(Character) ->
  Groups = [
    {space, " "},
    {dot, "."},
    {colon, ":"},
    {equal, "="},
    {singleQuotes, "'"},
    {doubleQuotes, "\""},
    {roundBracketLeft, "("},
    {roundBracketRight, ")"},
    {squareBracketLeft, "["},
    {squareBracketRight, "]"},
    {curlyBracketLeft, "{"},
    {curlyBracketRight, "}"},
    {digit, digits()},
    {letter, letters()},
    {ascii, "!#$%&*+,-/;<>?@\^_`|~"}],
  [{Type, _}] = lists:filter(fun ({_, String}) -> lists:member(Character, String) end, Groups),
  Type.

digits() -> "0123456789".
letters() -> "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".
space() -> " ".
dot() -> ".".
comma() -> ",".