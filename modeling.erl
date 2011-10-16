-module(modeling).

-export([test/0, test/1, test_code/1, print/0]).

-record(token, {string, type}).
-record(state, {optional=false, states, string, tokens}).

test_code(File) ->
  {ok, Data} = file:read_file("./" ++ File ++ ".ml"),
  binary:bin_to_list(Data).

print() ->
  lists:foreach(fun(Each) -> io:format("~p~n", [Each]) end, test_code("test1")).

test() -> test("test").

test(File) ->
  {success, Tokens, _RestString} = scan([start], test_code(File), []),
  FilteredTokens = filter_tokens(Tokens, [spaces, newline, comma, colon, left_bracket, right_bracket, eof]),
  io:format("tokens: ~p~n", [lists:reverse(FilteredTokens)]).

filter_tokens(Tokens, FilterTypes) ->
  [Token || Token=#token{type=Type} <- Tokens, lists:member(Type, FilterTypes) == false].

grammar(State) ->
  case State of
    start -> [entities];
    entities ->[entity, {any, [next_entity, stop]}];
    next_entity -> [newline, entities];
    entity -> [entity_name, left_bracket, entity_type, right_bracket, {any, [dot, attributes]}];
    attributes -> [newline, attribute];
    attribute -> [{optional, spaces}, attribute_name, colon,
      {optional, spaces}, attribute_value, {any, [next_attribute, dot]}];
    next_attribute -> [comma, newline, attribute];
    stop -> [{optional, eof}];
    _ -> undefined
  end.

scan([{optional, OptionalState}|Rest], String, Tokens) ->
  io:format("scan optional: ~p~n~p~n~p~n~n", [OptionalState, String, Tokens]),
  case grammar(OptionalState) of
    undefined ->
      case scan_type(String, OptionalState) of
        {true, Token, RestString} -> scan(Rest, RestString, [Token|Tokens]);
        {false, _, _} -> scan(Rest, String, Tokens)
      end;
    States ->
      case scan(States, String, Tokens) of
        {error, _} -> scan(Rest, String, Tokens);
        {success, NewTokens, RestString} -> scan(Rest, RestString, NewTokens)
      end
  end;
  

scan([{any, []}|_Rest], _String, _Tokens) ->
  {error, "no match in any state"};

scan([{any, [FirstState|RestAny]}|Rest], String, Tokens) ->
  io:format("scan any: ~p~n~p~n~p~n~n", [FirstState, String, Tokens]),
  case grammar(FirstState) of
    undefined ->
      case scan_type(String, FirstState) of
        {true, Token, RestString} -> scan(Rest, RestString, [Token|Tokens]);
        {false, _, _} -> scan([{any, RestAny}|Rest], String, Tokens)
      end;
    States ->
      case scan(States, String, Tokens) of
        {error, _} -> scan([{any, RestAny}|Rest], String, Tokens);
        {success, NewTokens, RestString} -> scan(Rest, RestString, NewTokens)
      end
  end;

scan([], String, Tokens) ->
  {success, Tokens, String};

scan([FirstState|Rest], String, Tokens) ->
  io:format("scan: ~p~n~p~n~p~n~n", [FirstState, String, Tokens]),
  case grammar(FirstState) of
    undefined ->
      case scan_type(String, FirstState) of
        {true, Token, RestString} -> scan(Rest, RestString, [Token|Tokens]);
        {false, _, _} -> {error, "required state not matched"}
      end;
    States ->
      case scan(States, String, Tokens) of
        {error, Message} -> {error, Message};
        {no_match, String, Tokens} -> scan(Rest, String, Tokens);
        {success, NewTokens, RestString} -> scan(Rest, RestString, NewTokens)
      end
  end.

scan_type(String, Type) -> scan_type(String, "", Type).

scan_type([], Match, Type) ->
  type_matched([], Match, Type);

scan_type(String=[First|Rest], Match, Type) ->
  case member(First, Type) of
    true -> scan_type(Rest, [First|Match], Type);
    false -> type_matched(String, Match, Type)
  end.

type_matched(String, Match, Type) ->
  {string:len(Match)>0, token(lists:reverse(Match), Type), String}.

token(String, Type) ->
  #token{string=String, type=Type}.

member(Char, Type) -> lists:member(Char, primitive_chars(Type)).

name() -> utils:letters() ++ utils:digits() ++ utils:space().

primitive_chars(Type) ->
  case Type of  
    entity_name -> name();
    entity_type -> name();
    attribute_name -> name();
    attribute_value -> name();
    spaces -> " ";
    eof -> "\n ";
    colon -> ":";
    comma -> ",";
    dot -> utils:dot();
    left_bracket -> "(";
    right_bracket -> ")";
    newline -> "\n"
  end.