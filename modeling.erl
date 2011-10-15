-module(modeling).

-export([test/0]).

-record(token, {string, type}).

testCode() ->
  {ok, Data} = file:read_file("./test.ml"),
  binary:bin_to_list(Data).

test() ->
  lists:reverse(scan([start], false, testCode(), [])).

scan(States, MatchSucceeded, RestString, Tokens) ->
  io:format("~p~n~p~n~p~n~p~n~n", [States, MatchSucceeded, RestString, Tokens]),
  NewState = case {States, MatchSucceeded} of
    {[start|_], _} -> entity_name;
    {[entity_name|_], true} -> left_bracket;
    {[left_bracket, entity_name|_], true} -> entity_type;
    {[entity_type|_], true} -> right_bracket;
    {[right_bracket,entity_type|_], true} -> newline;
    {[newline, right_bracket, entity_type|_], _} -> attribute_name;
    {[attribute_name|_], true} -> colon;
    {[attribute_name|_], false} -> dot;
    {[colon, attribute_name|_], true} -> attribute_value;
    {[attribute_value|_], true} -> comma;
    {[comma, attribute_value|_], true} -> attribute_name;
    {[comma, attribute_value|_], false} -> dot;
    {[dot|_], _} -> success
  end,
  case NewState of
    success -> Tokens;
    _ -> eval(NewState, RestString, States, Tokens)
  end.

eval(State, String, States, Tokens) ->
  {Success, Token, RestString} = scan_type(String, State),
  case Success of
    true -> scan([State|States], true, RestString, [Token|Tokens]);
    false -> scan([State|States], false, String, Tokens)
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

member(Char, Type) -> lists:member(Char, chars_for_type(Type)).

name() -> utils:letters() ++ utils:digits() ++ utils:space().

chars_for_type(Type) ->
  case Type of  
    entity_name -> name();
    entity_type -> name();
    attribute_name -> name();
    attribute_value -> name();
    colon -> ": ";
    comma -> ",\n ";
    dot -> utils:dot();
    left_bracket -> "(";
    right_bracket -> ")";
    newline -> "\n"
  end.