-module(modeling).

-export([test/0, test/1, test_code/1, print/0]).

-record(token, {string, type}).
-record(state, {optional=false, states, string="", tokens=[]}).

test_code(File) ->
  {ok, Data} = file:read_file("./" ++ File ++ ".ml"),
  binary:bin_to_list(Data).

print() ->
  lists:foreach(fun(Each) -> io:format("~p~n", [Each]) end, test_code("test1")).

test() -> test("test").

test(File) ->
  {success, Tokens, _RestString} = scan(#state{states=[start], string=test_code(File)}),
  ReversedTokens = lists:reverse(Tokens),
  io:format("tokens: ~p~n", [ReversedTokens]),
  FilteredTokens = filter_tokens(ReversedTokens, [spaces, newline, comma, colon, left_bracket, right_bracket, eof]),
  io:format("tokens filtered: ~p~n", [FilteredTokens]).

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

scan(State=#state{states=[{optional, OptionalState}|Rest]}) ->
  io:format("scan optional: ~p~n", [State]),
  scan_generic(State#state{optional=true, states=[OptionalState|Rest]});
  

scan(#state{states=[{any, []}|_Rest]}) ->
  {error, "no match in any state"};

scan(State=#state{states=[{any, [FirstState|RestAny]}|Rest], string=String, tokens=Tokens}) ->
  io:format("scan any: ~p~n~p~n~p~n~n", [FirstState, String, Tokens]),
  case grammar(FirstState) of
    undefined ->
      case scan_type(String, FirstState) of
        {true, Token, RestString} -> scan(#state{states=Rest, string=RestString, tokens=[Token|Tokens]});
        {false, _, _} -> scan(State#state{states=[{any, RestAny}|Rest]})
      end;
    States ->
      case scan(State#state{states=States}) of
        {error, _} -> scan(State#state{states=[{any, RestAny}|Rest]});
        {success, NewTokens, RestString} -> scan(#state{states=Rest, string=RestString, tokens=NewTokens})
      end
  end;

scan(#state{states=[], string=String, tokens=Tokens}) ->
  {success, Tokens, String};

scan(State) ->
  scan_generic(State).

scan_generic(State=#state{states=[FirstState|Rest], string=String, tokens=Tokens}) ->
  io:format("scan: ~p~n~p~n~p~n~n", [FirstState, String, Tokens]),
  case grammar(FirstState) of
    undefined -> scan_primitive(FirstState, State#state{states=Rest});
    States -> scan_resolved_states(States, State#state{states=Rest})
  end.

scan_primitive(PrimitiveState, State=#state{optional=Optional, states=Rest, string=String, tokens=Tokens}) ->
  case scan_type(String, PrimitiveState) of
    {true, Token, RestString} -> scan(#state{states=Rest, string=RestString, tokens=[Token|Tokens]});
    {false, _, _} ->
      case Optional of
        true -> scan(State#state{states=Rest});
        false -> {error, "required state not matched"}
      end
  end.

scan_resolved_states(States, State=#state{optional=Optional, states=Rest}) ->
  case scan(State#state{states=States}) of
    {error, Message} ->
      case Optional of
        true -> scan(State#state{states=Rest});
        false -> {error, Message}
      end;
    {success, NewTokens, RestString} -> scan(#state{states=Rest, string=RestString, tokens=NewTokens})
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