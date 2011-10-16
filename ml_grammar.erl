-module(ml_grammar).
-export([grammar/1, primitive_rules/1]).

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

primitive_rules(Type) ->
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

name() -> utils:letters() ++ utils:digits() ++ utils:space().