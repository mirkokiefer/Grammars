-module(json_grammar).
-export([grammar/1, primitive_chars/1]).

grammar(State) ->
  case State of
    start -> [object];
    object -> [object_start, {optional, whitespace}, {optional, properties},
      {optional, whitespace}, object_end];
    properties -> [property, {optional, next_property}];
    next_property -> [comma, {optional, whitespace}, properties];
    property -> [key, colon, value];
    key -> [{optional, whitespace}, double_quotes, key_name, double_quotes];
    value -> [{any, [string_value, object, array]}];
    string_value -> [double_quotes, string_literal, double_quotes];
    array -> [array_start, {optional, array_values}, array_end];
    array_values -> [array_value, {optional, next_array_value}];
    next_array_value -> [comma, {optional, whitespace}, array_values];
    array_value -> [{any, [string_value, object, array]}];
    stop -> [{optional, eof}];
    _ -> undefined
  end.

primitive_chars(Type) ->
  case Type of  
    key_name -> name();
    string_literal -> name();
    object_start -> "{";
    object_end -> "}";
    array_start -> "[";
    array_end -> "]";
    spaces -> " ";
    eof -> "\n ";
    colon -> ":";
    comma -> ",";
    double_quotes -> "\"";
    dot -> utils:dot();
    left_bracket -> "(";
    right_bracket -> ")";
    newline -> "\n";
    whitespace -> "\n "
  end.

name() -> utils:letters() ++ utils:digits() ++ utils:space().