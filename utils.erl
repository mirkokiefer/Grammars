-module(utils).
-export([member/2, anyMember/2]).

member(String, SubString) ->
  case string:str(String, SubString) of
    0 -> false;
    _Any -> true
  end.

anyMember(_String, []) ->
  false;

anyMember(String, [FirstSubString|Rest]) ->
  case member(String, FirstSubString) of
    true -> true;
    false -> anyMember(String, Rest)
  end.