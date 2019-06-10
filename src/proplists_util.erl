-module(proplists_util).
-compile(export_all).

get_value(Key, Props, Default) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default; 
    Val -> Val
  end.
