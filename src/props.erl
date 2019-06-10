-module(props).
-compile(export_all).

get(Key, Props, Default) ->
  case proplists:get_value(Key, Props) of
    undefined -> Default;
    Val -> Val
  end.

