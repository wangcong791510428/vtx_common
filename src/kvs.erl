-module(kvs).
-compile(export_all).

new() ->
  dict:new().

from_list(Props) ->
  Dict = dict:from_list(Props).

size(Kvs) ->
  dict:size(Kvs).

store({Key, Val}, Kvs) ->
  store([{Key, Val}], Kvs);

store(Props, Kvs) when is_list(Props)->
  Nulls = [<<>>, undefined],

  Props1 = lists:map(fun canonicalize/1, Props),
  Props2 = [{Key, Val} || {Key, Val}<-Props, not lists:member(Val, Nulls)], 
  Dict = dict:from_list(Props2),
  
  Fun = fun(Key, Value1, Value2) -> Value1 end,
  dict:merge(Fun, Dict, Kvs).

get(Key, Kvs) ->
  get(Key, Kvs, undefined).

get(Key, Kvs, Default) ->
  Nulls = [<<>>, undefined], 
  case dict:find(Key, Kvs) of
    {ok, Val} -> 
      case lists:member(Val, Nulls) of
        false -> {ok, Val};
        true  -> {ok, Default}
      end;
    error -> 
      {ok, Default}
  end.
  
to_list(Kvs) ->
  dict:to_list(Kvs).
  
%% internal

canonicalize({Key, Val}) when is_atom(Key) ->
  Key1 = atom_to_list(Key),
  Key2 = string:to_lower(Key1),
  {Key2, Val}; 
canonicalize({Key, Val}) when is_list(Key) ->
  Key1 = string:to_lower(Key),
  {Key1, Val};
canonicalize({Key, Val}) when is_binary(Key) ->
  Key1 = binary_to_list(Key),
  Key2 = string:to_lower(Key1),
  {Key2, Val};
canonicalize(_) ->
  {undefined, undefined}.
