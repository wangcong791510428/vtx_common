-define(dbg1(Data), log4erl:debug("*dbg ~p:~p: ~p",
                    [?MODULE, ?LINE, Data])).
-define(dbg2(Fmt,Data), log4erl:debug("*dbg ~p:~p: "++Fmt,
                    [?MODULE, ?LINE]++Data)).
%-define(dbg1(Data), ok).
%-define(dbg2(Fmt, Data), ok).


-define(
    ORELSE_TRY_THROW_FUN(Thunk, Exception), 
    fun() ->
        case Thunk of 
            true -> true;
            ok -> ok;
            {ok, Any} -> {ok, Any};
            _ -> throw(Exception)
        end
    end
).

-define(
    ANDALSO_TRY_THROW_FUN(Thunk, Exception),
    fun() ->
        case Thunk of
            true -> throw(Exception);
            ok -> throw(Exception);
            {ok, _Any} -> throw(Exception)
            Any -> Any;
        end
    end
).

-define(ORELSE_TRY_THROW(THUNK, EXCEPTION),  ?ORELSE_TRY_THROW_FUN(THUNK, EXCEPTION)()).
-define(ANDALSO_TRY_THROW(THUNK, EXCEPTION), ?ANDALSO_TRY_THROW_FUN(THUNK, EXCEPTION)()).

-define(set(Value, Default), case Value=/=undefined of true->Value; false->Default end).
