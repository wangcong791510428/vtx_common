-module(util).
-compile(export_all).

-define(IN(X,Min,Max), X >= Min, X =< Max).

%%----------------------------------------------------------------------
%% @doc return the hex representation of a byte
byte_to_hex_string(X) ->
    [nibble_to_hex_char(X bsr 4),nibble_to_hex_char(X band 15)].

%%----------------------------------------------------------------------
%% @doc Convert an integer in 0..15 to a hex character

nibble_to_hex_char(X) when X < 10 -> $0 + X;
nibble_to_hex_char(X) -> $A + X - 10.

%%----------------------------------------------------------------------
%% @doc Convert a hex string to a binary.

hex2bin(Str) ->
    L = hex2list(Str),
    list_to_binary(L).
%%----------------------------------------------------------------------
%% @doc Convert a hex string to a list of bytes.

hex2list([H1,H2|T]) ->
    I = hex_nibble2int(H1) * 16 + hex_nibble2int(H2),
    [I|hex2list(T)];
hex2list([]) ->
    [].

%%----------------------------------------------------------------------
%% @doc Convert a hex nibble chanrcater to an integer.

hex_nibble2int(X) when ?IN(X, $0, $9) -> X - $0;
hex_nibble2int(X) when ?IN(X, $a, $f) -> X - $a + 10;
hex_nibble2int(X) when ?IN(X, $A, $F) -> X - $A + 10.

bin2hex(B) ->
    L = binary_to_list(B),
    LH0 = lists:map(fun(X)->erlang:integer_to_list(X,16) end, L),
    LH = lists:map(fun([X,Y])->[X,Y];([X])->[$0,X] end, LH0), % add zeros
    lists:flatten(LH).

%% url decode
url_decode([$%, Hi, Lo | _Tail]) ->
    _Hex = erlang:list_to_integer([Hi, Lo], 16).
% given a pathname "../foo/bar/" it gives back the fully qualified
% absolute pathname.
abs_pathname(" " ++ Filename) ->
    % strip leading whitspace
    abs_pathname(Filename);
abs_pathname([$/ |_]=Filename) ->
    Filename;
abs_pathname(Filename) ->
    {ok, Cwd} = file:get_cwd(),
    {Filename2, Args} = separate_cmd_args(Filename, ""),
    abs_pathname(Filename2, Cwd) ++ Args.

abs_pathname(Filename, Dir) ->
    Name = filename:absname(Filename, Dir ++ "/"),
    OutFilename = filename:join(fix_path_list(filename:split(Name), [])),
    % If the filename is a dir (last char slash, put back end slash
    case string:right(Filename,1) of
    "/" ->
        OutFilename ++ "/";
    "\\" ->
        OutFilename ++ "/";
    _Else->
        OutFilename
    end.

% if this as an executable with arguments, seperate out the arguments
% ""./foo\ bar.sh -baz=blah" -> {"./foo\ bar.sh", " -baz=blah"}
separate_cmd_args("", CmdAcc) ->
    {lists:reverse(CmdAcc), ""};
separate_cmd_args("\\ " ++ Rest, CmdAcc) -> % handle skipped value
    separate_cmd_args(Rest, " \\" ++ CmdAcc);
separate_cmd_args(" " ++ Rest, CmdAcc) ->
    {lists:reverse(CmdAcc), " " ++ Rest};
separate_cmd_args([Char|Rest], CmdAcc) ->
    separate_cmd_args(Rest, [Char | CmdAcc]).


% lowercases string bytes that are the ascii characters A-Z.
% All other characters/bytes are ignored.
ascii_lower(String) ->
    ascii_lower(String, []).

ascii_lower([], Acc) ->
    lists:reverse(Acc);
ascii_lower([Char | RestString], Acc) when Char >= $A, Char =< $B ->
    ascii_lower(RestString, [Char + ($a-$A) | Acc]);
ascii_lower([Char | RestString], Acc) ->
    ascii_lower(RestString, [Char | Acc]).

% Is a character whitespace?
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.

% removes leading and trailing whitespace from a string
trim(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).

% takes a heirarchical list of dirs and removes the dots ".", double dots
% ".." and the corresponding parent dirs.
fix_path_list([], Acc) ->
    lists:reverse(Acc);
fix_path_list([".."|Rest], [_PrevAcc|RestAcc]) ->
    fix_path_list(Rest, RestAcc);
fix_path_list(["."|Rest], Acc) ->
    fix_path_list(Rest, Acc);
fix_path_list([Dir | Rest], Acc) ->
    fix_path_list(Rest, [Dir | Acc]).

implode(List, Sep) ->
    implode(List, Sep, []).

implode([], _Sep, Acc) ->
    lists:flatten(lists:reverse(Acc));
implode([H], Sep, Acc) ->
    implode([], Sep, [H|Acc]);
implode([H|T], Sep, Acc) ->
    implode(T, Sep, [Sep,H|Acc]).
 
uuid() ->
    uuid(32).
uuid(16) ->
    T = term_to_binary({make_ref(), now()}),
    <<I:160/integer>> = crypto:sha(T),
    string:to_lower(lists:flatten( io_lib:format("~16..0s", [erlang:integer_to_list(I, 16)]) ));
uuid(32) ->
    T = term_to_binary({make_ref(), now()}),
    <<I:160/integer>> = crypto:sha(T),
    string:to_lower(lists:flatten( io_lib:format("~32..0s", [erlang:integer_to_list(I, 16)]) )).

md5_to_hex(Md5) ->
    <<I:128/integer>> = Md5,
    string:to_lower(lists:flatten( io_lib:format("~32..0s", [erlang:integer_to_list(I, 16)]) )).

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

max(L, R) when L >= R ->
    L;
max(L, R) ->
    R.

min(L, R) when L =< R ->
    L;
min(L, R) ->
    R.

qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).
