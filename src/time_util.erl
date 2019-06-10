-module(time_util).
-compile(export_all).

get_time_schema(JoinedT0, JoinedTf, TimeResolution) ->
    get_time_schema(JoinedT0, JoinedTf, TimeResolution, []).
get_time_schema(JoinedT0, JoinedTf, TimeResolution, Acc) ->
    case JoinedT0 > JoinedTf of
        true -> 
            lists:reverse(Acc);
        false ->
            JoinedTnext = next_timepoint(JoinedT0, TimeResolution),
            get_time_schema(JoinedTnext, JoinedTf, TimeResolution, [JoinedT0 | Acc])
    end.
  
%%%%
%% joined time: 2011_05_24_14_00_00 or 2011_05_24_00_00_00
%% return time: 2011_05_24_15_00_00 or 2011_05_25_00_00_00
next_timepoint(JoinedTime, TimeResolution) ->
    TimeStep = case TimeResolution of
	      "hour" -> 3600;
	      "day" -> 86400
    end,

    List = string:tokens(JoinedTime, "_"),
    [Year, Month, Day, Hour, Minute, Second] = lists:map(
	      fun(String) -> list_to_integer(String) end,
	      List
    ),
    DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    DateTimeSeconds = epoch(datetime, DateTime),

    {{Year1, Month1, Day1},{Hour1, Minute1, Second1}} = epoch_to_datetime(DateTimeSeconds+TimeStep),
    List1 = lists:map(
	      fun(Integer) -> 
	        case Integer < 10 of
		          true -> "0"++integer_to_list(Integer);
		          false ->integer_to_list(Integer)
	        end
	      end,
	      [Year1, Month1, Day1, Hour1, Minute1, Second1]
    ),
    string:join(List1, "_").
    

%%%%
%% compact  time: 20110524140330
%% standard time: 2011-05-24T14:03:30+08:00
%% joined   time: 2011_05_24_14_00_00 or 2011_05_24_00_00_00
%% mark     time: 2011-05-24 14:03:30
compact_to_standard(CompactTime) ->
    <<
      YearBin:4/binary,
      MonthBin:2/binary,
      DayBin:2/binary,
      HourBin:2/binary,
      MinuteBin:2/binary,
      SecondBin:2/binary
    >> = list_to_binary(CompactTime),

    [Year, Month, Day, Hour, Minute, Second] = lists:map(
        fun binary_to_list/1,
        [YearBin, MonthBin, DayBin, HourBin, MinuteBin, SecondBin]
    ),
    Date = string:join([Year, Month, Day], "-"),
    Time = string:join([Hour, Minute, Second], ":"),
    %TimeZone = "+08:00",
    TimeZone = "+0800",

    lists:flatten( io_lib:format("~sT~s~s", [Date,Time,TimeZone])).

standard_to_compact(StandardTime) ->
    Pattern = pattern:get(standard_time),
    {match,[Year, Month, Day, Hour, Minute, Second, TimeZone]} = re:run(StandardTime,
        Pattern,
        [{capture, ['Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'TimeZone'],list}]),

    lists:flatten([Year, Month, Day, Hour, Minute, Second]).

joined_to_compact(JoinedTime) ->
    L = string:tokens(JoinedTime, "_"),
    lists:flatten(L).

compact_to_joined(CompactTime, Resolution) ->
    <<
      YearBin:4/binary,
      MonthBin:2/binary,
      DayBin:2/binary,
      HourBin:2/binary,
      _/binary
    >> = list_to_binary(CompactTime),

    [Year, Month, Day, Hour] = lists:map(
        fun binary_to_list/1,
        [YearBin, MonthBin, DayBin, HourBin]
    ),
    Minute = "00",
    Second = "00",
    case Resolution of
        "hour" ->
            string:join([Year, Month, Day, Hour, Minute, Second], "_");
        "day" ->
            string:join([Year, Month, Day, "00", Minute, Second], "_")
    end.

joined_to_standard(JoinedTime) ->
    %TimeZone = "+08:00",
    TimeZone = "+0800",
    [Year, Month, Day, Hour, Minute, Second] = string:tokens(JoinedTime, "_"),
    Date = string:join([Year, Month, Day], "-"),
    Time = string:join([Hour, Minute, Second], ":"),

    lists:flatten( io_lib:format("~sT~s~s", [Date,Time,TimeZone])).

standard_to_joined(StandardTime, Resolution) ->
    Pattern = pattern:get(standard_time),
    {match,[Year, Month, Day, Hour, _Minute, _Second, TimeZone]} = re:run(StandardTime,
        Pattern,
        [{capture, ['Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'TimeZone'],list}]),
    Minute = "00",
    Second = "00",

    case Resolution of
        "hour" ->
            string:join([Year, Month, Day, Hour, Minute, Second], "_");
        "day" ->
            string:join([Year, Month, Day, "00", Minute, Second], "_")
    end.

mark_to_standard(Mark) ->
    %TimeZone = "+08:00",
    TimeZone = "+0800",
    Pattern = pattern:get(mark_time),
    {match, [Date, Time]} = re:run(Mark, Pattern, [{capture, ['Date', 'Time'],list}]),
    lists:flatten(io_lib:format("~sT~s~s", [Date,Time,TimeZone])).

standard_to_mark(StandardTime) ->
    Pattern = pattern:get(standard_time),
    {match,[Year, Month, Day, _Hour, _Minute, _Second, _TimeZone]} = re:run(StandardTime,
        Pattern,
        [{capture, ['Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'TimeZone'],list}]),

    Date = string:join([Year, Month, Day], "-"),
    Date++" 00:00:00".    

epoch() ->
    (calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time( now()))-719528*24*3600).

epoch(now, Now) ->
    (calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time( Now) ) -719528*24*3600);

epoch(datetime, DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) -719528*24*3600).

epoch_to_datetime(Epoch) ->
    Seconds = Epoch + 719528*24*3600,
    calendar:gregorian_seconds_to_datetime(Seconds).


%%%
%%% Create W3CDTF (http://www.w3.org/TR/NOTE-datetime) formatted date
%%% w3cdtf(GregSecs) -> "YYYY-MM-DDThh:mm:ssTZD"
%%%
w3cdtf({_Mega, _Sec, _Micro}=TimeStamp) ->
    DateTime = calendar:now_to_datetime(TimeStamp),
    GregSecs = calendar:datetime_to_gregorian_seconds(DateTime),
    w3cdtf(GregSecs);

w3cdtf(GregSecs) ->    
    Date = calendar:gregorian_seconds_to_datetime(GregSecs),
    {{Y, Mo, D},{H, Mi, S}} = Date,
    [UDate|_] = calendar:local_time_to_universal_time_dst(Date),
    {DiffD,{DiffH,DiffMi,_}}=calendar:time_difference(UDate,Date),
    w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi).

%%%  w3cdtf's helper function
w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, DiffMi) when DiffH < 12,  DiffH /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "+" ++ add_zero(DiffH) ++ ":"  ++
        add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi /= 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(23-DiffH) ++
        ":" ++ add_zero(60-DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, DiffD, DiffH, DiffMi) when DiffH > 12,  DiffD /= 0, DiffMi == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "-" ++ add_zero(24-DiffH) ++
        ":" ++ add_zero(DiffMi);

w3cdtf_diff(Y, Mo, D, H, Mi, S, _DiffD, DiffH, _DiffMi) when DiffH == 0 ->
    i2l(Y) ++ "-" ++ add_zero(Mo) ++ "-" ++ add_zero(D) ++ "T" ++
        add_zero(H) ++ ":" ++ add_zero(Mi) ++ ":"  ++
        add_zero(S) ++ "Z".

add_zero(I) when is_integer(I) -> add_zero(i2l(I));
add_zero([A])               -> [$0,A];
add_zero(L) when is_list(L)    -> L.

i2l(I) when is_integer(I) -> integer_to_list(I).
a2l(A) when is_atom(A) -> atom_to_list(A).

