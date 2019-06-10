-compile([{parse_transform, lager_transform}]).

-include_lib("vtx_common/include/macro.hrl").

-include_lib("vtx_common/include/iam.hrl").

-include_lib("vtx_common/include/air.hrl").

%-include_lib("vtx_common/include/octopus.hrl").
-include_lib("octopus.hrl").
-include_lib("ns.hrl").

-include_lib("vtx_common/include/iris.hrl").

-include_lib("vtx_common/include/cell.hrl").

% erlson
-include_lib("erlson/include/erlson.hrl").
% elog
%-include_lib("elog/include/elog.hrl").

-define(COMMITTED,  0).
-define(CREATED,   -1).
-define(DELETED,   -2).
-define(PHANTOM,   -3).

-define(SEPERATOR, ":").
-define(ZONE_INDEX,     1).
-define(SERVICE_INDEX,  2).
-define(RESOURCE_INDEX, 3).

% version
-define(IAM_VERSION,  "v1").
-define(AIR_VERSION,  "v1").
-define(COS_VERSION,  "v1").
-define(BUZZ_VERSION, "v1").
-define(IRIS_VERSION, "v1").

-record(exception, {
  code,
  message
}).


-define(
    TRY_CALL_FUN(Thunk),
    fun() ->
        case Thunk of
            true -> true;
            ok -> ok;
            {ok, Any} -> {ok, Any};
            {error, Code} -> throw(#exception{code=Code});
            _ -> throw(#exception{code=500})
        end
    end
).

-define(TRY_CALL(THUNK), ?TRY_CALL_FUN(THUNK)()).
