-module(ecrn_app).

-behaviour(application).

%% API
-export([manual_start/0, manual_stop/0]).

-export([start/2, stop/1]).

-include("erlcron.hrl").

manual_start() ->
    %application:start(crypto),
    application:start(eunit),
    %application:start(sasl),
    application:start(erlcron).

manual_stop() ->
    application:stop(erlcron).

%% @private
start(_StartType, _StartArgs) ->
    case ecrn_sup:start_link() of
        {ok, Pid} ->
            {ok, H} = inet:gethostname(),
            Def = application:get_env(erlcron, defaults, #{}),
            is_map(Def) orelse
              erlang:error("erlcron/defaults config deve ser um mapa!"),
            ?LOG_INFO("CRON: iniciou protocolor e  ~p usando defaults: ~1024p", [H, Def]),
            setup(Def),
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
stop(_State) ->
    ok.

setup(Def) ->
    case application:get_env(erlcron, crontab) of
        {ok, Crontab} ->
            lists:foreach(fun(CronJob) ->
                Res  = erlcron:cron(CronJob, Def),
                Res2 = if is_reference(Res) -> io_lib:format(": ~p", [Res]); true -> [] end,
                ?LOG_INFO("CRON: adicionado formato ~1024p~s", [CronJob, Res2]),
                case Res of
                    already_started ->
                        erlang:error({duplicate_job_reference, CronJob});
                    ignored ->
                        ok;
                    Ref when is_reference(Ref); is_atom(Ref); is_binary(Ref) ->
                        ok;
                    {error, Reason} ->
                        erlang:error({failed_to_add_cron_job, CronJob, Reason})
                end
            end, Crontab);
        undefined ->
            ok
    end.
