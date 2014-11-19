%% @copyright Heroku
%% @author Andrew Gwozdziewycz <apg@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Rest server for logplex
%% @end

-module(logplex_rest).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-export([child_spec/0, dispatch/0,
        is_authorized/2]).

child_spec() ->
    ranch:child_spec(?MODULE, 100,
                     ranch_tcp,
                     [{port, logplex_app:config(http_log_input_port)}],
                     cowboy_protocol,
                     [{env,
                       [{dispatch, dispatch()}]}]).

dispatch() ->
    cowboy_router:compile([{'_',
                            [{<<"/healthcheck">>, logplex_logs_rest, [healthcheck]},
                             {<<"/logs">>, logplex_logs_rest, [logs]},
                             % Deprecate all but v3
                             {<<"/v3/channels[/:chan_id]">>, [{chan_id, int}], logplex_channels_rest, []},
                             {<<"/v3/channels[/:chan_id]/token">>, [{chan_id, int}], logplex_channels_rest, [token]}
%%                             {<<"/v3/sessions/[...]">>, logplex_sessions_rest, [api]}
                            ]}]).


is_authorized(Req, State) ->
   { true, Req, State }.
