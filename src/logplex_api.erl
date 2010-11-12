-module(logplex_api).
-export([loop/1, start_link/1, stop/0]).

-include_lib("logplex.hrl").

start_link(Options) ->
    io:format("START API~n"),
    mochiweb_http:start(Options).

stop() ->
    LSock = mochiweb_socket_server:get(?MODULE, listen),
    gen_tcp:close(LSock),
    ok.

%% TODO: authorize
handlers() ->
    [{['GET', "/healthcheck"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['POST', "/channels$"], fun(Req, _Match) ->
	{struct, Params} = mochijson2:decode(Req:recv_body()),
        ChannelName = proplists:get_value(<<"name">>, Params),
        ChannelName == undefined andalso error_resp(Req, 400, <<"name post param missing">>),
        ChannelId = logplex_channel:create(ChannelName),
        not is_integer(ChannelId) andalso error_resp(Req, 500, <<"failed to create channel">>),
        Req:respond({200, [{"Content-Type", "text/html"}], integer_to_list(ChannelId)})
    end},

    {['POST', "/channels/(\\d+)/token"], fun(Req, [ChannelId]) ->
	{struct, Params} = mochijson2:decode(Req:recv_body()),
        TokenName = proplists:get_value(<<"name">>, Params),
        TokenName == undefined andalso error_resp(Req, 400, <<"name post param missing">>),
        Token = logplex_token:create(ChannelId, TokenName),
        not is_list(Token) andalso error_resp(Req, 500, <<"failed to create token">>),
        Req:respond({200, [{"Content-Type", "text/html"}], Token})
    end},

    {['POST', "/sessions"], fun(Req, _Match) ->
        Body = Req:recv_body(),
        Session = logplex_session:create(Body),
        not is_list(Session) andalso error_resp(Req, 500, <<"failed to create session">>),
        Req:respond({200, [{"Content-Type", "text/html"}], Session})
    end},

    {['GET', "/sessions/([\\w-]+)"], fun(Req, [Session]) ->
        Body = logplex_session:lookup(Session),
        not is_binary(Body) andalso error_resp(Req, 404, <<"session not found">>),
        Data = mochiweb_util:parse_qs(Body),
        ChannelId = proplists:get_value("channel_id", Data),
        not is_list(ChannelId) andalso error_resp(Req, 400, <<"session missing channel_id">>), 
        Filters = filters(Data),
        Num0 =
            case proplists:get_value("num", Data) of
                undefined -> 20;
                StrNum -> list_to_integer(StrNum)
            end,
        Num = ternary(Filters == [], Num0, -1),
        Logs = logplex_channel:logs(ChannelId, Num),
        Socket = Req:get(socket),
        Req:start_response({200, [{"Content-Type", "text/html"}]}),
        [begin
            Msg1 = logplex_utils:parse_msg(Msg),
            logplex_utils:filter(Msg1, Filters) andalso gen_tcp:send(Socket, logplex_utils:format(Msg1))
        end || {ok, Msg} <- lists:reverse(Logs)],
        case proplists:get_value("tail", Data) of
            undefined -> ok;
            _ ->
                logplex_tail:register(list_to_binary(ChannelId)),
                tail_loop(Socket, Filters)
        end,
        gen_tcp:close(Socket)
    end},

    {['GET', "/channels/(\\d+)/info"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['GET', "/channels/(\\d+)/drains"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['POST', "/channels/(\\d+)/drains"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['DELETE', "/channels/(\\d+)/drains"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['GET', "/channels/(\\d+)/drains/(\\w+)"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end},

    {['DELETE', "/channels/(\\d+)"], fun(Req, _Match) ->
        Req:respond({200, [], <<"OK">>})
    end}].

loop(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    io:format("REQ: ~p ~p~n", [Method, Path]),
    serve(handlers(), Method, Path, Req),
    ok.

serve([], _Method, _Path, Req) ->
    Req:respond({404, [], <<"Not Found.">>});

serve([{[HMethod, Regexp], Fun}|Tail], Method, Path, Req) ->
    case re:run(Path, Regexp, [{capture, all_but_first, list}]) of
        {match, Captured} when HMethod == Method ->
            case catch Fun(Req, Captured) of
                {'EXIT', Reason} -> exit(Reason);
                _ -> ok
            end;
        _ ->
            serve(Tail, Method, Path, Req)
    end.

error_resp(Req, RespCode, Body) ->
    Req:respond({RespCode, [{"Content-Type", "text/html"}], Body}),
    throw(normal).

tail_loop(Socket, Filters) ->
    inet:setopts(Socket, [{packet, raw}, {active, once}]),
    receive
        {log, Msg} ->
            io:format("recv'd tail msg ~p~n", [Msg]),
            Msg1 = logplex_utils:parse_msg(Msg),
            logplex_utils:filter(Msg1, Filters) andalso gen_tcp:send(Socket, logplex_utils:format(Msg1)),
            tail_loop(Socket, Filters);
        {tcp_closed, Socket} ->
            io:format("client closed socket~n")
    end.

filters(Data) ->
    filters(Data, []).

filters([], Filters) ->
    Filters;

filters([{"ps", Ps}|Tail], Filters) ->
    Filters1 = [
        fun(Msg) ->
            Msg#msg.ps == Ps
        end | Filters],
    filters(Tail, Filters1);

filters([{"source", Source}|Tail], Filters) ->
    Filters1 = [
        fun(Msg) ->
            Msg#msg.source == Source
        end | Filters],
    filters(Tail, Filters1);

filters([_|Tail], Filters) ->
    filters(Tail, Filters).

ternary(true, A, _B) -> A;
ternary(_, _A, B) -> B.
