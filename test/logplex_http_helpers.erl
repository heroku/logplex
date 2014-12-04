-module(logplex_http_helpers).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%% Other helpers
get_(Url, Opts) ->
    request(get, Url, Opts).

post(Url, Opts) ->
    request(post, Url, Opts).

delete(Url, Opts) ->
    request(delete, Url, Opts).

request(Method, Url, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),
    Timeout = proplists:get_value(timeout, Opts, 1000),
    Request =
        case Method of
            post ->
                ContentType = proplists:get_value(content_type, Opts, "application/json"),
                BodyToSend = proplists:get_value(body, Opts, []),
                {Url, Headers, ContentType, BodyToSend};
            _ ->
                {Url, Headers}
        end,
    HttpOpts = proplists:get_value(http_opts, Opts, []),
    HttpcOpts = proplists:get_value(opts, Opts, []),
    case httpc:request(Method, Request, [{timeout, Timeout}| HttpOpts], HttpcOpts) of
        {ok, {{HttpVersion, StatusCode, HttpReason}, Headers0, Body}} ->
            [{status_code, StatusCode},
             {http_version, HttpVersion},
             {http_reason, HttpReason},
             {headers, Headers0},
             {body, Body}];
        {ok, {StatusCode, Body}} ->
            [{status_code, StatusCode},
             {body, Body}];
        {ok, ReqId} when is_reference(ReqId) ->
            {Headers0, Body} = wait_for_http(ReqId, [], []),
            [{headers, Headers0},
             {body, Body}];
        Other -> ct:pal("request fail: ~p", [Other]),
                 exit(bad_case)
    end.

wait_for_http(RequestId, Headers, Body) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            wait_for_http(RequestId, Headers, Body);
        {http, {RequestId, stream, BinBody}} ->
            Body0 = binary_to_list(BinBody),
            wait_for_http(RequestId, Headers, Body++Body0);
        {http, {RequestId, stream_end, Headers0}} ->
            {Headers++Headers0, Body}
    end.

wait_for_messages(Channel, MessageAmount, MaxWait, StartTime) ->
    case  timer:now_diff(os:timestamp(), StartTime) / 1000 > MaxWait of
        true ->
            {error, timeout};
        _ ->
            Msg = logplex_SUITE:read_logs(Channel),
            if
                length(Msg) >= MessageAmount ->
                    Msg;
                true ->
                    timer:sleep(10),
                    wait_for_messages(Channel, MessageAmount, MaxWait, StartTime)
            end
    end.
