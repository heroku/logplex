%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
-module(logplex_logging).

-export([dest/2]).

dest(Host, Port) ->
    iolist_to_binary([host_str(Host), ":", integer_to_list(Port)]).

host_str({A,B,C,D}) ->
    Quads = [integer_to_list(Quad) || Quad <- [A,B,C,D]],
    string:join(Quads,".");
host_str(H)
  when is_list(H); is_binary(H) ->
    H.
