-module(utils).
-author('prataprc@gmail.com').

% module API
-export([logexit/2, logexit/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

logexit(From, Reason, Pid) ->
    error_logger:info_msg(
        "Proc ~p exited with ~p in ~p~n", [From, Reason, Pid] ).

logexit(Pid, Reason) ->
    error_logger:info_msg(
        "Proc ~p exiting with ~p ~n", [Pid, Reason] ).

