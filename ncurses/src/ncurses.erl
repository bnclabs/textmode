-module(ncurses).
-behaviour(application).

% Behaviour Callbacks
-export([start/2, stop/1]).

% Behaviour Callbacks
start(_, _) ->
    nc_srv:start_link().

stop(_) ->
    ok.
