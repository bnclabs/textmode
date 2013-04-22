-module(xbox).
-author('prataprc@gmail.com').

% Module API
-export([handle/2]).

-include("ncurses.hrl").
-include("ncdom.hrl").



handle(?EV_XCREATE=Event, XNode) -> 
    callback(oncreate, [Event, XNode]);

handle(?EV_LOAD(_Msg)=Event, XNode) -> 
    callback(onload, [Event, XNode]);

handle(?EV_UNLOAD(_Msg)=Event, XNode) -> 
    callback(onunload, [Event, XNode]);

handle(?EV_KEYPRESS(_Msg)=Event, XNode) ->
    callback(onkeypress, [Event, XNode]);

handle(?EV_XDESTROY=Event, XNode) -> 
    callback(ondestroy, [Event, XNode]).


callback(Fn, [Event, XNode]=Args) ->
    case XNode of
        #xnode{mod=none} -> {Event, XNode};
        #xnode{mod=false} -> {Event, XNode};
        #xnode{mod=Mod} -> erlang:apply(Mod, Fn, Args)
    end.
