-module(ncnode).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% Module API
-export([start/1]).

% Behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- Module API
start({PPid, Node}) ->
    Name = Node#node.name,
    CPids = dom_spawn(PPid, Node#node.content, []),
    gen_server:start({local, Name}, ?MODULE, {PPid, CPids, Node}, []).

%load
%unload

%---- Behaviour Callbacks
init({ParentPid, ChildPids, Node}) ->
    process_flag(trap_exit, true),
    Box = Node#node.box,
    {ok, #nodew{
            pproc=ParentPid,
            cprocs=ChildPids,
            node=Node,
            box=Box,
            view=Box#box.view}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

%---- internal functions
dom_spawn(_, [], Acc) -> lists:reverse( Acc );
dom_spawn(ParentPid, [#text{} | CNodes], Acc) ->
    dom_spawn(ParentPid, CNodes, Acc);
dom_spawn(ParentPid, [#node{}=CNode | CNodes], Acc) ->
    {ok, CPid} = ?MODULE:start(ParentPid, CNode),
    dom_spawn(ParentPid, CNodes, [CPid | Acc]).

