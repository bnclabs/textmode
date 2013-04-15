-module(ncnode).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module API
-export([start/1, shutdown/1]).

% behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API
start({PPid, Node}) -> ?MODULE:start({PPid, Node, Node});
start({PPid, RootNode, Node}) ->
    Name = list_to_atom( Node#node.name ),
    gen_server:start({local, Name}, ?MODULE, {PPid, RootNode, Node}, []).


shutdown(Node) ->
    Name = list_to_atom( Node#node.name ),
    gen_server:call(Name, shutdown, infinity).

%---- behaviour Callbacks
init({ParentPid, RootNode, Node}) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("ncnode ~p : ~p~n", [Node#node.name, self()]),
    Tup = {self(), RootNode},
    ChildPids = spawn_childnodes(Tup, Node#node.content, []),
    Box = Node#node.box,
    {ok, #nodew{
            pproc=ParentPid,
            cprocs=ChildPids,
            pid=self(),
            rootnode=RootNode,
            node=Node,
            box=Box,
            view=Box#box.view}}.


% To be received only for the root node.
handle_call(shutdown, _From, State) ->
    % TODO : Handle DOM shutdown, by something like `unload`.
    {reply, ok, State}.


handle_info(_Info, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {noreply, State}.


terminate(normal, Name) ->
    error_logger:info_msg("ncnode ~p terminating : ~p~n", [Name, normal]);

terminate(Reason, State) ->
    Name = State#nodew.node#node.name,
    error_logger:info_msg("ncnode ~p terminating : ~p~n", [Name, Reason]).

%---- internal functions
spawn_childnodes(_, [], Acc) -> lists:reverse( Acc );
spawn_childnodes(Tup, [#text{} | CNodes], Acc) ->
    spawn_childnodes(Tup, CNodes, Acc);
spawn_childnodes(Tup, [#node{}=CNode | CNodes], Acc) ->
    {ParentPid, RootNode} = Tup,
    {ok, CPid} = ?MODULE:start({ParentPid, RootNode, CNode}),
    spawn_childnodes(Tup, CNodes, [CPid | Acc]).

