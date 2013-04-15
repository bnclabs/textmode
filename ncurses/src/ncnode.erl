-module(ncnode).
-author('prataprc@gmail.com').

% module API
-export([start_link/1]).

-export([node_handler/1]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API
start_link({PPid, Node}) -> ?MODULE:start_link({PPid, Node, Node});
start_link({_, _, Node}=Args) ->
    Pid = erlang:spawn_link(?MODULE, node_handler, Args),
    erlang:register( list_to_atom(Node#node.name), Pid ),
    {ok, Pid}.

%---- internal functions
node_handler({ParentPid, RootNode, Node}) ->
    process_flag(trap_exit, true),
    Tup = {self(), RootNode},
    ChildPids = spawn_kids(Tup, Node#node.content, []),
    Box = Node#node.box,
    error_logger:info_msg("ncnode ~p : ~p~n", [Node#node.name, self()]),
    State = #nodew{pproc=ParentPid,
                   cprocs=ChildPids,
                   pid=self(),
                   rootnode=RootNode,
                   node=Node,
                   box=Box,
                   view=Box#box.view},
    msgloop(State).
    
msgloop(State) ->
    NewState = 
        receive
            {_From, shutdown} -> % To be received only for the root node.
                % TODO : Handle DOM shutdown, by something like `unload`.
                State
        end,
    msgloop(NewState).

spawn_kids(_, [], Acc) -> lists:reverse( Acc );
spawn_kids(Tup, [#text{} | CNodes], Acc) -> spawn_kids(Tup, CNodes, Acc);
spawn_kids(Tup, [#node{}=CNode | CNodes], Acc) ->
    {ParentPid, RootNode} = Tup,
    {ok, CPid} = ?MODULE:start_link({ParentPid, RootNode, CNode}),
    spawn_kids(Tup, CNodes, [CPid | Acc]).
