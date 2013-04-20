-module(ncnode).
-author('prataprc@gmail.com').

% module API
-export([spawndom/1, getfocus/1, xpath/2, flatten/2]).

-export([init/1]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API
spawndom(Tag) ->
    Pid = erlang:spawn_link(?MODULE, init, Tag),
    XNode = ncchan:send_receive(Pid, ?EV_XNODE),
    {ok, Pid, XNode}.

getfocus([]) -> none;
getfocus([#xnode{tag=Tag, cnodes=CNodes}=XNode | XNodes]) -> 
    case ncdom:attr(focus, Tag) of
        {focus, "default"} -> XNode;
        false -> case getfocus(CNodes) of none -> getfocus(XNodes); N -> N end
    end;
getfocus(#xnode{}=XNode) -> getfocus([XNode]).


xpath(#xnode{pid=Pid}=XNode, [#xnode{pid=Pid} | _]) -> [XNode];
xpath(_, []) -> [];
xpath(XNode, [#xnode{cnodes=CNodes}=N | Ns]) ->
    case xpath(XNode, CNodes) of
        [] -> xpath(XNode, Ns);
        XPath -> [N | XPath]
    end.


flatten(preorder, #xnode{}=XNode) -> lists:reverse( flatten( XNode, [] )).


%add_xdrv(Pid, #xnode{xpids=XPids, cnodes=CNodes}=XNode) ->
%    Fn = fun(A, B) -> 
%            case {node(A), node(B)} of
%                {node(), node()} -> true;
%                {node(), _} -> true;
%                _ -> false
%            end
%         end,
%    NCRefs = lists:sort(Fn, [Pid | XPids]),
%    XNode#xnode{xpids=NCRefs, cnodes=add_xdrv(Pid, CNodes, [])}.
%
%add_xdrv(_, [], Acc) -> lists:reverse(Acc);
%add_xdrv(Pid, [CNode | CNodes], Acc) -> 
%    add_xdrv(Pid, CNodes, [add_xdrv(Pid, CNode) | Acc]).


%---- internal functions
init(Tag) ->
    process_flag(trap_exit, true),
    erlang:register(Tag#tag.name, self()),
    Mod = ncdom:attr(callback, Tag),
    CNodes = spawndoms(Tag#tag.content),
    error_logger:info_msg("ncnode ~p : ~p~n", [Tag#tag.name, self()]),
    msgloop( #xnode{tag=Tag, pid=self(), mod=Mod, cnodes=CNodes} ).


spawndoms([], Acc) -> lists:reverse( Acc );
spawndoms([#text{} | CTags], Acc) -> spawndoms(CTags, Acc);
spawndoms([#tag{}=CTag | CTags], Acc) ->
    {ok, _XPid, XNode} = ?MODULE:spawndom(CTag),
    spawndoms(CTags, [XNode | Acc]).

spawndoms(CTags) -> spawndoms(CTags, []).


msgloop(State) ->
    NewState =
        receive
            {From, shutdown} -> % Incomplete !!
                From ! State;
            {From, #event{}=Event} ->
                case handle(Event, State) of
                    {reply, Reply, S} -> ncchan:reply(From, Reply), S
                end
        end,
    msgloop(NewState).



%-- event handlers
handle(?EV_XNODE=_Event, State) ->
    {reply, State, State};

handle(?EV_LOAD=IEvent, #xnode{mod=Mod, state=XState}=State) ->
    {OEvent, NodeState1} = erlang:apply(Mod, onload, [IEvent, XState]),
    {reply, OEvent, State#xnode{state=NodeState1}};

handle(?EV_UNLOAD=IEvent, #xnode{mod=Mod, state=XState}=State) ->
    NodeState1 = erlang:apply(Mod, onunload, [IEvent, XState]),
    {reply, ok, State#xnode{state=NodeState1}};

handle(?EV_KEYPRESS()=IEvent, #xnode{mod=Mod, state=XState}=State) ->
    NodeState1 = erlang:apply(Mod, onkeypress, [IEvent, XState]),
    {reply, ok, State#xnode{state=NodeState1}}.
