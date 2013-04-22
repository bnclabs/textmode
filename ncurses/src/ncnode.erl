-module(ncnode).
-author('prataprc@gmail.com').

% module API
-export([spawndom/1, getfocus/1, xpath/2, xnode/1]).

-export([init/1]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API
spawndom(Tag) ->
    Pid = erlang:spawn_link(?MODULE, init, [Tag]),
    #xnode{pid=Pid}.

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
    end;
xpath(XNode, XRoot) -> xpath(XNode, [XRoot]).


xnode(#xnode{pid=Pid}) -> xnode(Pid);
xnode(Pid) -> ncchan:send_receive(Pid, ?EV_XNODE).


%---- internal functions
init(Tag) ->
    process_flag(trap_exit, true),
    erlang:register(Tag#tag.name, self()),
    Mod = ncdom:attr(callback, Tag),
    CNodes = spawndoms(Tag#tag.content, []),
    error_logger:info_msg("ncnode ~p : ~p~n", [Tag#tag.name, self()]),
    State = #xnode{tag=Tag, pid=self(), mod=Mod, cnodes=CNodes},
    {reply, _Evt, NewState} = handle(Tag#tag.tagname, ?EV_XCREATE, State),
    msgloop(NewState). 


spawndoms([], Acc) -> lists:reverse( Acc );
spawndoms([#text{} | CTags], Acc) -> spawndoms(CTags, Acc);
spawndoms([#tag{}=CTag | CTags], Acc) ->
    spawndoms(CTags, [?MODULE:spawndom(CTag) | Acc]).


msgloop(#xnode{tag=Tag}=State) ->
    #tag{tagname=Tagname, name=Name} = Tag,
    receive
        {'EXIT', _From, _Reason} ->
            msgloop(State);
        {From, ?EV_XDESTROY=Event} ->
            {reply, OutEvent, NewState} = handle(Tagname, Event, State),
            ncchan:reply(From, OutEvent),
            utils:logexit(Name, Event),
            NewState;
        {From, ?EV_XNODE} ->
            #xnode{cnodes=CNodes} = State,
            NewState = State#xnode{cnodes=lists:map(fun xnode/1, CNodes)},
            ncchan:reply(From, NewState),
            msgloop(NewState);
        {From, #event{}=Event} ->
            case handle(Tagname, Event, State) of
                {reply, ?EV_UNLOAD(_Msg)=OutEvent, NewState} ->
                    ncchan:propagate(NewState, ?EV_XDESTROY),
                    ncchan:reply(From, OutEvent),
                    msgloop(NewState);
                {reply, OutEvent, NewState} ->
                    ncchan:reply(From, OutEvent),
                    msgloop(NewState)
            end 
    end.


%-- 
handle(box, Event, State) -> 
    {Event1, NewState1} = handle(Event, State),
    {Event2, NewState2} = xbox:handle(Event1, NewState1),
    {reply, Event2, NewState2}.


handle(?EV_LOAD(Msg)=Event, State) -> 
    #xnode{xpids=XPids} = State,
    Pid = Msg#dommsg.screen#screen.pid,
    NewState =
        case lists:member(Pid, XPids) of
            true -> State;
            false ->
                State#xnode{xpids=lists:sort(fun sort_xpids/2, [Pid | XPids])}
        end,
    {ncchan:stop_event(Event), NewState};

handle(?EV_UNLOAD(Msg)=Event, State) -> 
    #xnode{xpids=XPids} = State,
    { ncchan:stop_event(Event),
      State#xnode{xpids=lists:delete(Msg#dommsg.screen#screen.pid, XPids)}
    };

handle(Event, State) -> {Event, State}.


sort_xpids(A, B) ->
    ErlNode = node(),
    case {node(A), node(B)} of
        {ErlNode, ErlNode} -> true;
        {ErlNode, _} -> true;
        _ -> false
    end.
