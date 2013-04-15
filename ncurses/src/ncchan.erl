-module(ncchan).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module API
-export([start_link/1, new/1, subscribe/2, subscribe/3, subscribe/4,
         unsubscribe/2, event/2, adddom/2, bubbleup/3]).

% behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

new(Chname) ->
    gen_server:call(?MODULE, {new, Chname}, infinity).

subscribe(Chname, Proc) ->
    gen_server:call(?MODULE, {subscribe, Chname, Proc}, infinity).

subscribe(Chname, Mod, Fn) ->
    gen_server:call(?MODULE, {subscribe, Chname, Mod, Fn, []}, infinity).

subscribe(Chname, Mod, Fn, Args) ->
    gen_server:call(?MODULE, {subscribe, Chname, Mod, Fn, Args}, infinity).

unsubscribe(Chname, Ref) ->
    gen_server:call(?MODULE, {unsubscribe, Chname, Ref}, infinity).

event(Chname, Event) ->
    gen_server:call(?MODULE, {event, Chname, Event}, infinity).

adddom(AppPath, RootNode) ->
    gen_server:call(?MODULE, {adddom, AppPath, RootNode}, infinity).

bubbleup(AppPath, Name, Event) ->
    gen_server:call(?MODULE, {bubbleup, AppPath, Name, Event}, infinity).


%---- behaviour callbacks

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #evtsys{channels=[], appdoms=[]}}.


handle_call({new, Chname}, _From, #evtsys{channels=Channels}=State) ->
    {reply, ok,
      State#evtsys{
        channels=[#channel{name=Chname, subscribers=[]} | Channels] }};

handle_call({subscribe, Chname, Mod, Fn, Args}, _From, State) ->
    Ref = erlang:make_ref(),
    case add_handler(Chname, {Ref, Mod, Fn, Args}, State) of
        {false, NewState} -> {reply, false, NewState};
        {true, NewState} -> {reply, Ref, NewState}
    end;

handle_call({subscribe, Chname, Proc}, _From, State) ->
    Ref = erlang:make_ref(),
    case add_handler(Chname, {Ref, Proc}, State) of
        {false, NewState} -> {reply, false, NewState};
        {true, NewState} -> {reply, Ref, NewState}
    end;

handle_call({unsubscribe, Chname, Ref}, _From, State) ->
    {reply, ok, remove_handler(Chname, Ref, State)};

handle_call({event, Chname, Event}, _From, State) ->
    #evtsys{channels=Channels} = State,
    #channel{subscribers=Subscrs} = lists:keyfind(Chname, 2, Channels),
    doevent(Subscrs, Event),
    {reply, ok, State};

handle_call({adddom, AppPath, RootNode}, _From, State) ->
    #evtsys{appdoms=Doms}=State,
    {reply, ok, State#evtsys{appdoms=[{AppPath, RootNode} | Doms]}};

handle_call({deldom, AppPath}, _From, State) ->
    #evtsys{appdoms=Doms}=State,
    {reply, ok, State#evtsys{appdoms=lists:keydelete(AppPath, 1, Doms)}};

handle_call({bubbleup, AppPath, Name, Event}, _From, State) ->
    #evtsys{appdoms=Doms}=State,
    {AppPath, RootNode} = lists:keyfind(AppPath, 1, Doms),
    bubble_event(Event, lists:reverse( nodepath( Name, RootNode ))),
    {reply, ok, State}.


handle_info({'EXIT', From, Reason}, State) ->
    error_logger:info_msg("Process ~p exited because of ~p~n", [From, Reason]),
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


code_change(_, State, _) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


%---- internal functions

add_handler(Chname, Item, #evtsys{channels=Channels}=State) ->
    case lists:keyfind(Chname, 2, Channels) of
        false ->
            {false, State};      % TODO : Log error ?
        #channel{subscribers=Subs}=Channel ->
            Chnl = Channel#channel{subscribers=[Item | Subs]},
            {true,
             State#evtsys{channels=lists:keyreplace(Chname, 2, Channels, Chnl)}
            }
    end.


remove_handler(Chname, Ref, #evtsys{channels=Channels}=State) ->
    case lists:keyfind(Chname, 2, Channels) of
        false ->
            State;              % TODO : Log error ?
        #channel{subscribers=Subs}=Channel ->
            Chnl = Channel#channel{subscribers=lists:keydelete(Ref, 2, Subs)},
            State#evtsys{channels=lists:keyreplace(Chname, 2, Channels, Chnl)}
    end.


doevent([], _) -> ok;
doevent([{Ref, M, F, Args} | Rest], Event) ->
    erlang:spawn_link(M, F, [Ref, Event | Args]),
    doevent(Rest, Event);
doevent([{Ref, Proc} | Rest], Event) ->
    Proc ! {Ref, Event},
    doevent(Rest, Event).


bubble_event(_, []) -> ok;
bubble_event(Event, [#node{name=Name}, Nodes]) ->
    list_to_atom(Name) ! Event,
    bubble_event(Event, Nodes).

nodepath(_, []) -> [];
nodepath(Name, [#text{} | CNodes]) -> nodepath(Name, CNodes);
nodepath(Name, [#node{}=CNode | CNodes]) -> 
    case nodepath(Name, CNode) of
        [] -> nodepath(Name, CNodes);
        Ls -> Ls
    end;
nodepath(Name, #node{name=Name}=Node) -> [Node];
nodepath(Name, #node{content=Content}=Node) ->
    case nodepath(Name, Content) of
        [] -> [];
        Ls -> [Node | Ls]
    end.
