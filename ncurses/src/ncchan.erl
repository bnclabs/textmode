-module(ncchan).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module API
-export([start_link/1, new/1, subscribe/2, subscribe/3, subscribe/4,
         unsubscribe/2, publish/2, bubble/2, capture/2, propagate/2,
         send_receive/2, reply/2, stop_event/1, finish_event/1]).

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

bubble(Doc, Event) ->
    gen_server:cast(?MODULE, {bubble, self(), Doc, Event}).

capture(Doc, Event) ->
    gen_server:cast(?MODULE, {capture, self(), Doc, Event}).

propagate(#xnode{}=XNode, Event) ->
    gen_server:cast(?MODULE, {propagate, self(), XNode, Event}).

publish(Chname, Event) ->
    gen_server:cast(?MODULE, {publish, self(), Chname, Event}).

send_receive(Proc, Msg) ->
    Proc ! {self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(From, Msg) ->
    From ! {reply, Msg}.


stop_event(#event{}=Event) -> Event#event{status=stop}.

finish_event(#event{}=Event) -> Event#event{status=finish}.


%---- behaviour callbacks
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #evtsys{channels=[]}}.


handle_call({new, Chname}, _From, #evtsys{channels=Channels}=State) ->
    Channel = #channel{name=Chname, subscribers=[]},
    {reply, ok, State#evtsys{channels=[Channel| Channels] }};

handle_call({subscribe, Chname, Mod, Fn, Args}, _From, State) ->
    Ref = erlang:make_ref(),
    case add_handler(Chname, {Ref, Mod, Fn, Args}, State) of
        {error, _}=Reason -> {reply, Reason, State};
        NewState -> {reply, Ref, NewState}
    end;

handle_call({subscribe, Chname, Proc}, _From, State) ->
    Ref = erlang:make_ref(),
    case add_handler(Chname, {Ref, Proc}, State) of
        {error, _}=Reason -> {reply, Reason, State};
        NewState -> {reply, Ref, NewState}
    end;

handle_call({unsubscribe, Chname, Ref}, _From, State) ->
    case remove_handler(Chname, Ref, State) of
        {error, _}=Reason -> {reply, Reason, State};
        NewState -> {reply, ok, NewState}
    end.


handle_cast({bubble, From, #doc{root=XRoot,focus=XNode}, Event}, State) ->
    Ev = domevent( lists:reverse( ncnode:xpath( XNode, XRoot )), Event ),
    reply(From, {event, Ev}),
    {noreply, State};

handle_cast({capture, From, #doc{root=XRoot,focus=XNode}, Event}, State) ->
    Ev = domevent( ncnode:xpath(XNode, XRoot), Event ),
    reply(From, {event, Ev}),
    {noreply, State};

handle_cast({propagate, From, #xnode{}=XNode, Event}, State) ->
    Fold = fun(Node, Acc) -> [Node | Acc] end,
    Children = fun(#xnode{cnodes=CNodes}) -> CNodes end,
    XNodes = lists:reverse( tree:preorder( XNode, Fold, Children, [] )),
    Ev = domevent(XNodes, Event),
    reply(From, {event, Ev}),
    {noreply, State};

handle_cast({publish, From,Chname,Event}, #evtsys{channels=Channels}=State) ->
    #channel{subscribers=Subscrs} = lists:keyfind(Chname, 2, Channels),
    reply( From, {event, chevent(Subscrs, Event)}),
    {noreply, State}.


handle_info({'EXIT', From, Reason}, State) ->
    utils:logexit(From, Reason, self()),
    {noreply, State}.


code_change(_, State, _) ->
    {noreply, State}.


terminate(Reason, State) ->
    timer:sleep(1000),
    error_logger:info_msg("ncchan terminating : ~p ~p~n", [Reason, State]),
    {noreply, State}.


%---- internal functions

add_handler(Chname, Item, #evtsys{channels=Channels}=State) ->
    case lists:keyfind(Chname, 2, Channels) of
        false ->
            {error, "channel not found"};
        #channel{subscribers=Subs}=Channel ->
            Chnl = Channel#channel{subscribers=[Item | Subs]},
            State#evtsys{channels=lists:keyreplace(Chname, 2, Channels, Chnl)}
    end.


remove_handler(Chname, Ref, #evtsys{channels=Channels}=State) ->
    case lists:keyfind(Chname, 2, Channels) of
        false ->
            {error, "channel not found"};
        #channel{subscribers=Subs}=Channel ->
            Chnl = Channel#channel{subscribers=lists:keydelete(Ref, 1, Subs)},
            State#evtsys{channels=lists:keyreplace(Chname, 2, Channels, Chnl)}
    end.


chevent([], _, Event) -> finish_event(Event);
chevent(_, _, #event{status=stop}=Event) -> Event;
chevent([{_, M, F, Args} | Rest], Event, Opts) ->
    EArgs = Args ++ [Event],
    chevent(
        Rest,
        case lists:member(asyn, Opts) of
            true -> erlang:spawn_link(M, F, EArgs), Event;
            false -> erlang:apply(M, F, EArgs)
        end,
        Opts
    );

chevent([{_, Proc} | Rest], Event, Opts) ->
    chevent(
        Rest,
        case lists:member(asyn, Opts) of
            true -> Proc ! Event;
            false -> send_receive(Proc, Event)
        end,
        Opts
    ).

chevent(X, Event) -> chevent(X, Event, [asyn]).


domevent([], Event) -> finish_event(Event);
domevent(_, #event{status=stop}=Event) -> Event;
domevent([#xnode{pid=Pid} | Rest], #event{status=go}=Event) ->
    domevent( Rest, send_receive( Pid, Event )).

