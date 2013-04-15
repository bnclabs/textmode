-module(ncchan).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% module API
-export([]).

% behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

-include_lib("ncurses.hrl").
-include_lib("ncdom.hrl").

%---- module API

%---- behaviour callbacks

init({ParentPid, RootNode, Node}) ->
    process_flag(trap_exit, true),
    {ok, {}}.


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

new(Chname, Channels) ->
    [{Chname, []} | Chans].

subscribe(Chname, Mod, Fn, State) ->
    subscribe(Chname, Mod, Fn, [], State).

subscribe(Chname, Mod, Fn, Args, Channels) ->
    #channel{subscribers=Subscrs}=Channel = lists:keyfind(Chname, 2, Channels),
    Ref = erlang:make_ref(),
    lists:keyreplace(
        Chname, 2, Channels,
        Channel#channel{subscribers=[{Ref, Mod, Fn, Args} | Subscrs]} ).

subscribe(Chname, Proc, Channels) ->
    #channel{subscribers=Subscrs}=Channel = lists:keyfind(Chname, 2, Channels),
    Ref = erlang:make_ref(),
    lists:keyreplace(
        Chname, 2, Channels,
        Channel#channel{subscribers=[{Ref, Proc} | Subscrs]} ).

unsubscribe(Ref, Channels)
    #channel{subscribers=Subscrs}=Channel = lists:keyfind(Chname, 2, Channels),
    lists:keyreplace(
        Chname, 2, Channels,
        Channel#channel{subscribers=lists:keydelete(Ref, 1, Subscrs)} ).

event(Chname, Event, Channels) ->
    #channel{subscribers=Subscrs}=Channel = lists:keyfind(Chname, 2, Channels),
    doevent(Subscrs, Event).


doevent([], Event) -> ok;
doevent([{Ref, M, F, Args} | Rest], Event) ->
    erlang:apply(M, F, [Event | Args]),
    doevent(Rest, Event).
doevent([{Ref, Proc} | Rest], Event) ->
    Proc ! Event,
    doevent(Rest, Event).


nodepath(Name, #node{content=Content, name=Name}=Node) ->
    [Node];
nodepath(Name, #node{content=Content}=Node) ->
    case nodepath(Node#node.content) of
        [] -> [];
        Ls -> [ Node | Ls ];
    end;
nodepath(Name, []) ->
    [];
nodepath(Name, [#text{} | CNodes]) ->
    nodepath(CNodes);
nodepath(Name, [#node{}=CNode | CNodes]) ->
    nodepath(CNode)


bubbleup([], Event) -> ok;
bubbleup([Node | Nodes], Event) ->
    Name = list_to_atom(Node#node.name),
    Name ! Event,
    bubbleup(Nodes, Event).



