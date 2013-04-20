-module(tree).
-author('prataprc@gmail.com').

% module API
-export([preorder/4, inorder/4, postorder/4, levelorder/4]).

-ifdef(TEST).
-record( x, {v, ns=[]}).
-include_lib("eunit/include/eunit.hrl").
-endif.

%---- module API
preorder(Root, Fold, Children, Acc) ->
    prefold(Root, Fold, Children, Acc).

inorder(_Root, _Fold, _Children, _Acc) ->
    "Not implemented".

postorder(Root, Fold, Children, Acc) ->
    postfold(Root, Fold, Children, Acc).

levelorder(Root, Fold, Children, Acc) ->
    lvlfold([Root], Fold, Children, [], Acc).


%-- internal functions
prefold([], _, _, Acc) -> Acc;
prefold([CNode | CNodes], Fold, Children, Acc) ->
    prefold(CNodes, Fold, Children, prefold(CNode, Fold, Children, Acc));
prefold(Node, Fold, Children, Acc) ->
    prefold( Children(Node), Fold, Children, Fold(Node, Acc) ).

postfold([], _, _, Acc) -> Acc;
postfold([CNode | CNodes], Fold, Children, Acc) ->
    postfold(CNodes, Fold, Children, postfold(CNode, Fold, Children, Acc));
postfold(Node, Fold, Children, Acc) ->
    Fold(Node, postfold(Children(Node), Fold, Children, Acc)).

lvlfold([], _, _, [], Acc) -> Acc;
lvlfold([], Fold, Children, Level, Acc) -> 
    lvlfold(Level, Fold, Children, [], Acc);
lvlfold([Node | Nodes], Fold, Children, Level, Acc) ->
    lvlfold(Nodes, Fold, Children, Level ++ Children(Node), Fold(Node, Acc)).


-ifdef(TEST).
%---- eunit test cases for tree algorithm.

sampletree() ->
    #x{v=1, ns=[ #x{v=2, ns=[ #x{v=4, ns=[#x{v=7}]}, #x{v=5} ]},
                 #x{v=3, ns=[ #x{v=6, ns=[#x{v=8}, #x{v=9}]} ]} ]}.

sample_test() ->
    Fold = fun(E, Acc) -> [E | Acc] end,
    Children = fun(#x{ns=Cs}) -> Cs end,
    preorder(sampletree(), Fold, Children),
    postorder(sampletree(), Fold, Children),
    levelorder(sampletree(), Fold, Children),
    ok.
-endif.
