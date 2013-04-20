-module(tree).
-author('prataprc@gmail.com').

% module API
-export([preorder/3, inorder/3, levelorder/3]).

%---- module API
preorder(Root, Fold, Children) ->
    lists:reverse( prefold(Root, Fold, Children, []) ).

inorder(Root, Fold, Children) ->
    lists:reverse( infold(Root, Fold, Children, []) ).

levelorder(Root, Fold, Children) ->
    lists:reverse( lvlfold([Root], Fold, Children, [], []) ).


%-- internal functions
prefold([], _, _, Acc) -> Acc;
prefold([CNode | CNodes], Fold, Children, Acc) ->
    prefold(CNodes, Fold, Children, prefold(CNode, Fold, Children, Acc));
prefold(Node, Fold, Children, Acc) ->
    prefold( Children(Node), Fold, Children, Fold(Node, Acc) ).

infold([], _, _, Acc) -> Acc;
infold([CNode | CNodes], Fold, Children, Acc) ->
    infold(CNodes, Fold, Children, infold(CNode, Fold, Children, Acc));
infold(Node, Fold, Children, Acc) ->
    Fold(Node, infold(Children(Node), Fold, Children, Acc)).

lvlfold([], _, _, [], Acc) -> Acc;
lvlfold([], Fold, Children, Level, Acc) -> 
    lvlfold(Level, Fold, Children, [], Acc);
lvlfold([Node | Nodes], Fold, Children, Level, Acc) ->
    lvlfold(Nodes, Fold, Children, Level ++ Children(Node), Fold(Node, Acc)).
