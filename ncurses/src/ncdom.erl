-module(ncdom).
-author('prataprc@gmail.com').

-export([pagefile/1, boxify/2]).

-include("ncdom.hrl").
-include_lib("xmerl/include/xmerl.hrl").

pagefile(XmlFile) ->
    {Node, _Misc} = xmerl_scan:file( XmlFile ),
    xmlelement(Node).


xmlelement(Node) ->
    Tag = Node#xmlElement.name,
    Attrs = xmlattrs(Node#xmlElement.attributes, []),
    Content = xmlcontent(Node#xmlElement.content, []),
    #node{tag=Tag, attributes=Attrs, content=Content}.


xmlattrs([], Acc) -> lists:reverse(Acc);
xmlattrs([#xmlAttribute{name=Name, value=Value} | As], Acc) ->
    Value =
        case Name of
            "y" -> list_to_integer(Value);
            "x" -> list_to_integer(Value);
            _ -> Value
        end,
    xmlattrs(As, [{Name, Value} | Acc]).


xmlcontent([], Acc) -> lists:reverse(Acc);
xmlcontent([#xmlText{value=_} | Cs], Acc) -> xmlcontent(Cs, Acc);
xmlcontent([#xmlElement{}=E | Cs], Acc) ->
    xmlcontent(Cs, [{elem, xmlelement(E)} | Acc]).


boxify({Y, X, Rows, Cols}, Node) ->
    ViewPort = #view{y=Y, x=X, rows=Rows, cols=Cols},
    Box_ = #box{y=Y, x=X, rows=Rows, cols=Cols},
    Box = box( ViewPort,
               margin(Node, Box_), border(Node, Box_), padding(Node, Box_),
               {Y, X, Rows, Cols} ),
    Content = boxify(Box#box.view, Node#node.content, []),
    Node#node{content=Content, box=Box}.

boxify(_, [], Acc) -> lists:reverse(Acc);
boxify(ViewPort, [CNode | CNodes], Acc) ->
    #view{y=Y_, x=X_, rows=Rows, cols=Cols} = ViewPort,
    Y = case attr(y, CNode) of false -> Y_; Yo -> Yo end,
    X = case attr(x, CNode) of false -> X_; Xo -> Xo end,
    boxify(ViewPort, CNodes, [boxify({Y, X, Rows, Cols}, CNode) | Acc]).
    
box(ViewPort, Margin, Border, Padding, {Y, X, Rows, Cols}) ->
    #margin{top=Mt, right=Mr, bottom=Mb, left=Ml} = Margin,
    #view{y=Vy, x=Vx, rows=VRows, cols=VCols} = ViewPort,
    {Top, BRows} =
        case Y of
            auto -> top(ViewPort, {Mt, Mr, Mb, Ml}, Rows, Cols);
            _ -> {Vy+Y, if (VRows-Y) >= Rows -> Rows; true -> VRows-Y end}
        end,
    {Left, BCols} =
        case X of
            auto -> left(ViewPort, {Mt, Mr, Mb, Ml}, Rows, Cols);
            _ -> {Vx+X, if (VCols-X) >= Cols -> Cols; true -> VCols-X end}
        end,
    Box = #box{y=Top, x=Left, rows=BRows, cols=BCols, margin=Margin,
               border=Border, padding=Padding},
    Box#box{view=view(Box)}.


view(Box) ->
    viewpadding(viewborder(Box), Box#box.padding).


viewborder(#box{y=Y, x=X, rows=Rows, cols=Cols, border=Border}) ->
    {Vy, Vx, Vrows, Vcols} = viewbt(tuple_to_list(Border), {Y,X,Rows,Cols}),
    #view{y=Vy, x=Vx, rows=Vrows, cols=Vcols}.

viewbt([none | Ls], {Y, X, Rs, Cs}) -> viewbr(Ls, {Y, X, Rs, Cs});
viewbt([_ | Ls], {Y, X, Rs, Cs}) -> viewbr( Ls, {Y+1, X, Rs-1, Cs}).

viewbr([none | Ls], {Y, X, Rs, Cs}) -> viewbb(Ls, {Y, X, Rs, Cs});
viewbr([_ | Ls], {Y, X, Rs, Cs}) -> viewbb(Ls, {Y, X, Rs, Cs-1}).

viewbb( [none | Ls], {Y, X, Rs, Cs}) -> viewbl(Ls, {Y, X, Rs, Cs});
viewbb( [_ | Ls], {Y, X, Rs, Cs}) -> viewbl(Ls, {Y, X, Rs-1, Cs}).

viewbl([none | []], {Y, X, Rs, Cs}) -> {Y, X, Rs, Cs};
viewbl([_ | []], {Y, X, Rs, Cs}) -> {Y, X+1, Rs, Cs-1}.


viewpadding(#view{rows=Vrows, cols=Vcols}=View, Padding) ->
    #padding{top=Pt, right=Pr, bottom=Pb, left=Pl} = Padding,
    {Top, VRows} = top(View, {Pt,Pr,Pb,Pl}, Vrows, Vcols),
    {Left, VCols} = left(View, {Pt,Pr,Pb,Pl}, Vrows, Vcols),
    View#view{y=Top, x=Left, rows=VRows, cols=VCols}.


margin(Node, _Box) ->
    Attr = case attr(margin, Node) of
                false -> {0, 0, 0, 0};
                {margin, Value} -> marginto( string:tokens(Value, " "), [] )
           end,
    Node#node{ 
        attributes=lists:keyreplace(margin, 1, Node#node.attributes, Attr)}.

marginto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
marginto(["auto" | Ls], Acc) -> marginto(Ls, [auto | Acc]);
marginto([X | Ls], Acc) -> marginto(Ls, [list_to_integer(X) | Acc]).


border(Node, Box) ->
    B  = case attr(border, Node) of
            false -> {none, none, none, none};
            {border, Value} -> borderto(all, string:tokens(Value, " "), Box)
         end,
    Fn = fun({I, Attr, Pref}, Acc) ->
            case attr(Attr, Node) of
                false -> Acc;
                {Attr, V} ->
                    E = borderto(Pref, string:tokens(V, " "), Box),
                    erlang:setelement(I, Acc, E)
            end
         end,
    LAttrs = [ {1, bordertop, top}, {2, borderright, right},
               {3, borderbottom, bottom}, {4, borderleft, left} ],
    Attr = lists:foldl( Fn, B, LAttrs),
    Node#node{ 
        attributes=lists:keyreplace(border, 1, Node#node.attributes, Attr)}.


borderto(all, ["0", _, _], _) -> {none, none, none, none};
borderto(all, ["1", Char, Color], Box) ->
    #box{y=Y, x=X, rows=Rows, cols=Cols} = Box,
    { {Y,        X,        Cols, list_to_atom(Char), list_to_atom(Color)},
      {Y,        X+Cols-1, Rows, list_to_atom(Char), list_to_atom(Color)},
      {Y+Rows-1, X+Cols-1, Cols, list_to_atom(Char), list_to_atom(Color)},
      {Y+Rows-1, X,        Rows, list_to_atom(Char), list_to_atom(Color)}
    };

borderto(top, ["0", _, _], _) -> none;
borderto(top, ["1", Char, Color], Box) ->
    #box{y=Y, x=X, cols=Cols} = Box,
    {Y, X, Cols, list_to_atom(Char), list_to_atom(Color)};

borderto(right, ["0", _, _], _) -> none;
borderto(right, ["1", Char, Color], Box) ->
    #box{y=Y, x=X, rows=Rows, cols=Cols} = Box,
    {Y, X+Cols-1, Rows, list_to_atom(Char), list_to_atom(Color)};

borderto(bottom, ["0", _, _], _) -> none;
borderto(bottom, ["1", Char, Color], Box) ->
    #box{y=Y, x=X, rows=Rows, cols=Cols} = Box,
    {Y+Rows-1, X+Cols-1, Cols, list_to_atom(Char), list_to_atom(Color)};

borderto(left, ["0", _, _], _) -> none;
borderto(left, ["1", Char, Color], Box) ->
    #box{y=Y, x=X, rows=Rows} = Box,
    {Y+Rows-1, X, Rows, list_to_atom(Char), list_to_atom(Color)}.


padding(Node, _Box) ->
    Attr = case attr(padding, Node) of
            false -> {0, 0, 0, 0};
            {padding, Value} -> paddingto( string:tokens(Value, " "), [] )
           end,
    Node#node{ 
        attributes=lists:keyreplace(padding, 1, Node#node.attributes, Attr)}.

paddingto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
paddingto(["auto" | Ls], Acc) -> paddingto(Ls, [auto | Acc]);
paddingto([X | Ls], Acc) -> paddingto(Ls, [list_to_integer(X) | Acc]).


top(#view{y=0, x=0}, {Top, _, _, _}, _, _) when Top < 0 ->
    {error, "margin cannot be negative for outermost box."};
    % TODO : Log here

top(#view{y=Vy, rows=VRows}, _, Rows, _) when VRows < Rows ->
    {ok, Vy, VRows};

top(#view{y=Vy, rows=VRows}, {auto, _, auto, _}, Rows, _) ->
    {ok, Vy + ((VRows-Rows) div 2), Rows};

top(#view{y=Vy, rows=VRows}, {auto, _, Bottom, _}, Rows, _) ->
    Diff = VRows - Rows - Bottom,
    Top = if Diff < 0 -> Vy; true -> Vy+Diff end,
    {ok, Top, Rows};

top(#view{y=Vy, rows=VRows}, {Top, _, _, _}, Rows, _) ->
    Diff = VRows - Rows,
    BTop = if Diff < Top -> Vy+Diff; true -> Vy+Top end,
    {ok, BTop, Rows}.


left(#view{y=0, x=0}, {_, _, _, Left}, _, _) when Left < 0 ->
    {error, "margin cannot be negative for outermost box."};
    % TODO : Log here

left(#view{x=Vx, cols=VCols}, _, _, Cols) when VCols < Cols ->
    {ok, Vx, VCols};

left(#view{x=Vx, cols=VCols}, {_, auto, _, auto}, _, Cols) ->
    {ok, Vx + ((VCols-Cols) div 2), Cols};

left(#view{x=Vx, cols=VCols}, {_, Right, _, auto}, _, Cols) ->
    Diff = VCols - Cols - Right,
    Left = if Diff < 0 -> Vx; true -> Vx+Diff end,
    {ok, Left, Cols};

left(#view{x=Vx, cols=VCols}, {_, _, _, Left}, _, Cols) ->
    Diff = VCols - Cols,
    BLeft = if Diff < Left -> Vx+Diff; true -> Vx+Left end,
    {ok, BLeft, Cols}.

attr(Name, Node) ->
    lists:keyfind(Name, 1, Node#node.attributes).
