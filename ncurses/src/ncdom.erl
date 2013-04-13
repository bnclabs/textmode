-module(ncdom).
-author('prataprc@gmail.com').

% Module API
-export([pagefile/2, boxify/2, textof/2]).

-include("ncdom.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%---- Module API

pagefile(Appname, XmlFile) ->
    Node =
        case xmerl_scan:file( XmlFile ) of
            {error, Misc} -> erlang:display(Misc); % TODO : log error
            {Node_, _Misc} -> Node_
        end,
    PagePath = filename:join(["ncapp://",Appname,filename:basename(XmlFile)]),
    xmltag(PagePath, Node).


% First argument describes the view-port of containing node of Node.
boxify({Y, X, Rows, Cols}, NodeP) ->
    ViewPort = #view{y=Y, x=X, rows=Rows, cols=Cols},

    Yb = case attr(y, NodeP) of false -> auto; {y, Y_} -> Y_ end,
    Xb = case attr(x, NodeP) of false -> auto; {x, X_} -> X_ end,
    Brows = case attr(rows, NodeP) of false -> Rows; {rows, R_} -> R_ end,
    Bcols = case attr(cols, NodeP) of false -> Cols; {cols, C_} -> C_ end,

    Box_ = #box{y=Yb, x=Xb, rows=Brows, cols=Bcols},
    Node = padding( border( margin( NodeP#node{box=Box_} ))),
    Node1 = box(ViewPort, Node, {Yb, Xb, Brows, Bcols}),
    Content = boxify(Node1#node.box#box.view, Node#node.content, []),
    Node1#node{content=Content}.


textof([], Acc) -> string:join(lists:reverse(Acc), ' ');
textof([#text{content=Text} | Cs], Acc) ->
    Fn = fun(Char, Str) -> string:strip(Str, both, Char) end,
    textof(Cs, [lists:foldl(Fn, Text, [$ , $\n, $\r] ) | Acc]).


%---- internal functions
xmltag(PagePath, E) ->
    Node = 
        #node{
            tag=E#xmlElement.name,
            attributes=xmlattrs(E#xmlElement.attributes, []),
            content=xmlcontent(PagePath, E#xmlElement.content, [])
        },
    Node#node{name=nodename(PagePath, Node)}.


xmlattrs([], Acc) -> lists:reverse(Acc);
xmlattrs([#xmlAttribute{name=Name, value=Value} | As], Acc) ->
    Val =
        case Name of
            y -> list_to_integer(Value);
            x -> list_to_integer(Value);
            rows -> list_to_integer(Value);
            cols -> list_to_integer(Value);
            _ -> Value
        end,
    xmlattrs(As, [{Name, Val} | Acc]).


xmlcontent(_, [], Acc) -> lists:reverse(Acc);
xmlcontent(PagePath, [#xmlText{value=Value} | Cs], Acc) -> 
    xmlcontent(PagePath, Cs, [#text{content=Value} | Acc]);
xmlcontent(PagePath, [#xmlElement{}=E | Cs], Acc) ->
    xmlcontent(PagePath, Cs, [xmltag(PagePath, E) | Acc]).


nodename(PagePath, Node) ->
    case attr(id, Node) of
        false -> PagePath; % TODO: log error
        {id, Id} -> filename:join([PagePath, Id])
    end.


boxify(_, [], Acc) -> lists:reverse(Acc);
boxify(ViewPort, [#text{}=CNode | CNodes], Acc) ->
    boxify(ViewPort, CNodes, [CNode | Acc]);
boxify(ViewPort, [CNode | CNodes], Acc) ->
    #view{y=Y, x=X, rows=Rows, cols=Cols} = ViewPort,
    boxify(ViewPort, CNodes, [boxify({Y, X, Rows, Cols}, CNode) | Acc]).
    
box(ViewPort, Node, {Y, X, Rows, Cols}) ->
    #box{margin=Margin, border=Border, padding=Padding} = Node#node.box,
    {Mt, Mr, Mb, Ml} = Margin,
    #view{y=Vy, x=Vx, rows=VRows, cols=VCols} = ViewPort,
    {ok, Top, BRows} =
        case Y of
            auto -> top(ViewPort, {Mt, Mr, Mb, Ml}, Rows, Cols);
            _ -> {ok, Vy+Y, if (VRows-Y) >= Rows -> Rows; true -> VRows-Y end}
        end,
    {ok, Left, BCols} =
        case X of
            auto -> left(ViewPort, {Mt, Mr, Mb, Ml}, Rows, Cols);
            _ -> {ok, Vx+X, if (VCols-X) >= Cols -> Cols; true -> VCols-X end}
        end,
    Box = #box{y=Top, x=Left, rows=BRows, cols=BCols, margin=Margin,
               border=Border, padding=Padding},
    view( borderize(Node#node{box=Box}) ).


view(Node) ->
    Box = Node#node.box,
    View = viewpadding( viewborder(Box), Box#box.padding ),
    Node#node{ box=Box#box{view=View} }.


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
    {ok, Top, VRows} = top(View, Padding, Vrows, Vcols),
    {ok, Left, VCols} = left(View, Padding, Vrows, Vcols),
    View#view{y=Top, x=Left, rows=VRows, cols=VCols}.


margin(Node) ->
    Val  = case attr(margin, Node) of
                false -> {0, 0, 0, 0};
                {margin, Value} -> marginto( string:tokens(Value, " "), [] )
           end,
    Node#node{ box=Node#node.box#box{margin=Val} }.

marginto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
marginto(["auto" | Ls], Acc) -> marginto(Ls, [auto | Acc]);
marginto([X | Ls], Acc) -> marginto(Ls, [list_to_integer(X) | Acc]).


border(Node) ->
    B  = case attr(border, Node) of
            false ->
                {none, none, none, none};
            {border, Value} ->
                BB = borderval( string:tokens( Value, " " )),
                {BB, BB, BB, BB}
         end,
    Fn = fun({I, Attr}, Acc) ->
            case attr(Attr, Node) of
                false -> Acc;
                {Attr, V} ->
                    erlang:setelement(
                        I, Acc, borderval( string:tokens( V, " " )))
            end
         end,
    LAttrs = [ {1, 'border-top'}, {2, 'border-right'},
               {3, 'border-bottom'}, {4, 'border-left'} ],
    Attr = {border, lists:foldl( Fn, B, LAttrs)},
    Node#node{ 
        attributes=lists:keystore(border, 1, Node#node.attributes, Attr)}.

borderval([Sz, Ch, Cl]) ->
    {list_to_integer(Sz), list_to_atom(Ch), list_to_atom(Cl)}.

borderize(#node{box=Box}=Node) ->
    {border, {Tb, Rb, Bb, Lb}} = attr(border, Node),
    Border = { borderto(top, Tb, Box), borderto(right, Rb, Box),
               borderto(bottom, Bb, Box), borderto(left, Lb, Box)},
    Node#node{ box=Node#node.box#box{border=Border} }.


borderto(top, none, _) -> none;
borderto(top, {0, _, _}, _) -> none;
borderto(top, {1, Char, Color}, #box{y=Y, x=X, cols=Cols})->
    {Y, X, Cols, Char, Color};

borderto(right, none, _) -> none;
borderto(right, {0, _, _}, _) -> none;
borderto(right, {1, Char, Color}, #box{y=Y, x=X, rows=Rows, cols=Cols})->
    {Y, X+Cols-1, Rows, Char, Color};

borderto(bottom, none, _) -> none;
borderto(bottom, {0, _, _}, _) -> none;
borderto(bottom, {1, Char, Color}, #box{y=Y, x=X, rows=Rows, cols=Cols})->
    {Y+Rows-1, X+Cols-1, Cols, Char, Color};

borderto(left, none, _) -> none;
borderto(left, {0, _, _}, _) -> none;
borderto(left, {1, Char, Color}, #box{y=Y, x=X, rows=Rows})->
    {Y+Rows-1, X, Rows, Char, Color}.


padding(Node) ->
    Val  = case attr(padding, Node) of
            false -> {0, 0, 0, 0};
            {padding, Value} -> paddingto( string:tokens(Value, " "), [] )
           end,
    Node#node{ box=Node#node.box#box{padding=Val} }.

paddingto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
paddingto(["auto" | Ls], Acc) -> paddingto(Ls, [auto | Acc]);
paddingto([X | Ls], Acc) -> paddingto(Ls, [list_to_integer(X) | Acc]).


top(#view{y=0, x=0}, {Top, _, _, _}, _, _) when Top < 0 ->
    {error, "margin cannot be negative for outermost box, top."};
    % TODO : Log here

top(#view{y=Vy, rows=VRows}, _, Rows, _) when VRows < Rows ->
    {ok, Vy, VRows};

top(#view{y=Vy, rows=VRows}, {auto, _, auto, _}, Rows, _) ->
    {ok, Vy + ((VRows-Rows) div 2), Rows};

top(#view{y=Vy, rows=VRows}, {auto, _, Bottom, _}, Rows, _) ->
    Diff = VRows - Rows - Bottom,
    Top = if Diff < 0 -> Vy; true -> Vy+Diff end,
    {ok, Top, Rows};

top(#view{y=Vy}, {Top, _, _, _}, Rows, _) ->
    {ok, Vy+Top, Rows}.


left(#view{y=0, x=0}, {_, _, _, Left}, _, _) when Left < 0 ->
    {error, "margin cannot be negative for outermost box, left."};
    % TODO : Log here

left(#view{x=Vx, cols=VCols}, _, _, Cols) when VCols < Cols ->
    {ok, Vx, VCols};

left(#view{x=Vx, cols=VCols}, {_, auto, _, auto}, _, Cols) ->
    {ok, Vx + ((VCols-Cols) div 2), Cols};

left(#view{x=Vx, cols=VCols}, {_, Right, _, auto}, _, Cols) ->
    Diff = VCols - Cols - Right,
    Left = if Diff < 0 -> Vx; true -> Vx+Diff end,
    {ok, Left, Cols};

left(#view{x=Vx}, {_, _, _, Left}, _, Cols) ->
    {ok, Vx+Left, Cols}.

attr(Name, Node) ->
    lists:keyfind(Name, 1, Node#node.attributes).

