-module(ncdom).
-author('prataprc@gmail.com').

% Module API
-export([doc/3, parsexml/2, boxify/2, textof/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("ncurses.hrl").
-include("ncdom.hrl").

%---- Module API

doc(XPort, DocPath, XmlFile) ->
    RootTag = boxify(XPort, parsexml(DocPath, XmlFile)),
    {ok, _, XRoot} = ncnode:spawndom(RootTag),
    #doc{docpath=DocPath, root=XRoot, focus=ncnode:getfocus(XRoot)}.


% Parse `XmlFile` for textmode-application `Appname` and return the
% root-element.
parsexml(DocPath, XmlFile) -> xmltag( DocPath, root(XmlFile) ).


% First argument describes the view-port of tag Tag.
boxify({Y, X, Rows, Cols}, Tag) ->
    ViewPort = #view{y=Y, x=X, rows=Rows, cols=Cols},

    Yb = case attr(y, Tag) of false -> auto; {y, Y_} -> Y_ end,
    Xb = case attr(x, Tag) of false -> auto; {x, X_} -> X_ end,
    Brows = case attr(rows, Tag) of false -> Rows; {rows, R_} -> R_ end,
    Bcols = case attr(cols, Tag) of false -> Cols; {cols, C_} -> C_ end,
    % Compute box and view dimensions.
    {Box, View} = 
        view( padding(Tag), % Tuple of padding
              borderize(
                    border(Tag),
                    box( ViewPort, margin(Tag), {Yb, Xb, Brows, Bcols} )
              )
            ),

    % Fresh 
    Tag#tag{
        box=Box,
        view=View,
        content=boxify(View, Tag#tag.content, [])
    }.

boxify(_, [], Acc) -> lists:reverse(Acc);
boxify(ViewPort, [#text{}=CTag | CTags], Acc) ->
    boxify(ViewPort, CTags, [CTag | Acc]);
boxify(ViewPort, [CTag | CTags], Acc) ->
    #view{y=Y, x=X, rows=Rows, cols=Cols} = ViewPort,
    boxify(ViewPort, CTags, [boxify({Y, X, Rows, Cols}, CTag) | Acc]).
    

textof([], Acc) -> string:join(lists:reverse(Acc), ' ');
textof([#text{content=Text} | Cs], Acc) ->
    Fn = fun(Char, Str) -> string:strip(Str, both, Char) end,
    textof(Cs, [lists:foldl(Fn, Text, [$ , $\n, $\r] ) | Acc]).


%---- internal functions

root(XmlFile) ->
    case xmerl_scan:file( XmlFile ) of
        {error, _Misc} -> 
            error_logger:error_msg("Unable to parse ~p~n", [XmlFile]);
        {Element, _Misc} ->
            Element
    end.


xmltag(DocPath, E) ->
    Tag = 
        #tag{
            tagname=E#xmlElement.name,
            attributes=xmlattrs(E#xmlElement.attributes, []),
            content=xmlcontent(DocPath, E#xmlElement.content, [])
        },
    Tag#tag{name=ncpath:nodename(DocPath, Tag), docpath=DocPath}.


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
xmlcontent(DocPath, [#xmlText{value=Value} | Cs], Acc) -> 
    xmlcontent(DocPath, Cs, [#text{content=Value} | Acc]);
xmlcontent(DocPath, [#xmlElement{}=E | Cs], Acc) ->
    xmlcontent(DocPath, Cs, [xmltag(DocPath, E) | Acc]).


%-- calculate box dimension
box(ViewPort, {Mt, Mr, Mb, Ml}, {Y, X, Rows, Cols}) ->
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
    #box{y=Top, x=Left, rows=BRows, cols=BCols}.


%-- calculate border dimension
borderize({Bt, Br, Bb, Bl}, Box) ->
    Border = { borderto(top, Bt, Box), borderto(right, Br, Box),
               borderto(bottom, Bb, Box), borderto(left, Bl, Box)},
    Box#box{border=Border}.

borderto(top, {none, none, none, none}, _) -> {none, none, none, none};
borderto(top, {0, _, _}, _) -> none;
borderto(top, {1, Char, Color}, #box{y=Y, x=X, cols=Cols})->
    {Y, X, Cols, Char, Color};

borderto(right, {none, none, none, none}, _) -> {none, none, none, none};
borderto(right, {0, _, _}, _) -> none;
borderto(right, {1, Char, Color}, #box{y=Y, x=X, rows=Rows, cols=Cols})->
    {Y, X+Cols-1, Rows, Char, Color};

borderto(bottom, {none, none, none, none}, _) -> {none, none, none, none};
borderto(bottom, {0, _, _}, _) -> none;
borderto(bottom, {1, Char, Color}, #box{y=Y, x=X, rows=Rows, cols=Cols})->
    {Y+Rows-1, X+Cols-1, Cols, Char, Color};

borderto(left, {none, none, none, none}, _) -> {none, none, none, none};
borderto(left, {0, _, _}, _) -> none;
borderto(left, {1, Char, Color}, #box{y=Y, x=X, rows=Rows})->
    {Y+Rows-1, X, Rows, Char, Color}.

%-- calculate view dimension
view(Padding, Box) ->
    View = viewpadding( viewborder(Box), Padding ),
    {Box, View}.

viewborder(#box{y=Y, x=X, rows=Rows, cols=Cols, border=Border}) ->
    {Vy, Vx, Vrows, Vcols} = viewbt(tuple_to_list(Border), {Y,X,Rows,Cols}),
    #view{y=Vy, x=Vx, rows=Vrows, cols=Vcols}.

viewbt([none | Ls], {Y, X, Rs, Cs}) -> viewbr(Ls, {Y, X, Rs, Cs});
viewbt([_ | Ls], {Y, X, Rs, Cs}) -> viewbr(Ls, {Y+1, X, Rs-1, Cs}).

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


%-- compute dimension
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

%-- normalize tag-attributes
margin(Tag) ->
    case attr(margin, Tag) of
        false -> {0, 0, 0, 0};
        {margin, Value} -> marginto( string:tokens(Value, " "), [] )
    end.

marginto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
marginto(["auto" | Ls], Acc) -> marginto(Ls, [auto | Acc]);
marginto([X | Ls], Acc) -> marginto(Ls, [list_to_integer(X) | Acc]).


border(Tag) ->
    B = borderattr(border, Tag, {none, none, none, none}),
    X = {'border-top', 'border-right', 'border-bottom', 'border-left'},
    Border = {B, B, B, B},
    Fn = fun(I,Br) -> setelement(I, Br, borderattr(element(I,X), Tag, B)) end,
    Fn(4, Fn(3, Fn(2, Fn(1, Border)))).

borderattr(Attr, Tag, Default) ->
    case attr(Attr, Tag) of
        false -> Default;
        {Attr, Value} -> borderval( string:tokens( Value, " " ))
     end.

borderval([Sz, Ch, Cl]) ->
    {list_to_integer(Sz), list_to_atom(Ch), list_to_atom(Cl)}.


padding(Tag) ->
    case attr(padding, Tag) of
        false -> {0, 0, 0, 0};
        {padding, Value} -> paddingto( string:tokens(Value, " "), [] )
    end.

paddingto([], Acc) -> list_to_tuple( lists:reverse( Acc ));
paddingto(["auto" | Ls], Acc) -> paddingto(Ls, [auto | Acc]);
paddingto([X | Ls], Acc) -> paddingto(Ls, [list_to_integer(X) | Acc]).


attr(Name, Tag) ->
    lists:keyfind(Name, 1, Tag#tag.attributes).

