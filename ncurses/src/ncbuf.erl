-module(ncbuf).
-author('prataprc@gmail.com').

% Module API
-export([init_block/2, bordercolor/1, frameborders/2, cornerize/3]).

-include("ncurses.hrl").
-include("ncdom.hrl").


init_block(Rows, Cols) ->
    erlang:list_to_tuple( lists:duplicate(Rows, erlang:make_tuple(Cols, $ ))).


frameborders(#tag{ box=#box{border={Bt, Br, Bb, Bl}} }=Tag, Buf) ->
    frameborders(
        Tag#tag.content,
        frameborder(left, Bl,
            frameborder(bottom, Bb, 
                frameborder(right, Br,
                    frameborder( top, Bt, Buf ))))
    );

frameborders([], Buf) -> Buf;
frameborders([#text{} | Es], Buf) -> frameborders(Es, Buf);
frameborders([#tag{}=Tag | Es], Buf) ->
    frameborders( Es, frameborders( Tag, Buf )).


cornerize(Buf, Rows, Cols) ->
    cornerize(Buf, 1, 1, Rows+1, Cols+1).

cornerize(Buf, Rows, Cols, Rows, Cols) -> Buf;
cornerize(Buf, Yb, Cols, Rows, Cols) -> cornerize(Buf, Yb+1, 1, Rows, Cols);
cornerize(Buf, Yb, Xb, Rows, Cols) ->
    case getchar(Buf, Yb, Xb) of
        $* -> 
            Ch = borderchar( getchar(Buf, Yb-1, Xb), getchar(Buf, Yb, Xb+1),
                             getchar(Buf, Yb+1, Xb), getchar(Buf, Yb, Xb-1) ),
            cornerize(setchar(Buf, {Yb, Xb}, Ch), Yb, Xb+1, Rows, Cols);
        _ ->
            cornerize(Buf, Yb, Xb+1, Rows, Cols)
    end.

frameborder(top, none, Buf) -> Buf;
frameborder(top, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(top, {Y, X, Len, Ch, Cl}, Buf) ->
    Chtype = {?ACS_HLINE, Cl},
    frameborder(
        top, {Y, X+1, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, Chtype) );

frameborder(right, none, Buf) -> Buf;
frameborder(right, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(right, {Y, X, Len, Ch, Cl}, Buf) ->
    Chtype = {?ACS_VLINE, Cl},
    frameborder(
        right, {Y+1, X, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, Chtype) );

frameborder(bottom, none, Buf) -> Buf;
frameborder(bottom, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(bottom, {Y, X, Len, Ch, Cl}, Buf) ->
    Chtype = {?ACS_VLINE, Cl},
    frameborder(
        bottom, {Y, X-1, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, Chtype) );

frameborder(left, none, Buf) -> Buf;
frameborder(left, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(left, {Y, X, Len, Ch, Cl}, Buf) ->
    Chtype = {?ACS_VLINE, Cl},
    frameborder(
        left, {Y-1, X, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, Chtype) ).


getchar(Buf, Y, X) ->
    try erlang:element( X, erlang:element( Y, Buf ))
    catch
        error:_Reason -> none % TODO : Log here.
    end.


setchar(Buf, Y, X, Char) -> 
    case getchar(Buf, Y, X) of
        $  -> setchar(Buf, {Y,X}, Char);
        Char -> Buf;
        $* -> Buf;
        none -> Buf;
        _ -> setchar(Buf, {Y,X}, $*)
    end.

setchar(Buf, {Y,X}, Char) ->
    try erlang:setelement(Y, Buf,
            erlang:setelement(X, erlang:element(Y, Buf), Char))
    catch
        error:_Reason -> Buf % TODO : Log here.
    end.

borderchar({?ACS_VLINE,Cl}, {?ACS_HLINE,_}, {?ACS_VLINE,_}, {?ACS_HLINE,_}) -> 
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_PLUS);
borderchar({?ACS_VLINE,Cl}, {?ACS_HLINE,_}, {?ACS_VLINE,_}, _)-> 
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_LTEE);
borderchar({?ACS_VLINE,Cl}, _, {?ACS_VLINE,_}, {?ACS_HLINE,_}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_RTEE);
borderchar({?ACS_VLINE,Cl}, {?ACS_HLINE,_}, _, {?ACS_HLINE,_}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_BTEE);
borderchar(_, {?ACS_HLINE,Cl}, {?ACS_VLINE,_}, {?ACS_HLINE,_}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_TTEE);
borderchar(_, {?ACS_HLINE,Cl}, {?ACS_VLINE,_}, _) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_ULCORNER);
borderchar(_, _, {?ACS_VLINE,Cl}, {?ACS_HLINE,_}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_URCORNER);
borderchar({?ACS_VLINE,Cl}, _, _, {?ACS_HLINE,_}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_LRCORNER);
borderchar({?ACS_VLINE,Cl}, {?ACS_HLINE,_}, _, _) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_LLCORNER);

borderchar({?ACS_VLINE,Cl}, _, _, _) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_VLINE);
borderchar(_, {?ACS_HLINE,Cl}, _, _) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_HLINE);
borderchar(_, _, {?ACS_VLINE,Cl}, _) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_VLINE);
borderchar(_, _, _, {?ACS_HLINE, Cl}) ->
    ?CH(?A_NORMAL, bordercolor(Cl), ?ACS_HLINE).

bordercolor(br) -> ?COLOR_PAIR(?COLOR_BR);
bordercolor(bg) -> ?COLOR_PAIR(?COLOR_BG);
bordercolor(by) -> ?COLOR_PAIR(?COLOR_BY);
bordercolor(bbl) -> ?COLOR_PAIR(?COLOR_BBl);
bordercolor(bm) -> ?COLOR_PAIR(?COLOR_BM);
bordercolor(bc) -> ?COLOR_PAIR(?COLOR_BC);
bordercolor(bw) -> ?COLOR_PAIR(?COLOR_BW);

bordercolor(rb) -> ?COLOR_PAIR(?COLOR_RB);
bordercolor(rg) -> ?COLOR_PAIR(?COLOR_RG);
bordercolor(ry) -> ?COLOR_PAIR(?COLOR_RY);
bordercolor(rbl) -> ?COLOR_PAIR(?COLOR_RBl);
bordercolor(rm) -> ?COLOR_PAIR(?COLOR_RM);
bordercolor(rc) -> ?COLOR_PAIR(?COLOR_RC);
bordercolor(rw) -> ?COLOR_PAIR(?COLOR_RW);
                               
bordercolor(gb) -> ?COLOR_PAIR(?COLOR_GB);
bordercolor(gr) -> ?COLOR_PAIR(?COLOR_GR);
bordercolor(gy) -> ?COLOR_PAIR(?COLOR_GY);
bordercolor(gbl) -> ?COLOR_PAIR(?COLOR_GBl);
bordercolor(gm) -> ?COLOR_PAIR(?COLOR_GM);
bordercolor(gc) -> ?COLOR_PAIR(?COLOR_GC);
bordercolor(gw) -> ?COLOR_PAIR(?COLOR_GW);

bordercolor(yb) -> ?COLOR_PAIR(?COLOR_YB);
bordercolor(yr) -> ?COLOR_PAIR(?COLOR_YR);
bordercolor(yg) -> ?COLOR_PAIR(?COLOR_YG);
bordercolor(ybl) -> ?COLOR_PAIR(?COLOR_YBl);
bordercolor(ym) -> ?COLOR_PAIR(?COLOR_YM);
bordercolor(yc) -> ?COLOR_PAIR(?COLOR_YC);
bordercolor(yw) -> ?COLOR_PAIR(?COLOR_YW);
                               
bordercolor(blb) -> ?COLOR_PAIR(?COLOR_BlB);
bordercolor(blr) -> ?COLOR_PAIR(?COLOR_BlR);
bordercolor(blg) -> ?COLOR_PAIR(?COLOR_BlG);
bordercolor(bly) -> ?COLOR_PAIR(?COLOR_BlY);
bordercolor(blm) -> ?COLOR_PAIR(?COLOR_BlM);
bordercolor(blc) -> ?COLOR_PAIR(?COLOR_BlC);
bordercolor(blw) -> ?COLOR_PAIR(?COLOR_BlW);
                               
bordercolor(mb) -> ?COLOR_PAIR(?COLOR_MB);
bordercolor(mr) -> ?COLOR_PAIR(?COLOR_MR);
bordercolor(mg) -> ?COLOR_PAIR(?COLOR_MG);
bordercolor(my) -> ?COLOR_PAIR(?COLOR_MY);
bordercolor(mbl) -> ?COLOR_PAIR(?COLOR_MBl);
bordercolor(mc) -> ?COLOR_PAIR(?COLOR_MC);
bordercolor(mw) -> ?COLOR_PAIR(?COLOR_MW);
                               
bordercolor(cb) -> ?COLOR_PAIR(?COLOR_CB);
bordercolor(cr) -> ?COLOR_PAIR(?COLOR_CR);
bordercolor(cg) -> ?COLOR_PAIR(?COLOR_CG);
bordercolor(cy) -> ?COLOR_PAIR(?COLOR_CY);
bordercolor(cm) -> ?COLOR_PAIR(?COLOR_CM);
bordercolor(cbl) -> ?COLOR_PAIR(?COLOR_CBl);
bordercolor(cw) -> ?COLOR_PAIR(?COLOR_CW);
                               
bordercolor(wb) -> ?COLOR_PAIR(?COLOR_WB);
bordercolor(wr) -> ?COLOR_PAIR(?COLOR_WR);
bordercolor(wg) -> ?COLOR_PAIR(?COLOR_WG);
bordercolor(wy) -> ?COLOR_PAIR(?COLOR_WY);
bordercolor(wm) -> ?COLOR_PAIR(?COLOR_WM);
bordercolor(wc) -> ?COLOR_PAIR(?COLOR_WC);
bordercolor(wbl) -> ?COLOR_PAIR(?COLOR_WBl).

