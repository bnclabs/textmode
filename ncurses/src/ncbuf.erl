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

frameborder(top, {none, none, none, none}, Buf) -> Buf;
frameborder(top, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(top, {Y, X, Len, Ch, Cl}, Buf) ->
    frameborder(
        top, {Y, X+1, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, ?ACS_HLINE) );

frameborder(right, {none, none, none, none}, Buf) -> Buf;
frameborder(right, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(right, {Y, X, Len, Ch, Cl}, Buf) ->
    frameborder(
        right, {Y+1, X, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, ?ACS_VLINE) );

frameborder(bottom, {none, none, none, none}, Buf) -> Buf;
frameborder(bottom, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(bottom, {Y, X, Len, Ch, Cl}, Buf) ->
    frameborder(
        bottom, {Y, X-1, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, ?ACS_HLINE) );

frameborder(left, {none, none, none, none}, Buf) -> Buf;
frameborder(left, {_, _, 0, _, _}, Buf) -> Buf;
frameborder(left, {Y, X, Len, Ch, Cl}, Buf) ->
    frameborder(
        left, {Y-1, X, Len-1, Ch, Cl}, setchar(Buf, Y+1, X+1, ?ACS_VLINE) ).


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

borderchar(?ACS_VLINE, ?ACS_HLINE, ?ACS_VLINE, ?ACS_HLINE) -> ?ACS_PLUS;
borderchar(?ACS_VLINE, ?ACS_HLINE, ?ACS_VLINE, _)-> ?ACS_LTEE;
borderchar(?ACS_VLINE, _, ?ACS_VLINE, ?ACS_HLINE) -> ?ACS_RTEE;
borderchar(?ACS_VLINE, ?ACS_HLINE, _, ?ACS_HLINE) -> ?ACS_BTEE;
borderchar(_, ?ACS_HLINE, ?ACS_VLINE, ?ACS_HLINE) -> ?ACS_TTEE;
borderchar(_, ?ACS_HLINE, ?ACS_VLINE, _) -> ?ACS_ULCORNER;
borderchar(_, _, ?ACS_VLINE, ?ACS_HLINE) -> ?ACS_URCORNER;
borderchar(?ACS_VLINE, _, _, ?ACS_HLINE) -> ?ACS_LRCORNER;
borderchar(?ACS_VLINE, ?ACS_HLINE, _, _) -> ?ACS_LLCORNER.

bordercolor(br) -> ?COLOR_BR;
bordercolor(bg) -> ?COLOR_BG;
bordercolor(by) -> ?COLOR_BY;
bordercolor(bbl) -> ?COLOR_BBl;
bordercolor(bm) -> ?COLOR_BM;
bordercolor(bc) -> ?COLOR_BC;
bordercolor(bw) -> ?COLOR_BW;

bordercolor(rb) -> ?COLOR_RB;
bordercolor(rg) -> ?COLOR_RG;
bordercolor(ry) -> ?COLOR_RY;
bordercolor(rbl) -> ?COLOR_RBl;
bordercolor(rm) -> ?COLOR_RM;
bordercolor(rc) -> ?COLOR_RC;
bordercolor(rw) -> ?COLOR_RW;
                               
bordercolor(gb) -> ?COLOR_GB;
bordercolor(gr) -> ?COLOR_GR;
bordercolor(gy) -> ?COLOR_GY;
bordercolor(gbl) -> ?COLOR_GBl;
bordercolor(gm) -> ?COLOR_GM;
bordercolor(gc) -> ?COLOR_GC;
bordercolor(gw) -> ?COLOR_GW;

bordercolor(yb) -> ?COLOR_YB;
bordercolor(yr) -> ?COLOR_YR;
bordercolor(yg) -> ?COLOR_YG;
bordercolor(ybl) -> ?COLOR_YBl;
bordercolor(ym) -> ?COLOR_YM;
bordercolor(yc) -> ?COLOR_YC;
bordercolor(yw) -> ?COLOR_YW;
                               
bordercolor(blb) -> ?COLOR_BlB;
bordercolor(blr) -> ?COLOR_BlR;
bordercolor(blg) -> ?COLOR_BlG;
bordercolor(bly) -> ?COLOR_BlY;
bordercolor(blm) -> ?COLOR_BlM;
bordercolor(blc) -> ?COLOR_BlC;
bordercolor(blw) -> ?COLOR_BlW;
                               
bordercolor(mb) -> ?COLOR_MB;
bordercolor(mr) -> ?COLOR_MR;
bordercolor(mg) -> ?COLOR_MG;
bordercolor(my) -> ?COLOR_MY;
bordercolor(mbl) -> ?COLOR_MBl;
bordercolor(mc) -> ?COLOR_MC;
bordercolor(mw) -> ?COLOR_MW;
                               
bordercolor(cb) -> ?COLOR_CB;
bordercolor(cr) -> ?COLOR_CR;
bordercolor(cg) -> ?COLOR_CG;
bordercolor(cy) -> ?COLOR_CY;
bordercolor(cbl) -> ?COLOR_CBl;
bordercolor(cm) -> ?COLOR_CM;
bordercolor(cw) -> ?COLOR_CW;
                               
bordercolor(wb) -> ?COLOR_WB;
bordercolor(wr) -> ?COLOR_WR;
bordercolor(wg) -> ?COLOR_WG;
bordercolor(wy) -> ?COLOR_WY;
bordercolor(wbl) -> ?COLOR_WBl;
bordercolor(wm) -> ?COLOR_WM;
bordercolor(wc) -> ?COLOR_WC.
