% Only support cbreak mode.

-module(nc_srv).
-behaviour(gen_server).

-include("ncurses.hrl").
-include("ncommands.hrl").

% Behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

% Module API
-export([start_link/0]).

% NCurses API
-export([
    % terminal
    beep/0, flash/0, curses_version/0,
    getyx/0, getyx/1, getbegyx/0, getbegyx/1, getmaxyx/0, getmaxyx/1,
    getparyx/0, getparyx/1, 

    % window 
    refresh/0, refresh/1, wnoutrefresh/1, doupdate/0,
    werase/0,  werase/1, clear/0, clear/1, clrtobot/0, clrtobot/1,
    clrtoeol/0, clrtoeol/1,
    
    % input options
    raw/0, noraw/0, cbreak/0, nocbreak/0, echo/0, noecho/0, keypad/2,
    nodelay/2, halfdelay/1, notimeout/2, timeout/1, wtimeout/2,
    
    % do character, string and line output
    addch/1, addch/2, addch/3, addch/4, echochar/1, echochar/2,
    delch/0, delch/1, delch/2, delch/3,
    addstr/1, addstr/2, addstr/3, addstr/4,
    addnstr/2, addnstr/3, addnstr/4, addnstr/5,
    addchstr/1, addchstr/2, addchstr/3, addchstr/4,
    addchnstr/2, addchnstr/3, addchnstr/4, addchnstr/5,
    deleteln/0, deleteln/1, insdelln/1, insdelln/2, insertln/0, insertln/1,
    insch/1, insch/2, insch/3, insch/4,
    insstr/1, insstr/2, insstr/3, insstr/4,
    insnstr/2, insnstr/3, insnstr/4, insnstr/5,

    % do attribute settings
    color_set/1, color_set/2, attrset/1, attrset/2, attroff/1, attroff/2,
    attron/1, attron/2, attr_get/0, attr_get/1, chgat/3, chgat/4, chgat/5,
    chgat/6,
    has_colors/0, can_change_color/0, start_color/0, init_pair/3,
    color_content/1,

    % do input
    getch/0, ungetch/1, has_key/1, inch/0, inch/1, inch/2, inch/3,
    innstr/1, innstr/2, innstr/3, innstr/4,
    inchnstr/1, inchnstr/2, inchnstr/3, inchnstr/4,

	move/2, curs_set/1,
    nl/0, nonl/0,
	scrollok/2, newwin/4, delwin/1, wmove/3,
	hline/2,
	whline/3, vline/2, wvline/3, border/8, wborder/9, box/3

    %leaveok/0
]).

% Utility functions
-export([ch/2, chattr/1, chcolor/1, init_pairs/0]).

% Records
-record(state, { port, getch, mode }).

%% Module API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

%%---- NCurses API

% terminal routines
beep() ->
    gen_server:call(?MODULE, {call, ?BEEP, undefined}, infinity).

flash() ->
    gen_server:call(?MODULE, {call, ?FLASH, undefined}, infinity).

curses_version() ->
    gen_server:call(?MODULE, {call, ?CURSES_VERSION, undefined}, infinity).

% window refresh
refresh() ->
    refresh(?W_STDSCR).

refresh(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?WREFRESH, Window}, infinity).

wnoutrefresh(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?WNOUTREFRESH, Window}, infinity).

doupdate() ->
    gen_server:call(?MODULE, {call, ?DOUPDATE, undefined}, infinity).

werase() ->
    werase(?W_STDSCR).

werase(Window) ->
    gen_server:call(?MODULE, {call, ?WERASE, Window}, infinity).

clear() ->
    clear(?W_STDSCR).

clear(Window) ->
    gen_server:call(?MODULE, {call, ?WCLEAR, Window}, infinity).

clrtobot() ->
    clrtobot(?W_STDSCR).

clrtobot(Window) ->
    gen_server:call(?MODULE, {call, ?WCLRTOBOT, Window}, infinity).

clrtoeol() ->
    clrtoeol(?W_STDSCR).

clrtoeol(Window) ->
    gen_server:call(?MODULE, {call, ?WCLRTOEOL, Window}, infinity).

getyx() ->
    getyx(?W_STDSCR).

getyx(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?GETYX, Window}, infinity).

getbegyx() ->
    getbegyx(?W_STDSCR).

getbegyx(Window) ->
    gen_server:call(?MODULE, {call, ?GETBEGYX, Window}, infinity).

getmaxyx() ->
    getmaxyx(?W_STDSCR).

getmaxyx(Window) ->
    gen_server:call(?MODULE, {call, ?GETMAXYX, Window}, infinity).


getparyx() ->
    getparyx(?W_STDSCR).

getparyx(Window) ->
    gen_server:call(?MODULE, {call, ?GETPARYX, Window}, infinity).


% input options
raw() -> 
    gen_server:call(?MODULE, {call, ?RAW, undefined}, infinity).

noraw() -> 
    gen_server:call(?MODULE, {call, ?NORAW, undefined}, infinity).

cbreak() -> 
    gen_server:call(?MODULE, {call, ?CBREAK, undefined}, infinity).

nocbreak() -> 
    "Not supported".

echo() -> 
    gen_server:call(?MODULE, {call, ?ECHO, undefined}, infinity).

noecho() -> 
    gen_server:call(?MODULE, {call, ?NOECHO, undefined}, infinity).

keypad(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    gen_server:call(?MODULE, {call, ?KEYPAD, {Window, BFlag}}, infinity).

nodelay(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    gen_server:call(?MODULE, {call, ?NODELAY, {Window, BFlag}}, infinity).

halfdelay(Tenths) when is_integer(Tenths) ->
    gen_server:call(?MODULE, {call, ?HALFDELAY, Tenths}, infinity).

notimeout(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    gen_server:call(?MODULE, {call, ?NOTIMEOUT, {Window, BFlag}}, infinity).

timeout(Delay) when is_integer(Delay) ->
    wtimeout(?W_STDSCR, Delay).

wtimeout(Window, Delay) when is_integer(Window) andalso is_boolean(Delay) ->
    gen_server:call(?MODULE, {call, ?WTIMEOUT, {Window, Delay}}, infinity).


% do character output
addch(Char) when is_integer(Char) ->
    addch(?W_STDSCR, Char).

addch(Window, Char) when is_integer(Window) andalso is_integer(Char) ->
    gen_server:call(?MODULE, {call, ?WADDCH, {Window, Char}}, infinity).

addch(Y, X, Char) 
        when is_integer(Char) andalso is_integer(X) andalso is_integer(Y) ->
    addch(?W_STDSCR, Y, X, Char).

addch(Window, Y, X, Char)
        when is_integer(Window) andalso is_integer(Y) andalso
             is_integer(X) andalso is_integer(Char) ->
    gen_server:call(?MODULE, {call, ?WADDCH, {Window,Y,X,Char}}, infinity).

echochar(Char) when is_integer(Char) ->
    echochar(?W_STDSCR, Char).

echochar(Window, Char) when is_integer(Window) andalso is_integer(Char) ->
    gen_server:call(?MODULE, {call, ?WECHOCHAR, {Window, Char}}, infinity).

delch() ->
    delch(?W_STDSCR).

delch(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?WDELCH, {Window}}, infinity).

delch(Y, X) ->
    delch(?W_STDSCR, Y, X).

delch(Window, Y, X )
        when is_integer(Window) andalso is_integer(Y) andalso is_integer(X) ->
    gen_server:call(?MODULE, {call, ?WDELCH, {Window, Y, X}}, infinity).

deleteln() ->
    deleteln(?W_STDSCR).

deleteln(Window) ->
    gen_server:call(?MODULE, {call, ?WDELETELN, {Window}}, infinity).

insdelln(N) ->
    insdelln(?W_STDSCR, N).

insdelln(Window, N) ->
    gen_server:call(?MODULE, {call, ?WINSDELLN, {Window, N}}, infinity).
    
insertln() ->
    insertln(?W_STDSCR).

insertln(Window) ->
    gen_server:call(?MODULE, {call, ?WINSERTLN, {Window}}, infinity).

insch(Char) when is_integer(Char) ->
    insch(?W_STDSCR, Char).

insch(Window, Char) when is_integer(Window) andalso is_integer(Char) ->
    gen_server:call(?MODULE, {call, ?WINSCH, {Window, Char}}, infinity).

insch(Y, X, Char) 
        when is_integer(Char) andalso is_integer(X) andalso is_integer(Y) ->
    insch(?W_STDSCR, Y, X, Char).

insch(Window, Y, X, Char)
        when is_integer(Window) andalso is_integer(Y) andalso
             is_integer(X) andalso is_integer(Char) ->
    gen_server:call(?MODULE, {call, ?WINSCH, {Window, Y, X, Char}}, infinity).

% do string output
addstr(String) ->
    addstr(?W_STDSCR, String).

addstr(Window, String) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    addnstr_(Window, Str, erlang:length(Str)).

addstr(Y, X, String)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    addstr(?W_STDSCR, Y, X, String).

addstr(Window, Y, X, String)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    addnstr_(Window, Y, X, Str, erlang:length(Str)).

%% In the following four APIs N should not be less than the size of String
%% list. It is better to use the addstr() variant.
addnstr(String, N) ->
    addnstr(?W_STDSCR, String, N).

addnstr(Window, String, N) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    addnstr_(Window, Str, N).

addnstr(Y, X, String, N)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    addnstr(?W_STDSCR, Y, X, String, N).

addnstr(Window, Y, X, String, N)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    addnstr_(Window, Y, X, Str, N).

addnstr_(Window, Str, N) ->                 % local method
    gen_server:call(?MODULE, {call, ?WADDNSTR, {Window, N, Str}}, infinity).

addnstr_(Window, Y, X, Str, N) ->           % local method
    Args = {Window, Y, X, N, Str},
    gen_server:call(?MODULE, {call, ?WADDNSTR, Args}, infinity).

%% This can send both string terms and list terms based on the chtype values.
addchstr(String) when is_list(String) ->
    addchstr(?W_STDSCR, String).

addchstr(Window, String) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    addchnstr_(Window, Str, erlang:length(Str)).

addchstr(Y, X, String)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    addchstr(?W_STDSCR, Y, X, String).

addchstr(Window, Y, X, String)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    addchnstr_(Window, Y, X, Str, erlang:length(Str)).

addchnstr(String, N) when is_list(String) ->
    addchnstr(?W_STDSCR, String, N).

addchnstr(Window, String, N) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    addchnstr_(Window, Str, N).

addchnstr(Y, X, String, N)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    addchnstr(?W_STDSCR, Y, X, String, N).

addchnstr(Window, Y, X, String, N)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    addchnstr_(Window, Y, X, Str, N).

addchnstr_(Window, Str, N) ->                 % local method
    gen_server:call(?MODULE, {call, ?WADDCHNSTR, {Window, N, Str}}, infinity).

addchnstr_(Window, Y, X, Str, N) ->           % local method
    Args = {Window, Y, X, N, Str},
    gen_server:call(?MODULE, {call, ?WADDCHNSTR, Args}, infinity).

insstr(String) when is_list(String) ->
    insstr(?W_STDSCR, String).

insstr(Window, String) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    insnstr_(Window, Str, erlang:length(Str)).

insstr(Y, X, String)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    insstr(?W_STDSCR, Y, X, String).

insstr(Window, Y, X, String)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    insnstr_(Window, Y, X, Str, erlang:length(Str)).

insnstr(String, N) when is_list(String) ->
    insnstr(?W_STDSCR, String, N).

insnstr(Window, String, N) when is_integer(Window) andalso is_list(String) ->
    Str = lists:flatten(String),
    insnstr_(Window, Str, N).

insnstr(Y, X, String, N)
        when is_list(String) andalso is_integer(X) andalso is_integer(Y) ->
    insnstr(?W_STDSCR, Y, X, String, N).

insnstr(Window, Y, X, String, N)
        when is_integer(Window) andalso is_integer(Y)
        andalso is_integer(X) andalso is_list(String) ->
    Str = lists:flatten(String),
    insnstr_(Window, Y, X, Str, N).

insnstr_(Window, Str, N) ->                 % local method
    gen_server:call(?MODULE, {call, ?WINSNSTR, {Window, N, Str}}, infinity).

insnstr_(Window, Y, X, Str, N) ->           % local method
    Args = {Window, Y, X, N, Str},
    gen_server:call(?MODULE, {call, ?WINSNSTR, Args}, infinity).


% do attribute settings
color_set(CPair) when is_integer(CPair) -> 
    color_set(?W_STDSCR, CPair).

color_set(Window, CPair) when is_integer(Window) andalso is_integer(CPair) ->
    gen_server:call(?MODULE, {call, ?COLOR_SET, {Window, CPair}}, infinity).

attrset(Attr) when is_integer(Attr) ->
    attrset(?W_STDSCR, Attr).

attrset(Window, Attr) when is_integer(Attr) andalso is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?ATTRSET, {Window, Attr}}, infinity).

attroff(Mask) ->
    attroff(?W_STDSCR, Mask).

attroff(Window, Mask) when is_integer(Mask) andalso is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?ATTROFF, {Window, Mask}}, infinity).

attron(Mask) ->
    attron(?W_STDSCR, Mask).

attron(Window, Mask) when is_integer(Mask) andalso is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?ATTRON, {Window, Mask}}, infinity).

attr_get() ->
    attr_get(?W_STDSCR).

attr_get(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?ATTR_GET, {Window}}, infinity).

chgat(N, Attr, CPair)
        when is_integer(N) andalso is_integer(Attr) andalso is_integer(CPair)->
    chgat(?W_STDSCR, N, Attr, CPair).

chgat(Window, N, Attr, CPair)
        when is_integer(Window) andalso is_integer(N) andalso 
             is_integer(Attr) andalso is_integer(CPair)->
    Args = {Window, N, Attr, CPair},
    gen_server:call(?MODULE, {call, ?CHGAT, Args}, infinity).

chgat(Y, X, N, Attr, CPair)
        when is_integer(Y) andalso is_integer(X) andalso 
             is_integer(N) andalso is_integer(Attr) andalso is_integer(CPair)->
    chgat(?W_STDSCR, Y, X, N, Attr, CPair).

chgat(Window, Y, X, N, Attr, CPair)
        when is_integer(Window) andalso is_integer(Y) andalso 
             is_integer(X) andalso is_integer(N) andalso is_integer(Attr) andalso
             is_integer(CPair)->
    Args = {Window, Y, X, N, Attr, CPair},
    gen_server:call(?MODULE, {call, ?CHGAT, Args}, infinity).

% do color settings
has_colors() ->
    gen_server:call(?MODULE, {call, ?HAS_COLORS, undefined}, infinity).

can_change_color() ->
    gen_server:call(?MODULE, {call, ?CAN_CHANGE_COLOR, undefined}, infinity).

start_color() ->
    gen_server:call(?MODULE, {call, ?START_COLOR, undefined}, infinity).

init_pair(N, FColor, BColor) when is_integer(N) andalso is_integer(FColor)
				  andalso is_integer(BColor) ->
    Args = {N, FColor, BColor},
    gen_server:call(?MODULE, {call, ?INIT_PAIR, Args}, infinity).

color_content(N) when is_integer(N) ->
    gen_server:call(?MODULE, {call, ?COLOR_CONTENT, N}, infinity).


% do inputs
getch() ->
    gen_server:call(?MODULE, getch, infinity).

ungetch(Char) ->
    gen_server:call(?MODULE, {call, ?UNGETCH, Char}, infinity).

has_key(Char) ->
    gen_server:call(?MODULE, {call, ?HAS_KEY, Char}, infinity).

inch() ->
    inch(?W_STDSCR).

inch(Window) ->
    gen_server:call(?MODULE, {call, ?INCH, {Window}}, infinity).

inch(Y, X) ->
    inch(?W_STDSCR, Y, X).

inch(Window, Y, X) ->
    gen_server:call(?MODULE, {call, ?INCH, {Window, Y, X}}, infinity).

innstr(N) ->
    innstr(?W_STDSCR, N).

innstr(Window, N) ->
    gen_server:call(?MODULE, {call, ?INNSTR, {Window, N}}, infinity).

innstr(Y, X, N) ->
    innstr(?W_STDSCR, Y, X, N).

innstr(Window, Y, X, N) ->
    gen_server:call(?MODULE, {call, ?INNSTR, {Window, Y, X, N}}, infinity).

inchnstr(N) ->
    inchnstr(?W_STDSCR, N).

inchnstr(Window, N) ->
    gen_server:call(?MODULE, {call, ?INCHNSTR, {Window, N}}, infinity).

inchnstr(Y, X, N) ->
    inchnstr(?W_STDSCR, Y, X, N).

inchnstr(Window, Y, X, N) ->
    gen_server:call(?MODULE, {call, ?INCHNSTR, {Window, Y, X, N}}, infinity).



move(Y, X) when is_integer(X) andalso is_integer(Y) ->
    gen_server:call(?MODULE, {call, ?MOVE, {Y,X}}, infinity).

curs_set(Flag) when is_integer(Flag) ->
    gen_server:call(?MODULE, {call, ?CURS_SET, Flag}, infinity).

nl() ->
    gen_server:call(?MODULE, {call, ?NL, undefined}, infinity).

nonl() ->
    gen_server:call(?MODULE, {call, ?NONL, undefined}, infinity).

scrollok(Window, BFlag) when is_integer(Window) andalso is_boolean(BFlag) ->
    gen_server:call(?MODULE, {call, ?SCROLLOK, {Window, BFlag}}, infinity).

newwin(Height, Width, StartY, StartX) when is_integer(Height) andalso 
					   is_integer(Width) andalso 
					   is_integer(StartY) andalso
					   is_integer(StartX) ->
    Args = {Height, Width, StartY, StartX},
    gen_server:call(?MODULE, {call, ?NEWWIN, Args}, infinity).

delwin(Window) when is_integer(Window) ->
    gen_server:call(?MODULE, {call, ?DELWIN, Window}, infinity).

wmove(Window, Y, X) when is_integer(Window) andalso is_integer(Y) andalso 
			 is_integer(X) ->
    gen_server:call(?MODULE, {call, ?WMOVE, {Window, X, Y}}, infinity).

hline(Char, MaxN) ->
    whline(?W_STDSCR, Char, MaxN).

whline(Window, Char, MaxN) when is_integer(Window) andalso is_integer(MaxN) ->
    gen_server:call(?MODULE, {call, ?WHLINE, {Window, Char, MaxN}}, infinity).

vline(Char, MaxN) ->
    wvline(?W_STDSCR, Char, MaxN).

wvline(Window, Char, MaxN) when is_integer(Window) andalso is_integer(MaxN) ->
    gen_server:call(?MODULE, {call, ?WVLINE, {Window, Char, MaxN}}, infinity).

border(Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) ->
    wborder(0, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs).

wborder(Window, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) 
  when is_integer(Ls) andalso is_integer(Rs) andalso 
       is_integer(Ts) andalso is_integer(Bs) andalso 
       is_integer(TLs) andalso is_integer(TRs) andalso 
       is_integer(BLs) andalso is_integer(BRs) ->
    Args = {Window, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs},
    gen_server:call(?MODULE, {call, ?WBORDER, Args}, infinity).

box(Window, Vert, Horz) when is_integer(Window) andalso is_integer(Vert) andalso
			     is_integer(Horz) ->
    gen_server:call(?MODULE, {call, ?BOX, {Window, Vert, Horz}}, infinity).


% Utility functions
ch(Attr, Color) ->
    fun(Char) -> Attr bor Color bor Char end.

chattr(Attr) ->
    fun(Char) -> Attr bor Char end.

chcolor(Color) ->
    fun(Char)-> Color bor Char end.

init_pairs([]) -> ok;
init_pairs([{N, Fg, Bg} | CPairs]) -> 
    init_pair(N, Fg, Bg),
    init_pairs(CPairs).

init_pairs() ->
    case nc_srv:has_colors() of
        true -> 
            init_pairs([
              {?COLOR_BR,  ?COLOR_BLACK, ?COLOR_RED},
              {?COLOR_BG,  ?COLOR_BLACK, ?COLOR_GREEN},
              {?COLOR_BY,  ?COLOR_BLACK, ?COLOR_YELLOW},
              {?COLOR_BBl, ?COLOR_BLACK, ?COLOR_BLUE},
              {?COLOR_BM,  ?COLOR_BLACK, ?COLOR_MAGENTA},
              {?COLOR_BC,  ?COLOR_BLACK, ?COLOR_CYAN},
              {?COLOR_BW,  ?COLOR_BLACK, ?COLOR_WHITE},
              
              {?COLOR_RB,  ?COLOR_RED, ?COLOR_BLACK},
              {?COLOR_RG,  ?COLOR_RED, ?COLOR_GREEN},
              {?COLOR_RY,  ?COLOR_RED, ?COLOR_YELLOW},
              {?COLOR_RBl, ?COLOR_RED, ?COLOR_BLUE},
              {?COLOR_RM,  ?COLOR_RED, ?COLOR_MAGENTA},
              {?COLOR_RC,  ?COLOR_RED, ?COLOR_CYAN},
              {?COLOR_RW,  ?COLOR_RED, ?COLOR_WHITE},
              
              {?COLOR_GB,  ?COLOR_GREEN, ?COLOR_BLACK},
              {?COLOR_GR,  ?COLOR_GREEN, ?COLOR_RED},
              {?COLOR_GY,  ?COLOR_GREEN, ?COLOR_YELLOW},
              {?COLOR_GBl, ?COLOR_GREEN, ?COLOR_BLUE},
              {?COLOR_GM,  ?COLOR_GREEN, ?COLOR_MAGENTA},
              {?COLOR_GC,  ?COLOR_GREEN, ?COLOR_CYAN},
              {?COLOR_GW,  ?COLOR_GREEN, ?COLOR_WHITE},
              
              {?COLOR_YB,  ?COLOR_YELLOW, ?COLOR_BLACK},
              {?COLOR_YR,  ?COLOR_YELLOW, ?COLOR_RED},
              {?COLOR_YG,  ?COLOR_YELLOW, ?COLOR_GREEN},
              {?COLOR_YBl, ?COLOR_YELLOW, ?COLOR_BLUE},
              {?COLOR_YM,  ?COLOR_YELLOW, ?COLOR_MAGENTA},
              {?COLOR_YC,  ?COLOR_YELLOW, ?COLOR_CYAN},
              {?COLOR_YW,  ?COLOR_YELLOW, ?COLOR_WHITE},
              
              {?COLOR_BlB, ?COLOR_BLUE, ?COLOR_BLACK},
              {?COLOR_BlR, ?COLOR_BLUE, ?COLOR_RED},
              {?COLOR_BlG, ?COLOR_BLUE, ?COLOR_GREEN},
              {?COLOR_BlY, ?COLOR_BLUE, ?COLOR_YELLOW},
              {?COLOR_BlM, ?COLOR_BLUE, ?COLOR_MAGENTA},
              {?COLOR_BlC, ?COLOR_BLUE, ?COLOR_CYAN},
              {?COLOR_BlW, ?COLOR_BLUE, ?COLOR_WHITE},
              
              {?COLOR_MB,  ?COLOR_MAGENTA, ?COLOR_BLACK},
              {?COLOR_MR,  ?COLOR_MAGENTA, ?COLOR_RED},
              {?COLOR_MG,  ?COLOR_MAGENTA, ?COLOR_GREEN},
              {?COLOR_MY,  ?COLOR_MAGENTA, ?COLOR_YELLOW},
              {?COLOR_MBl, ?COLOR_MAGENTA, ?COLOR_BLUE},
              {?COLOR_MC,  ?COLOR_MAGENTA, ?COLOR_CYAN},
              {?COLOR_MW,  ?COLOR_MAGENTA, ?COLOR_WHITE},
              
              {?COLOR_CB,  ?COLOR_CYAN, ?COLOR_BLACK},
              {?COLOR_CR,  ?COLOR_CYAN, ?COLOR_RED},
              {?COLOR_CG,  ?COLOR_CYAN, ?COLOR_GREEN},
              {?COLOR_CY,  ?COLOR_CYAN, ?COLOR_YELLOW},
              {?COLOR_CBl, ?COLOR_CYAN, ?COLOR_BLUE},
              {?COLOR_CM,  ?COLOR_CYAN, ?COLOR_MAGENTA},
              {?COLOR_CW,  ?COLOR_CYAN, ?COLOR_WHITE},
              
              {?COLOR_WB,  ?COLOR_WHITE, ?COLOR_BLACK},
              {?COLOR_WR,  ?COLOR_WHITE, ?COLOR_RED},
              {?COLOR_WG,  ?COLOR_WHITE, ?COLOR_GREEN},
              {?COLOR_WY,  ?COLOR_WHITE, ?COLOR_YELLOW},
              {?COLOR_WBl, ?COLOR_WHITE, ?COLOR_BLUE},
              {?COLOR_WM,  ?COLOR_WHITE, ?COLOR_MAGENTA},
              {?COLOR_WC,  ?COLOR_WHITE, ?COLOR_CYAN}
            ]),
            true;
        false ->
            false
    end.


% Behaviour Callbacks
init(no_args) ->
    process_flag(trap_exit, true),
    case load_driver() of
	ok ->
	    Port = erlang:open_port({spawn, "ncdrv"}, [binary]),
	    ok = do_call(Port, ?INITSCR),
	    ok = do_call(Port, ?START_COLOR),
	    ok = do_call(Port, ?WERASE, 0),
	    ok = do_call(Port, ?WREFRESH, 0),
	    {ok, #state{ port = Port }};
	{error, ErrorCode} ->
	    exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.

handle_call({call, Cmd, Args}, _From, State) ->
    {reply, do_call(State#state.port, Cmd, Args), State};
handle_call(getch,             From,  #state{ getch=undefined }=State) ->
    {noreply, State#state{ getch = From }};
handle_call(getch,             _From, State) ->
    {reply, -1, State}.

terminate(_Reason, State) ->
    do_call(State#state.port, ?ENDWIN),
    do_call(State#state.port, ?CURS_SET, ?CURS_NORMAL),
    erlang:port_close(State#state.port),
    erl_ddll:unload("ncdrv").

handle_info({_Port, {data, _Binary}}, #state{ getch = undefined } = State) ->
    {noreply, State};
handle_info({_Port, {data, Binary}}, State) ->
    gen_server:reply(State#state.getch, binary_to_term(Binary)),
    {noreply, State#state{ getch = undefined }}.

%% @hidden
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {noreply, State}.

%%-- Internal Functions

do_call(Port, Cmd) ->
    do_call(Port, Cmd, undefined).

do_call(Port, Cmd, Args) ->
    binary_to_term(erlang:port_control(Port, Cmd, term_to_binary(Args))).

load_driver() ->
    Dir = case code:priv_dir(ncurses) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "ncdrv").

