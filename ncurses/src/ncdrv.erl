% Only support cbreak mode.

-module(ncdrv).
-author('prataprc@gmail.com').
-behaviour(gen_server).

% Behaviour Callbacks
-export([
     init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3
]).

% Module API
-export([start_link/1, win/0, win/1, wdom/0, wdom/1, app/0, app/1, 
         trapexit/0]).

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

    % Window functions
    newwin/4, delwin/1,
    move/2, move/3, hline/2, hline/3, hline/4, hline/5,
    vline/2, vline/3, vline/4, vline/5, border/8, wborder/9, box/3,
	curs_set/1,
    nl/0, nonl/0,
	scrollok/2
]).

% Utility functions
-export([ch/2, chattr/1, chcolor/1, display/1]).

-include("ncurses.hrl").
-include("ncommands.hrl").

-define(INT(Val),
        is_integer(Val)).
-define(INT(Val1, Val2),
        is_integer(Val1) andalso is_integer(Val2)).
-define(INT(Val1, Val2, Val3),
        is_integer(Val1) andalso is_integer(Val2) andalso is_integer(Val3)).
-define(BOOL(Val),
        is_boolean(Val)).
-define(BOOL(Val1, Val2),
        is_boolean(Val1) andalso is_boolean(Val2)).
-define(BOOL(Val1, Val2, Val3),
        is_boolean(Val1) andalso is_boolean(Val2) andalso is_boolean(Val3)).
-define(INT_BOOL(Val1, Val2),
        is_integer(Val1) andalso is_boolean(Val2)).
%---- Module API

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

win() -> gen_server:call(?MODULE, win, infinity).
win(Win) -> gen_server:call(?MODULE, {win, Win}, infinity).
app() -> gen_server:call(?MODULE, app, infinity).
app(App) -> gen_server:call(?MODULE, {app, App}, infinity).
wdom() -> gen_server:call(?MODULE, wdom, infinity).
wdom(WDom) -> gen_server:call(?MODULE, {wdom, WDom}, infinity).

trapexit() ->
    process_flag(trap_exit, true),
    erlang:link(erlang:whereis(?MODULE)),
    receive {'EXIT', _From, Reason} -> Reason end.

%---- NCurses API

% terminal routines
beep() -> ncall({?BEEP, undefined}).
flash() -> ncall({?FLASH, undefine}).
curses_version() -> ncall({?CURSES_VERSION, undefined}).

% window refresh
refresh() -> refresh(?W_STDSCR).
refresh(Win) -> ncall({?WREFRESH, Win}).
wnoutrefresh(Win) -> ncall({?WNOUTREFRESH, Win}).
doupdate() -> ncall({?DOUPDATE, undefined}).
werase() -> werase(?W_STDSCR).
werase(Win) -> ncall({?WERASE, Win}).
clear() -> clear(?W_STDSCR).
clear(Win) -> ncall({?WCLEAR, Win}).
clrtobot() -> clrtobot(?W_STDSCR).
clrtobot(Win) -> ncall({?WCLRTOBOT, Win}).
clrtoeol() -> clrtoeol(?W_STDSCR).
clrtoeol(Win) -> ncall({?WCLRTOEOL, Win}).
getyx() -> getyx(?W_STDSCR).
getyx(Win) -> ncall({?GETYX, Win}).
getbegyx() -> getbegyx(?W_STDSCR).
getbegyx(Win) -> ncall({?GETBEGYX, Win}).
getmaxyx() -> getmaxyx(?W_STDSCR).
getmaxyx(Win) -> ncall({?GETMAXYX, Win}).
getparyx() -> getparyx(?W_STDSCR).
getparyx(Win) -> ncall({?GETPARYX, Win}).

% input options
raw() -> ncall({?RAW, undefined}).
noraw() -> ncall({?NORAW, undefined}).
cbreak() -> ncall({?CBREAK, undefined}).
nocbreak() -> "Not supported".
echo() -> ncall({?ECHO, undefined}).
noecho() -> ncall({?NOECHO, undefined}).
keypad(Win, Flag) -> ncall({?KEYPAD, {Win, Flag}}).
nodelay(Win, Flag) -> ncall({?NODELAY, {Win, Flag}}).
halfdelay(Tenths) -> ncall({?HALFDELAY, Tenths}).
notimeout(Win, Flag) -> ncall({?NOTIMEOUT, {Win, Flag}}).
timeout(Delay) -> wtimeout(?W_STDSCR, Delay).
wtimeout(Win, Delay) -> ncall({?WTIMEOUT, {Win, Delay}}).

% do character output
addch(Char) -> addch(?W_STDSCR, Char).
addch(Win, Char) -> ncall({?WADDCH, {Win, Char}}).
addch(Y, X, Char) -> addch(?W_STDSCR, Y, X, Char).
addch(Win, Y, X, Char) -> ncall({?WADDCH, {Win,Y,X,Char}}).

echochar(Char) -> echochar(?W_STDSCR, Char).
echochar(Win, Char) -> ncall({?WECHOCHAR, {Win, Char}}).

delch() -> delch(?W_STDSCR).
delch(Win) -> ncall({?WDELCH, {Win}}).
delch(Y, X) -> delch(?W_STDSCR, Y, X).
delch(Win, Y, X ) -> ncall({?WDELCH, {Win, Y, X}}).

insch(Char) -> insch(?W_STDSCR, Char).
insch(Win, Char) -> ncall({?WINSCH, {Win, Char}}).
insch(Y, X, Char) -> insch(?W_STDSCR, Y, X, Char).
insch(Win, Y, X, Char) -> ncall({?WINSCH, {Win, Y, X, Char}}).

% do string output
addstr(String) -> addstr(?W_STDSCR, String).
addstr(Win, String) ->
    Str = lists:flatten(String),
    addnstr_(Win, Str, erlang:length(Str)).
addstr(Y, X, String) -> addstr(?W_STDSCR, Y, X, String).
addstr(Win, Y, X, String) ->
    Str = lists:flatten(String),
    addnstr_(Win, Y, X, Str, erlang:length(Str)).

% In the following four APIs N should not be less than the size of String
% list. It is better to use the addstr() variant.
addnstr(String, N) -> addnstr(?W_STDSCR, String, N).
addnstr(Win, String, N) ->
    Str = lists:flatten(String),
    addnstr_(Win, Str, N).
addnstr(Y, X, String, N) -> addnstr(?W_STDSCR, Y, X, String, N).
addnstr(Win, Y, X, String, N) ->
    Str = lists:flatten(String),
    addnstr_(Win, Y, X, Str, N).

addnstr_(Win, Str, N) ->                 % local method
    ncall({?WADDNSTR, {Win, N, Str}}).

addnstr_(Win, Y, X, Str, N) ->           % local method
    ncall({?WADDNSTR, {Win, Y, X, N, Str}}).

%% This can send both string terms and list terms based on the chtype values.
addchstr(String) -> addchstr(?W_STDSCR, String).
addchstr(Win, String) ->
    Str = lists:flatten(String),
    addchnstr_(Win, Str, erlang:length(Str)).
addchstr(Y, X, String) -> addchstr(?W_STDSCR, Y, X, String).
addchstr(Win, Y, X, String) ->
    Str = lists:flatten(String),
    addchnstr_(Win, Y, X, Str, erlang:length(Str)).

addchnstr(String, N) -> addchnstr(?W_STDSCR, String, N).
addchnstr(Win, String, N) ->
    Str = lists:flatten(String),
    addchnstr_(Win, Str, N).
addchnstr(Y, X, String, N) -> addchnstr(?W_STDSCR, Y, X, String, N).
addchnstr(Win, Y, X, String, N) ->
    Str = lists:flatten(String),
    addchnstr_(Win, Y, X, Str, N).

addchnstr_(Win, Str, N) ->                 % local method
    ncall({?WADDCHNSTR, {Win, N, Str}}).

addchnstr_(Win, Y, X, Str, N) ->           % local method
    ncall({?WADDCHNSTR, {Win, Y, X, N, Str}}).

insstr(String) -> insstr(?W_STDSCR, String).
insstr(Win, String) ->
    Str = lists:flatten(String),
    insnstr_(Win, Str, erlang:length(Str)).
insstr(Y, X, String) -> insstr(?W_STDSCR, Y, X, String).
insstr(Win, Y, X, String) ->
    Str = lists:flatten(String),
    insnstr_(Win, Y, X, Str, erlang:length(Str)).

insnstr(String, N) -> insnstr(?W_STDSCR, String, N).
insnstr(Win, String, N) ->
    Str = lists:flatten(String),
    insnstr_(Win, Str, N).
insnstr(Y, X, String, N) -> insnstr(?W_STDSCR, Y, X, String, N).
insnstr(Win, Y, X, String, N) ->
    Str = lists:flatten(String),
    insnstr_(Win, Y, X, Str, N).

insnstr_(Win, Str, N) ->                 % local method
    ncall({?WINSNSTR, {Win, N, Str}}).

insnstr_(Win, Y, X, Str, N) ->           % local method
    ncall({?WINSNSTR, {Win, Y, X, N, Str}}).

deleteln() -> deleteln(?W_STDSCR).
deleteln(Win) -> ncall({?WDELETELN, {Win}}).
insdelln(N) -> insdelln(?W_STDSCR, N).
insdelln(Win, N) -> ncall({?WINSDELLN, {Win, N}}).
insertln() -> insertln(?W_STDSCR).
insertln(Win) -> ncall({?WINSERTLN, {Win}}).

% do attribute settings
color_set(CPair) -> color_set(?W_STDSCR, CPair).
color_set(Win, CPair) -> ncall({?COLOR_SET, {Win, CPair}}).
attrset(Attr) -> attrset(?W_STDSCR, Attr).
attrset(Win, Attr) -> ncall({?ATTRSET, {Win, Attr}}).
attroff(Mask) -> attroff(?W_STDSCR, Mask).
attroff(Win, Mask) -> ncall({?ATTROFF, {Win, Mask}}).
attron(Mask) -> attron(?W_STDSCR, Mask).
attron(Win, Mask) -> ncall({?ATTRON, {Win, Mask}}).
attr_get() -> attr_get(?W_STDSCR).
attr_get(Win) -> ncall({?ATTR_GET, {Win}}).
chgat(N, Attr, CPair) -> chgat(?W_STDSCR, N, Attr, CPair).
chgat(Win, N, Attr, CPair) -> ncall({?CHGAT, {Win, N, Attr, CPair}}).
chgat(Y, X, N, Attr, CPair) -> chgat(?W_STDSCR, Y, X, N, Attr, CPair).
chgat(Win, Y, X, N, Attr, CPair) -> ncall({?CHGAT, {Win,Y,X,N,Attr,CPair}}).

% do color settings
has_colors() -> ncall({?HAS_COLORS, undefined}).
can_change_color() -> ncall({?CAN_CHANGE_COLOR, undefined}).
start_color() -> ncall({?START_COLOR, undefined}).
init_pair(N, FColor, BColor) -> ncall({?INIT_PAIR, {N, FColor, BColor}}).
color_content(N) -> ncall({?COLOR_CONTENT, N}).

% do inputs
getch() -> ncall({?GETCH, undefined}).
ungetch(Char) -> ncall({?UNGETCH, Char}).
has_key(Char) -> ncall({?HAS_KEY, Char}).
inch() -> inch(?W_STDSCR).
inch(Win) -> ncall({?INCH, {Win}}).
inch(Y, X) -> inch(?W_STDSCR, Y, X).
inch(Win, Y, X) -> ncall({?INCH, {Win, Y, X}}).
innstr(N) -> innstr(?W_STDSCR, N).
innstr(Win, N) -> ncall({?INNSTR, {Win, N}}).
innstr(Y, X, N) -> innstr(?W_STDSCR, Y, X, N).
innstr(Win, Y, X, N) -> ncall({?INNSTR, {Win, Y, X, N}}).
inchnstr(N) -> inchnstr(?W_STDSCR, N).
inchnstr(Win, N) -> ncall({?INCHNSTR, {Win, N}}).
inchnstr(Y, X, N) -> inchnstr(?W_STDSCR, Y, X, N).
inchnstr(Win, Y, X, N) -> ncall({?INCHNSTR, {Win, Y, X, N}}).


% Win functions
newwin(Height, Width, StartY, StartX) ->
    ncall({?NEWWIN, {Height, Width, StartY, StartX}}).
delwin(Win) -> ncall({?DELWIN, Win}).
move(Y, X) -> move(?W_STDSCR, Y, X).
move(Win, Y, X) -> ncall({?MOVE, {Win, X, Y}}).
curs_set(Flag) -> ncall({?CURS_SET, Flag}).
hline(Char, MaxN) -> hline(?W_STDSCR, Char, MaxN).
hline(Win, Char, MaxN) -> ncall({?HLINE, {Win, Char, MaxN}}).
hline(Y, X, Char, MaxN) -> hline(?W_STDSCR, Y, X, Char, MaxN).
hline(Win, Y, X, Char, MaxN) -> ncall({?HLINE, {Win, Y, X, Char, MaxN}}).
vline(Char, MaxN) -> vline(?W_STDSCR, Char, MaxN).
vline(Win, Char, MaxN) -> ncall({?VLINE, {Win, Char, MaxN}}).
vline(Y, X, Char, MaxN) -> vline(?W_STDSCR, Y, X, Char, MaxN).
vline(Win, Y, X, Char, MaxN) -> ncall({?VLINE, {Win, Y, X, Char, MaxN}}).

nl() -> ncall({?NL, undefined}).
nonl() -> ncall({?NONL, undefined}).
scrollok(Win, Flag) -> ncall({?SCROLLOK, {Win, Flag}}).
border(Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) ->
    wborder(0, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs).
wborder(Win, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs) ->
    Args = {Win, Ls, Rs, Ts, Bs, TLs, TRs, BLs, BRs},
    ncall({?WBORDER, Args}).
box(Win, Vert, Horz) -> ncall({?BOX, {Win, Vert, Horz}}).


ncall({Cmd, Arg}, Timeout) ->
    {ok, App} = application:get_application(),
    gen_server:call(?MODULE, {curses, App, Cmd, Arg}, Timeout).

ncall(Request) -> ncall(Request, infinity).

% Utility functions
ch(Attr, Color) ->
    fun(Char) -> Attr bor Color bor Char end.

chattr(Attr) ->
    fun(Char) -> Attr bor Char end.

chcolor(Color) ->
    fun(Char)-> Color bor Char end.

display(Term) ->
    addstr( io_lib:format( "~p ~n", [Term] )),
    refresh(),
    ok.

% Behaviour Callbacks
init(_Args) ->
    process_flag(trap_exit, true),

    case load_driver() of
        ok ->
            Port = erlang:open_port({spawn, "ncdrv"}, [binary]),
            ok = do_call(Port, ?INITSCR),
            ok = do_call(Port, ?RAW),
            ok = do_call(Port, ?NOECHO),
            ok = do_call(Port, ?WERASE, 0),
            ok = do_call(Port, ?WREFRESH, 0),
            ok = do_call(Port, ?START_COLOR),
            init_pairs( Port, do_call(Port, ?HAS_COLORS )),
            {ok, #screen{ port = Port }};

        {error, ErrorCode} ->
            exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.


handle_call({curses, App, ?GETCH, _}, From, #screen{getch=undefined}=State) ->
    case State#screen.app of
        App -> {noreply, State#screen{getch=From}};
        _ -> {reply, false_context, State}
    end;

handle_call({curses, _App, ?GETCH, _}, _From, State) ->
    {reply, busy, State};

handle_call({curses, App, Cmd, Args}, _From, State) ->
    case State#screen.app of
        App -> {reply, do_call(State#screen.port, Cmd, Args), State};
        _ -> {reply, false_context, State}
    end;

handle_call(app, _From, #screen{app=App}=State) ->
    {reply, App, State};

handle_call({app, App}, _From, State) ->
    {reply, App, State#screen{app=App}};


handle_call(win, _From, #screen{win=Win}=State) ->
    {reply, Win, State};

handle_call({win, Win}, _From, State) ->
    {reply, Win, State#screen{win=Win}};

handle_call(wdom, _, #screen{wdom=WDom}=State) ->
    {reply, WDom, State};

handle_call({wdom, WDom}, _, State) ->
    {reply, WDom, State#screen{wdom=WDom}}.


handle_info({_Port, {data, Binary}}, #screen{getch=undefined}=State) ->
    State#screen.win ! {data, binary_to_term(Binary)},
    {noreply, State};

handle_info({_Port, {data, Binary}}, State) ->
    gen_server:reply(State#screen.getch, binary_to_term(Binary)),
    {noreply, State#screen{ getch = undefined }}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {noreply, State}.

terminate(_Reason, State) ->
    do_call(State#screen.port, ?ENDWIN),
    do_call(State#screen.port, ?CURS_SET, ?CURS_NORMAL),
    erlang:port_close(State#screen.port),
    erl_ddll:unload("ncdrv").

%%-- Internal Functions

init_pairs(Port, true) -> init_pairs(Port, ?COLOR_PAIR_LIST);
init_pairs(_, false) -> false;

init_pairs(_, []) -> true;
init_pairs(Port, [{N, Fg, Bg} | CPairs]) -> 
    do_call(Port, ?INIT_PAIR, {N, Fg, Bg}),
    init_pairs(Port, CPairs).


do_call(Port, Cmd, Args) ->
    binary_to_term(erlang:port_control(Port, Cmd, term_to_binary(Args))).

do_call(Port, Cmd) -> do_call(Port, Cmd, undefined).


load_driver() ->
    Dir = case code:priv_dir(ncurses) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "ncdrv").

