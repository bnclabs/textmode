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
-export([start_link/1, mainbox/0, mainbox/1, doc/0, doc/1, loaddoc/1, loaddoc/2,
         ncall/1, ncall/2, render/1, render/2, backdrop/1, backdrop/2
        ]).

% NCurses API
-export([
    % terminal
    beep/0, flash/0, curses_version/0,
    beep/1, flash/1, curses_version/1,
    getyx/0, getbegyx/0, getmaxyx/0, getparyx/0,
    getyx/1, getbegyx/1, getmaxyx/1, getparyx/1,

    % window 
    refresh/0, doupdate/0, erase/0,  clear/0, clrtobot/0, clrtoeol/0, 
    refresh/1, doupdate/1, erase/1,  clear/1, clrtobot/1, clrtoeol/1, 
    
    % input options
    raw/0, noraw/0, cbreak/0, nocbreak/0, echo/0, noecho/0, keypad/1,
    raw/1, noraw/1, cbreak/1, nocbreak/1, echo/1, noecho/1, keypad/2,
    nodelay/1, halfdelay/1, timeout/1, notimeout/1,
    nodelay/2, halfdelay/2, timeout/2, notimeout/2,
    
    % do character, string and line output
    addch/1, addch/2, addch/3, addch/4,
    echochar/1, echochar/2,
    delch/0, delch/1, delch/2, delch/3,
    insch/1, insch/2, insch/3, insch/4,

    addstr/1, addstr/2, addstr/3, addstr/4,
    addnstr/2, addnstr/3, addnstr/4, addnstr/5,
    addchstr/1, addchstr/2, addchstr/3, addchstr/4,
    addchnstr/2, addchnstr/3, addchnstr/4, addchnstr/5,
    insstr/1, insstr/2, insstr/3, insstr/4,
    insnstr/2, insnstr/3, insnstr/4, insnstr/5,
    deleteln/0, deleteln/1, insdelln/1, insdelln/2, insertln/0, insertln/1,

    % do attribute settings
    color_set/1, attrset/1, attroff/1, attron/1, attr_get/0,
    color_set/2, attrset/2, attroff/2, attron/2, attr_get/1,

    chgat/3, chgat/4, chgat/5, chgat/6,

    has_colors/0, can_change_color/0, start_color/0, init_pair/3,
    has_colors/1, can_change_color/1, start_color/1, init_pair/4,
    color_content/1, color_content/2,

    % do input
    getch/0, ungetch/1, has_key/1,
    getch/1, ungetch/2, has_key/2,
    inch/0, inch/1, inch/2, inch/3,
    innstr/1, innstr/2, innstr/3, innstr/4,
    inchnstr/1, inchnstr/2, inchnstr/3, inchnstr/4,

    % Window functions
    move/2, curs_set/1, nl/0, nonl/0, scrollok/1,
    move/3, curs_set/2, nl/1, nonl/1, scrollok/2
]).

% Utility functions
-export([ch/2, chattr/1, chcolor/1]).

-include("ncurses.hrl").
-include("ncdom.hrl").
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
    ServerName = 
        case init:get_argument(screen) of
            {ok, [[Name | _ ] | _]} -> {local, Name};
            _ -> {local, ?MODULE}
        end,
    gen_server:start_link(ServerName, ?MODULE, {ServerName, Args}, []).

mainbox() -> mainbox(?MODULE).
mainbox(Ref) ->
    gen_server:call(Ref, mainbox, infinity).

doc() -> doc(?MODULE).
doc(Ref) -> 
    gen_server:call(Ref, doc, infinity).

loaddoc(Doc) -> loaddoc(?MODULE, Doc).
loaddoc(Ref, Doc) ->
    gen_server:call(Ref, {doc, Doc}, infinity).


ncall(Ref, {Cmd}) -> ncall(Ref, {Cmd, undefined, infinity});
ncall(Ref, {Cmd, Args}) -> ncall(Ref, {Cmd, Args, infinity});
ncall(Ref, {Cmd, Args, Timeout}) ->
    gen_server:call(Ref, {docall, Cmd, Args}, Timeout).

ncall(Request) -> ncall(?MODULE, Request).


render(Ref, {Buf}) -> render(Ref, {0, 0, Buf});
render(Ref, {Y, Buf}) -> render(Ref, {Y, 0, Buf});
render(Ref, Req) -> 
    gen_server:call(Ref, {render, Req}, infinity).

render(Req) -> render(?MODULE, Req).


backdrop(Ref, XRoot) ->
    gen_server:call(Ref, {backdrop, XRoot}, infinity).

backdrop(XRoot) -> backdrop(?MODULE, XRoot).


%---- NCurses API

% terminal routines
beep() -> beep(?MODULE).
beep(Ref) -> ncall(Ref, {?BEEP}).
flash() -> flash(?MODULE).
flash(Ref) -> ncall(Ref, {?FLASH}).
curses_version() -> curses_version(?MODULE).
curses_version(Ref) -> ncall(Ref, {?CURSES_VERSION}).
getyx() -> getyx(?MODULE).
getyx(Ref) -> ncall(Ref, {?GETYX}).
getbegyx() -> getbegyx(?MODULE).
getbegyx(Ref) -> ncall(Ref, {?GETBEGYX}).
getmaxyx() -> getmaxyx(?MODULE).
getmaxyx(Ref) -> ncall(Ref, {?GETMAXYX}).
getparyx() -> getparyx(?MODULE).
getparyx(Ref) -> ncall(Ref, {?GETPARYX}).


%-- window refresh
refresh() -> refresh(?MODULE).
refresh(Ref) -> ncall(Ref, {?REFRESH}).
doupdate() -> doupdate(?MODULE).
doupdate(Ref) -> ncall(Ref, {?DOUPDATE}).
erase() -> ncdrv:erase(?MODULE).
erase(Ref) -> ncall(Ref, {?ERASE}).
clear() -> clear(?MODULE).
clear(Ref) -> ncall(Ref, {?CLEAR}).
clrtobot() -> clrtobot(?MODULE).
clrtobot(Ref) -> ncall(Ref, {?CLRTOBOT}).
clrtoeol() -> clrtoeol(?MODULE).
clrtoeol(Ref) -> ncall(Ref, {?CLRTOEOL}).

%-- input options
raw() -> raw(?MODULE).
raw(Ref) -> ncall(Ref, {?RAW}).

noraw() -> noraw(?MODULE).
noraw(Ref) -> ncall(Ref, {?NORAW}).

cbreak() -> cbreak(?MODULE).
cbreak(Ref) -> ncall(Ref, {?CBREAK}).

nocbreak() -> "Not supported".
nocbreak(_) -> "Not supported".

echo() -> echo(?MODULE).
echo(Ref) -> ncall(Ref, {?ECHO}).

noecho() -> noecho(?MODULE).
noecho(Ref) -> ncall(Ref, {?NOECHO}).

keypad(Flag) -> keypad(?MODULE, Flag).
keypad(Ref, Flag) -> ncall(Ref, {?KEYPAD, Flag}).

nodelay(Flag) -> nodelay(?MODULE, Flag).
nodelay(Ref, Flag) -> ncall(Ref, {?NODELAY, Flag}).

halfdelay(Tenths) -> halfdelay(?MODULE, Tenths).
halfdelay(Ref, Tenths) -> ncall(Ref, {?HALFDELAY, Tenths}).

timeout(Delay) -> timeout(?MODULE, Delay).
timeout(Ref, Delay) -> ncall(Ref, {?TIMEOUT, Delay}).

notimeout(Flag) -> notimeout(?MODULE, Flag).
notimeout(Ref, Flag) -> ncall(Ref, {?NOTIMEOUT, Flag}).

%-- do character output
addch(Char) -> addch(?MODULE, Char).
addch(Ref, Char) -> ncall(Ref, {?ADDCH, {Char}}).
addch(Y, X, Char) -> addch(?MODULE, Y, X, Char).
addch(Ref, Y, X, Char) -> ncall(Ref, {?ADDCH, {Y,X,Char}}).

echochar(Char) -> echochar(?MODULE, Char).
echochar(Ref, Char) -> ncall(Ref, {?ECHOCHAR, Char}).

delch() -> delch(?MODULE).
delch(Ref) -> ncall(Ref, {?DELCH, {}}).
delch(Y, X ) -> delch(?MODULE, Y, X).
delch(Ref, Y, X ) -> ncall(Ref, {?DELCH, {Y, X}}).

insch(Char) -> insch(?MODULE, Char).
insch(Ref, Char) -> ncall(Ref, {?INSCH, {Char}}).
insch(Y, X, Char) -> insch(?MODULE, Y, X, Char).
insch(Ref, Y, X, Char) -> ncall(Ref, {?INSCH, {Y, X, Char}}).


%-- do string output
addstr(String) -> addnstr(?MODULE, String, length(String)).
addstr(Ref, String) -> addnstr(Ref, String, length(String)).
addstr(Y, X, String) -> addnstr(?MODULE, Y, X, String, length(String)).
addstr(Ref, Y, X, String) -> addnstr(Ref, Y, X, String, length(String)).

addnstr(String, N) -> addnstr(?MODULE, String, N).
addnstr(Ref, String, N) -> ncall(Ref, {?ADDNSTR, {N, String}}).
addnstr(Y, X, String, N) -> addnstr(?MODULE, Y, X, String, N).
addnstr(Ref, Y, X, String, N) -> ncall(Ref, {?ADDNSTR, {Y, X, N, String}}).

% This can send both string terms and list terms based on the chtype values.
addchstr(String) -> addchnstr(?MODULE, String, length(String)).
addchstr(Ref, String) -> addchnstr(Ref, String, length(String)).
addchstr(Y, X, String) -> addchnstr(?MODULE, Y, X, String, length(String)).
addchstr(Ref, Y, X, String) -> addchnstr(Ref, Y, X, String, length(String)).

addchnstr(String, N) -> addchnstr(?MODULE, String, N).
addchnstr(Ref, String, N) -> ncall(Ref, {?ADDCHNSTR, {N, String}}).
addchnstr(Y, X, String, N) -> addchnstr(?MODULE, Y, X, String, N).
addchnstr(Ref, Y, X, String, N) -> ncall(Ref, {?ADDCHNSTR, {Y, X, N, String}}).

insstr(String) -> insnstr(?MODULE, String, length(String)).
insstr(Ref, String) -> insnstr(Ref, String, length(String)).
insstr(Y, X, String) -> insnstr(?MODULE, Y, X, String, length(String)).
insstr(Ref, Y, X, String) -> insnstr(Ref, Y, X, String, length(String)).

insnstr(String, N) -> insnstr(?MODULE, String, N).
insnstr(Ref, String, N) -> ncall(Ref, {?INSNSTR, {N, String}}).
insnstr(Y, X, String, N) -> insnstr(?MODULE, Y, X, String, N).
insnstr(Ref, Y, X, String, N) -> ncall(Ref, {?INSNSTR, {Y, X, N, String}}).

deleteln() -> deleteln(?MODULE).
deleteln(Ref) -> ncall(Ref, {?DELETELN}).

insdelln(N) -> insdelln(?MODULE, N).
insdelln(Ref, N) -> ncall(Ref, {?INSDELLN, N}).

insertln() -> insertln(?MODULE).
insertln(Ref) -> ncall(Ref, {?INSERTLN}).


%-- do attribute settings
color_set(CPair) -> color_set(?MODULE, CPair).
color_set(Ref, CPair) -> ncall(Ref, {?COLOR_SET, CPair}).

attrset(Attr) -> attrset(?MODULE, Attr).
attrset(Ref, Attr) -> ncall(Ref, {?ATTRSET, Attr}).

attroff(Mask) -> attroff(?MODULE, Mask).
attroff(Ref, Mask) -> ncall(Ref, {?ATTROFF, Mask}).

attron(Mask) -> attron(?MODULE, Mask).
attron(Ref, Mask) -> ncall(Ref, {?ATTRON, Mask}).

attr_get() -> attr_get(?MODULE).
attr_get(Ref) -> ncall(Ref, {?ATTR_GET}).

chgat(N, Attr, CPair) ->
    chgat(?MODULE, N, Attr, CPair).
chgat(Ref, N, Attr, CPair) ->
    ncall(Ref, {?CHGAT, {N, Attr, CPair}}).
chgat(Y, X, N, Attr, CPair) ->
    chgat(?MODULE, Y, X, N, Attr, CPair).
chgat(Ref, Y, X, N, Attr, CPair) -> 
    ncall(Ref, {?CHGAT, {Y, X, N, Attr, CPair}}).


%-- do color settings
has_colors() -> has_colors(?MODULE).
has_colors(Ref) -> ncall(Ref, {?HAS_COLORS}).

can_change_color() -> can_change_color(?MODULE).
can_change_color(Ref) -> ncall(Ref, {?CAN_CHANGE_COLOR}).

start_color() -> start_color(?MODULE).
start_color(Ref) -> ncall(Ref, {?START_COLOR}).

init_pair(N, FColor, BColor) ->
    init_pair(?MODULE, N, FColor, BColor).
init_pair(Ref, N, FColor, BColor) ->
    ncall(Ref, {?INIT_PAIR, {N, FColor, BColor}}).

color_content(N) -> color_content(?MODULE, N).
color_content(Ref, N) -> ncall(Ref, {?COLOR_CONTENT, N}).

%-- do inputs
getch() -> "Not supported".
getch(_) -> "Not supported".

ungetch(Char) -> ungetch(?MODULE, Char).
ungetch(Ref, Char) -> ncall(Ref, {?UNGETCH, Char}).

has_key(Char) -> has_key(?MODULE, Char).
has_key(Ref, Char) -> ncall(Ref, {?HAS_KEY, Char}).

inch() -> inch(?MODULE).
inch(Ref) -> ncall(Ref, {?INCH, {}}).
inch(Y, X) -> inch(?MODULE, Y, X).
inch(Ref, Y, X) -> ncall(Ref, {?INCH, {Y, X}}).

innstr(N) -> innstr(?MODULE, N).
innstr(Ref, N) -> ncall(Ref, {?INNSTR, {N}}).
innstr(Y, X, N) -> innstr(?MODULE, Y, X, N).
innstr(Ref, Y, X, N) -> ncall(Ref, {?INNSTR, {Y, X, N}}).

inchnstr(N) -> inchnstr(?MODULE, N).
inchnstr(Ref, N) -> ncall(Ref, {?INCHNSTR, {N}}).
inchnstr(Y, X, N) -> inchnstr(?MODULE, Y, X, N).
inchnstr(Ref, Y, X, N) -> ncall(Ref, {?INCHNSTR, {Y, X, N}}).


%-- Win functions
move(Y, X) -> move(?MODULE, Y, X).
move(Ref, Y, X) -> ncall(Ref, {?MOVE, {Y, X}}).

curs_set(Flag) -> curs_set(?MODULE, Flag).
curs_set(Ref, Flag) -> ncall(Ref, {?CURS_SET, Flag}).

nl() -> nl(?MODULE).
nl(Ref) -> ncall(Ref, {?NL}).

nonl() -> nonl(?MODULE).
nonl(Ref) -> ncall(Ref, {?NONL}).

scrollok(Flag) -> scrollok(?MODULE, Flag).
scrollok(Ref, Flag) -> ncall(Ref, {?SCROLLOK, Flag}).


% Utility functions
ch(Attr, Color) ->
    fun(Char) -> Attr bor Color bor Char end.

chattr(Attr) ->
    fun(Char) -> Attr bor Char end.

chcolor(Color) ->
    fun(Char)-> Color bor Char end.


% Behaviour Callbacks
init({{local, Name}, _Args}) ->
    process_flag(trap_exit, true),
    case load_driver() of
        ok ->
            Port = erlang:open_port({spawn, "ncdrv"}, [binary]),
            ok = docall(Port, ?INITSCR),
            ok = docall(Port, ?RAW),
            ok = docall(Port, ?NOECHO),
            ok = docall(Port, ?ERASE),
            ok = docall(Port, ?REFRESH),
            ok = docall(Port, ?START_COLOR),
            init_pairs( Port, docall( Port, ?HAS_COLORS )),
            {Rows, Cols} = docall(Port, ?GETMAXYX),
            {ok, #screen{
                    ref={Name, node()},
                    port=Port, 
                    rows=Rows, 
                    cols=Cols }};

        {error, ErrorCode} ->
            exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.


handle_call(pid, _From, State) ->
    {reply, self(), State};

handle_call(mainbox, _From, #screen{rows=Rows, cols=Cols}=State) ->
    {reply, {0, 0, Rows, Cols}, State};


handle_call(doc, _From, #screen{doc=Doc}=State) ->
    {reply, Doc, State};

handle_call({loaddoc, Doc}, _From, State) ->
    {reply, Doc, load_doc(Doc, State)};

handle_call({docall, Cmd, Args}, _From, #screen{port=Port}=State) ->
    {reply, docall(Port, Cmd, Args), State};

handle_call({render, Req}, _From, #screen{port=Port}=State) ->
    {reply, render_buf(Port, Req), State};

handle_call({backdrop, #xnode{tag=RootTag}}, _From, State) ->
    #screen{port=Port, rows=Rows, cols=Cols} = State,
    Buf = ncbuf:cornerize( 
                ncbuf:frameborders(RootTag, ncbuf:frameinit(Rows, Cols)),
                Rows, Cols),
    render_buf(Port, {0, 0, tuple_to_list(Buf)}),
    {reply, ok, State}.


handle_info({_Port, {data, Binary}}, #screen{doc=Doc}=State) ->
    Ch = binary_to_term(Binary),
    case Ch of
        3 -> init:stop(0);
        _ ->
            case Doc#doc.focus of
                none -> error_msg:info_msg("Input ~p~n", [Ch]);
                #xnode{pid=Pid} -> Pid ! Ch
            end
    end,
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


terminate(Reason, State) ->
    error_logger:info_msg("ncdrv terminating : ~p~n", [Reason]),
    docall(State#screen.port, ?ENDWIN),
    docall(State#screen.port, ?CURS_SET, ?CURS_NORMAL),
    erlang:port_close(State#screen.port),
    erl_ddll:unload("ncdrv"),
    ok.


code_change(_, State, _) ->
    {noreply, State}.

%%-- Internal Functions

docall(Port, Cmd, Args) ->
    binary_to_term( erlang:port_control( Port, Cmd, term_to_binary(Args) )).
docall(Port, Cmd) -> docall(Port, Cmd, undefined).


render_buf(Port, {_, _, []}) -> docall(Port, ?REFRESH);
render_buf(Port, {Y, X, [Line | Ls]}) ->
    S = if is_tuple(Line) -> tuple_to_list(Line); is_list(Line) -> Line end,
    docall(Port, ?ADDCHNSTR, {Y, X, length(S), S}),
    render_buf(Port, {Y+1, X, Ls}),
    ok.

load_doc(#doc{root=XRoot}=Doc, #screen{doc=none}=State) ->
    doc_backdrop(Doc, State),
    ncchan:propagate(XRoot, ?EV_LOAD),
    State#screen{doc=Doc};

load_doc(#doc{root=XRoot}=Doc, #screen{doc=OldDoc}=State) ->
    ncchan:propagate(OldDoc#doc.root, ?EV_UNLOAD),
    doc_backdrop(Doc, State),
    ncchan:propagate(XRoot, ?EV_LOAD),
    State#screen{doc=Doc}.

doc_backdrop(Doc, State) ->
    RootTag = Doc#doc.root#xnode.tag,
    #screen{port=Port, rows=Rows, cols=Cols} = State,
    Buf = ncbuf:cornerize( 
                ncbuf:frameborders(RootTag, ncbuf:frameinit(Rows, Cols)),
                Rows, Cols),
    render_buf(Port, {0, 0, tuple_to_list(Buf)}),
    ok.


init_pairs(Port, true) -> init_pairs(Port, ?COLOR_PAIR_LIST);
init_pairs(_, false) -> false;

init_pairs(_, []) -> true;
init_pairs(Port, [{N, Fg, Bg} | CPairs]) -> 
    docall(Port, ?INIT_PAIR, {N, Fg, Bg}),
    init_pairs(Port, CPairs).



load_driver() ->
    Dir = case code:priv_dir(ncurses) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "ncdrv").

