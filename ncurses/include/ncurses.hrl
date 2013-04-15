% chtype
-define(A_ATTRIBUTES, 16#FFFFFF00). % Mask value for chtype attribute portion
-define(A_CHARTEXT,   16#FF).
-define(A_COLOR,      16#FF00).

% coolean
-define(TRUE, 1).
-define(FALSE, 0).

% cursor visibility
-define(CURS_INVISIBLE, 0).
-define(CURS_NORMAL, 1).
-define(CURS_VERY_VISIBLE, 2).

% color
-define(COLOR_BLACK,    0).
-define(COLOR_RED,      1).
-define(COLOR_GREEN,    2).
-define(COLOR_YELLOW,   3).
-define(COLOR_BLUE,     4).
-define(COLOR_MAGENTA,  5).
-define(COLOR_CYAN,     6).
-define(COLOR_WHITE,    7).

-define(COLOR_BR,    1).
-define(COLOR_BG,    2).
-define(COLOR_BY,    3).
-define(COLOR_BBl,   4).
-define(COLOR_BM,    5).
-define(COLOR_BC,    6).
-define(COLOR_BW,    7).

-define(COLOR_RB,    9).
-define(COLOR_RG,    10).
-define(COLOR_RY,    11).
-define(COLOR_RBl,   12).
-define(COLOR_RM,    13).
-define(COLOR_RC,    14).
-define(COLOR_RW,    15).

-define(COLOR_GB,    17).
-define(COLOR_GR,    18).
-define(COLOR_GY,    19).
-define(COLOR_GBl,   20).
-define(COLOR_GM,    21).
-define(COLOR_GC,    22).
-define(COLOR_GW,    23).

-define(COLOR_YB,    25).
-define(COLOR_YR,    26).
-define(COLOR_YG,    27).
-define(COLOR_YBl,   28).
-define(COLOR_YM,    29).
-define(COLOR_YC,    30).
-define(COLOR_YW,    31).

-define(COLOR_BlB,   33).
-define(COLOR_BlR,   34).
-define(COLOR_BlG,   35).
-define(COLOR_BlY,   36).
-define(COLOR_BlM,   37).
-define(COLOR_BlC,   38).
-define(COLOR_BlW,   39).

-define(COLOR_MB,    41).
-define(COLOR_MR,    42).
-define(COLOR_MG,    43).
-define(COLOR_MY,    44).
-define(COLOR_MBl,   45).
-define(COLOR_MC,    46).
-define(COLOR_MW,    47).

-define(COLOR_CB,    49).
-define(COLOR_CR,    50).
-define(COLOR_CG,    51).
-define(COLOR_CY,    52).
-define(COLOR_CBl,   53).
-define(COLOR_CM,    54).
-define(COLOR_CW,    55).

-define(COLOR_WB,    57).
-define(COLOR_WR,    58).
-define(COLOR_WG,    59).
-define(COLOR_WY,    60).
-define(COLOR_WBl,   61).
-define(COLOR_WM,    62).
-define(COLOR_WC,    63).
-define(CH(Attr, Color, Char), (Attr bor Color bor Char)).

-define(COLOR_PAIR(C), (C bsl 8)).

% attribute
-define(A_NORMAL,       0).
-define(A_STANDOUT,     (1 bsl (8 + 8))).
-define(A_UNDERLINE,    (1 bsl (8 + 9))).
-define(A_REVERSE,      (1 bsl (8 + 10))).
-define(A_BLINK,        (1 bsl (8 + 11))).
-define(A_DIM,          (1 bsl (8 + 12))).
-define(A_BOLD,         (1 bsl (8 + 13))).
-define(A_ALTCHARSET,   (1 bsl (8 + 14))).
-define(A_INVIS,        (1 bsl (8 + 15))).
-define(A_PROTECT,      (1 bsl (8 + 16))).

-define(W_STDSCR, 0).

% Alternate character set
-define(ACS_LRCORNER,  16#40006a).
-define(ACS_URCORNER,  16#40006b).
-define(ACS_ULCORNER,  16#40006c).
-define(ACS_LLCORNER,  16#40006d).
-define(ACS_HLINE,     16#400071).
-define(ACS_LTEE,      16#400074).
-define(ACS_RTEE,      16#400075).
-define(ACS_BTEE,      16#400076).
-define(ACS_TTEE,      16#400077).
-define(ACS_VLINE,     16#400078).
-define(ACS_PLUS,      16#40006e).

-define(ACS_DIAMOND,   4194400).
-define(ACS_CKBOARD,   4194401).
-define(ACS_DEGREE,    4194406).
-define(ACS_PLMINUS,   4194407).
-define(ACS_BOARD,     4194408).
-define(ACS_LANTERN,   4194409).
-define(ACS_S1,        4194415).
-define(ACS_S3,        4194416).
-define(ACS_S7,        4194418).
-define(ACS_S9,        4194419).
-define(ACS_LEQUAL,    4194425).
-define(ACS_GEQUAL,    4194426).
-define(ACS_PI,        4194427).
-define(ACS_NEQUAL,    4194428).
-define(ACS_STERLING,  4194429).
-define(ACS_BULLET,    4194430).
-define(ACS_RARROW,    4194347).
-define(ACS_LARROW,    4194348).
-define(ACS_UARROW,    4194349).
-define(ACS_DARROW,    4194350).
-define(ACS_BLOCK,     4194352).

% key codes
-define(KEY_TAB, 9).
-define(KEY_ESC, 27).
-define(KEY_DOWN, 258).
-define(KEY_UP, 259).
-define(KEY_LEFT, 260).
-define(KEY_RIGHT, 261).
-define(KEY_HOME, 262).
-define(KEY_F(N), 264+N).
-define(KEY_DEL, 330).
-define(KEY_INS, 331).
-define(KEY_PGDOWN, 338).
-define(KEY_PGUP, 339).
-define(KEY_END, 360).

-define(COLOR_PAIR_LIST,
            [ {?COLOR_BR,  ?COLOR_BLACK, ?COLOR_RED},
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
            ]).

% NCurses screen state
-record( screen, {
            port,
            rows,
            cols,
            getch,
            onquit,
            app={none, none, none}, % {appname, window_node, root_node}
            apps=[]         % list of {appname, root_node}
         }).

-record( evtsys, {
            channels,   % list of channels record
            appdoms     % { app_path, root_node }
         }).

-record( channel, {
            name,       % name of the channel
            subscribers % [ {Ref, M, F, A}, {Ref, Proc}, ...]
         }).

-record( event, {
            name,       % can be any erlang term.
            value       % event payload
         }).
