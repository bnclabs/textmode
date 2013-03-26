% chtype
-define(A_ATTRIBUTES, 16#FFFFFF00). % Mask value for chtype attribute portion
-define(A_CHARTEXT,   16#FF).
-define(A_COLOR,      16#FF00).

% Boolean
-define(TRUE, 1).
-define(FALSE, 0).

% Cursor visibility
-define(CURS_INVISIBLE, 0).
-define(CURS_NORMAL, 1).
-define(CURS_VERY_VISIBLE, 2).

% Color
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

% Attribute
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

% Border characters
-define(ACS_DIAMOND, 4194400).
-define(ACS_CKBOARD, 4194401).
-define(ACS_DEGREE, 4194406).
-define(ACS_PLMINUS, 4194407).
-define(ACS_BOARD, 4194408).
-define(ACS_LANTERN, 4194409).
-define(ACS_LRCORNER, 4194410).
-define(ACS_URCORNER, 4194411).
-define(ACS_ULCORNER, 4194412).
-define(ACS_LLCORNER, 4194413).
-define(ACS_PLUS, 4194414).
-define(ACS_S1, 4194415).
-define(ACS_S3, 4194416).
-define(ACS_HLINE, 4194417).
-define(ACS_S7, 4194418).
-define(ACS_S9, 4194419).
-define(ACS_LTEE, 4194420).
-define(ACS_RTEE, 4194421).
-define(ACS_BTEE, 4194422).
-define(ACS_TTEE, 4194423).
-define(ACS_VLINE, 4194424).
-define(ACS_LEQUAL, 4194425).
-define(ACS_GEQUAL, 4194426).
-define(ACS_PI, 4194427).
-define(ACS_NEQUAL, 4194428).
-define(ACS_STERLING, 4194429).
-define(ACS_BULLET, 4194430).
-define(ACS_RARROW, 4194347).
-define(ACS_LARROW, 4194348).
-define(ACS_UARROW, 4194349).
-define(ACS_DARROW, 4194350).
-define(ACS_BLOCK, 4194352).

% Key codes
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
