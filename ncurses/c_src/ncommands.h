#define BEEP        1 
#define FLASH       2 
#define CURSES_VERSION 3 
#define GETYX       4
#define GETMAXYX    5
#define GETBEGYX    6
#define GETPARYX    7

#define REFRESH     11
#define DOUPDATE    13
#define ERASE       14
#define CLEAR       15
#define CLRTOBOT    16
#define CLRTOEOL    17

#define RAW         21
#define NORAW       22
#define CBREAK      23
#define NOCBREAK    24
#define ECHO        25
#define NOECHO      26
#define KEYPAD      27
#define NODELAY     28
#define HALFDELAY   29
#define NOTIMEOUT   30
#define TIMEOUT     31

#define ADDCH       40
#define ECHOCHAR    41
#define DELCH       42
#define INSCH       43
#define ADDNSTR     44
#define INSNSTR     45
#define ADDCHNSTR   46
#define DELETELN    47
#define INSDELLN    48
#define INSERTLN    49

#define COLOR_SET   51
#define ATTRSET     52
#define ATTROFF     53
#define ATTRON      54
#define ATTR_GET    55
#define CHGAT       56
#define HAS_COLORS  57
#define CAN_CHANGE_COLOR 58
#define START_COLOR 59
#define INIT_PAIR   60
#define COLOR_CONTENT 61

#define GETCH       70
#define UNGETCH     71
#define HAS_KEY     72
#define INCH        73
#define INNSTR      74
#define INCHNSTR    75

#define INITSCR     100
#define ENDWIN      101
#define MOVE        104
#define CURS_SET    105

#define NL          119
#define NONL        120
#define SCROLLOK    121
