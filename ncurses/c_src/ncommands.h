#define BEEP        1 
#define FLASH       2 
#define CURSES_VERSION 3 
#define GETYX       4
#define GETMAXYX    5
#define GETBEGYX    6
#define GETPARYX    7

#define WREFRESH    11
#define WNOUTREFRESH 12
#define DOUPDATE    13
#define WERASE      14
#define WCLEAR      15
#define WCLRTOBOT   16
#define WCLRTOEOL   17

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
#define WTIMEOUT    31

#define WADDCH      40
#define WECHOCHAR   41
#define WADDNSTR    42
#define WADDCHNSTR  43
#define WDELCH      44
#define WDELETELN   45
#define WINSDELLN   46
#define WINSERTLN   47
#define WINSCH      48
#define WINSNSTR    49

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

#define UNGETCH     71
#define HAS_KEY     72
#define INCH        73
#define INNSTR      74
#define INCHNSTR    75

#define INITSCR     100
#define ENDWIN      101
#define NEWWIN      102
#define DELWIN      103
#define MOVE        104
#define CURS_SET    105
#define HLINE       110
#define VLINE       111

#define NL          119
#define NONL        120
#define SCROLLOK    121
#define WBORDER     134
#define BOX         135
