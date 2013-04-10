// Includes
#include <stdlib.h>
#include <string.h>
#include "ncommands.h"
#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"
#include "ncurses.h"    // selectively include ncursesw.
#include "assert.h"

#define _INT(A1,A2,A3,A4) ((int)(A1) << 24 | (A2) << 16 | (A3) << 8 | (A4))
#define _MAXWINDOWS 60
#define MAX_STRLEN  180

#if ERL_DRV_EXTENDED_MAJOR_VERSION < 2
  #define ErlDrvSizeT int
  #define ErlDrvSSizeT int
#endif

// State structure
typedef struct {
  WINDOW *win[_MAXWINDOWS+1];
  ei_x_buff eixb;
  char *args;
  int argslen;
  int index;
  int version;
  ErlDrvPort drv_port;
} state;

// Driver state and type conversion
void init_state(state *st, char *args, int argslen);
void ok(state *st);
void error_tuple(state *st, int code);
void boolean(state *st, int code);
void tuple(ei_x_buff *eixb, int size);
void list(ei_x_buff *eixb, int len);
void empty_list(ei_x_buff *eixb);
void atom(ei_x_buff *eixb, const char *str, int size);
void integer(ei_x_buff *eixb, int integer);
void string(ei_x_buff *eixb, const char *str);
void encode_ok_reply(state *st, int code);
void encode_string_reply(state *st, const char* str);
void encode_attr_get_reply(state *st, int code, int attr, int colorpair);
int findfreewindowslot(state *st);

void decode_string_chtype(state *st, chtype *chstr);

// Decoding libraries
void decode_string_chtype( state *st, chtype *chstr ) {
  int arity, i;
  ei_decode_list_header(st->args, &(st->index), &arity);
  if( arity ) {
    for(i=0; i<arity; i++)
      ei_decode_long(st->args, &(st->index), (long *)&chstr[i]);
    empty_list(&(st->eixb));
  } else {
    chstr[0] = 0;
  }
}

///// ncurses bridge API

// for terminal
void do_beep(state *st);
void do_flash(state *st);
void do_curses_version(state *st);
void do_getyx(state *st);
void do_getbegyx(state *st);
void do_getmaxyx(state *st);
void do_getparyx(state *st);

// for screen
void do_initscr(state *st);
void do_endwin(state *st);

// for window refresh
void do_wrefresh(state *st);
void do_wnoutrefresh(state *st);
void do_doupdate(state *st);
void do_werase(state *st);
void do_wclear(state *st);
void do_wclrtobot(state *st);
void do_wclrtoeol(state *st);

// for input options
void do_cbreak(state *st);
void do_nocbreak(state *st);
void do_raw(state *st);
void do_noraw(state *st);
void do_echo(state *st);
void do_noecho(state *st);
void do_keypad(state *st);
void do_nodelay(state *st);
void do_halfdelay(state *st);
void do_notimeout(state *st);
void do_wtimeout(state *st);

// for doing character, string and line output
void do_waddch(state *st);
void do_wechochar(state *st);
void do_wdelch(state *st);
void do_waddnstr(state *st);
void do_waddchnstr(state *st);
void do_wdeleteln(state *st);
void do_winsdelln(state *st);
void do_winsertln(state *st);
void do_winsch(state *st);
void do_winsnstr(state *st);

// for attribute settings
void do_color_set(state *st);
void do_attrset(state *st);
void do_attroff(state *st);
void do_attron(state *st);
void do_attr_get(state *st);
void do_chgat(state *st);
void do_has_colors(state *st);  // do color settings
void do_can_change_color(state *st);  // do color settings
void do_start_color(state *st);
void do_init_pair(state *st);
void do_color_content(state *st);

// input related.
void do_ungetch(state *st);
void do_has_key(state *st);
void do_inch(state *st);
void do_innstr(state *st);

void do_curs_set(state *st);
void do_curs_set(state *st);
void do_wattron(state *st);
void do_wattroff(state *st);
void do_nl(state *st);
void do_nonl(state *st);
void do_scrollok(state *st);
void do_newwin(state *st);
void do_delwin(state *st);
void do_wmove(state *st);
void do_hline(state *st);
void do_vline(state *st);
void do_wborder(state *st);
void do_box(state *st);

//// Erlang Callbacks

static ErlDrvData start(ErlDrvPort port, char *command) {
  state *drvstate = (state *)driver_alloc(sizeof(state));
  drvstate->drv_port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    drvstate->win[i] = NULL;
  return (ErlDrvData)drvstate;
}

static void stop(ErlDrvData drvstate) {
  state *st = (state *)drvstate;
  driver_select(st->drv_port, (ErlDrvEvent)(size_t)fileno(stdin), DO_READ, 0);
  driver_free(drvstate);
}

static void do_getch(ErlDrvData drvstate, ErlDrvEvent event) {
  state *st = (state *)drvstate;
  ei_x_buff eixb;
  int keycode;
  ei_x_new_with_version(&eixb);
  keycode = getch();
  integer(&eixb, keycode);
  driver_output(st->drv_port, eixb.buff, eixb.index);
}

static ErlDrvSSizeT control(ErlDrvData drvstate, unsigned int command,
			    char *args, ErlDrvSizeT argslen,
			    char **rbuf, ErlDrvSizeT rbuflen) {
  state *st = (state *)drvstate;
  init_state(st, args, argslen);

  switch (command) {
  // terminal
  case BEEP: do_beep(st); break;
  case FLASH: do_flash(st); break;
  case CURSES_VERSION: do_curses_version(st); break;
  case GETYX: do_getyx(st); break;
  case GETBEGYX: do_getbegyx(st); break;
  case GETMAXYX: do_getmaxyx(st); break;
  case GETPARYX: do_getparyx(st); break;
  // screen
  case INITSCR: do_initscr(st); break;
  case ENDWIN: do_endwin(st); break;
  // window refresh
  case WREFRESH: do_wrefresh(st); break;
  case WNOUTREFRESH: do_wnoutrefresh(st); break;
  case DOUPDATE: do_doupdate(st); break;
  case WERASE: do_werase(st); break;
  case WCLEAR: do_wclear(st); break;
  case WCLRTOBOT: do_wclrtobot(st); break;
  case WCLRTOEOL: do_wclrtoeol(st); break;
  // input options
  case RAW: do_raw(st); break;
  case NORAW: do_noraw(st); break;
  case CBREAK: do_cbreak(st); break;
  case NOCBREAK: do_nocbreak(st); break;
  case ECHO: do_echo(st); break;
  case NOECHO: do_noecho(st); break;
  case KEYPAD: do_keypad(st); break;
  case NODELAY: do_nodelay(st); break;
  case HALFDELAY: do_halfdelay(st); break;
  case NOTIMEOUT: do_notimeout(st); break;
  case WTIMEOUT: do_wtimeout(st); break;
  // do character, string, line output
  case WADDCH: do_waddch(st); break;
  case WECHOCHAR: do_wechochar(st); break;
  case WDELCH: do_wdelch(st); break;
  case WADDNSTR: do_waddnstr(st); break;
  case WADDCHNSTR: do_waddchnstr(st); break;
  case WDELETELN: do_wdeleteln(st); break;
  case WINSDELLN: do_winsdelln(st); break;
  case WINSERTLN: do_winsertln(st); break;
  case WINSCH: do_winsch(st); break;
  case WINSNSTR: do_winsnstr(st); break;
  // do attribute settings
  case COLOR_SET: do_color_set(st); break;
  case ATTRSET: do_attrset(st); break;
  case ATTROFF: do_attroff(st); break;
  case ATTRON: do_attron(st); break;
  case ATTR_GET: do_attr_get(st); break;
  case CHGAT: do_chgat(st); break;
  case HAS_COLORS: do_has_colors(st); break; // do color settings
  case CAN_CHANGE_COLOR: do_can_change_color(st); break;
  case START_COLOR: do_start_color(st); break;
  case INIT_PAIR: do_init_pair(st); break;
  case COLOR_CONTENT: do_color_content(st); break;
  // input related
  case UNGETCH: do_ungetch(st); break;
  case HAS_KEY: do_has_key(st); break;
  case INCH: do_inch(st); break;
  case INNSTR: do_innstr(st); break;

  case NEWWIN: do_newwin(st); break;
  case DELWIN: do_delwin(st); break;
  case MOVE: do_wmove(st); break;
  case CURS_SET: do_curs_set(st); break;
  case HLINE: do_hline(st); break;
  case VLINE: do_vline(st); break;
  case NL: do_nl(st); break;
  case NONL: do_nonl(st); break;
  case SCROLLOK: do_scrollok(st); break;
  case WBORDER: do_wborder(st); break;
  case BOX: do_box(st); break;
  default: break;
  }

  int rlen = st->eixb.index;
  ErlDrvBinary *response = driver_alloc_binary(rlen);
  memcpy(response->orig_bytes, st->eixb.buff, rlen);
  ei_x_free(&(st->eixb));
  *rbuf = (char *)response;
  return rlen;
}

//// NCurses bridge functions - definitions

// terminal
void do_beep(state *st) {
  encode_ok_reply(st, beep());
}

void do_flash(state *st) {
  encode_ok_reply(st, flash());
}

void do_curses_version(state *st) {
  encode_string_reply(st, curses_version());
}

void do_getyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_getbegyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getbegyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_getmaxyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getmaxyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_getparyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getparyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}


// screen
void do_initscr(state *st) {
  st->win[0] = (WINDOW *)initscr();
  driver_select(st->drv_port, (ErlDrvEvent)(size_t)fileno(stdin), DO_READ, 1);
  if (st->win[0] == NULL) {
    encode_ok_reply(st, -1);
  } else {
    encode_ok_reply(st, 0);
  }
}

void do_endwin(state *st) {
  encode_ok_reply(st, endwin());
}

// window refresh
void do_wrefresh(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wrefresh(st->win[slot]));
}

void do_wnoutrefresh(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wnoutrefresh(st->win[slot]));
}

void do_doupdate(state *st) {
  encode_ok_reply(st, doupdate());
}

void do_werase(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, werase(st->win[slot]));
}

void do_wclear(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wclear(st->win[slot]));
}

void do_wclrtobot(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wclear(st->win[slot]));
}

void do_wclrtoeol(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wclear(st->win[slot]));
}

// input options
void do_cbreak(state *st) {
  encode_ok_reply(st, cbreak());
}
void do_nocbreak(state *st) {
  encode_ok_reply(st, nocbreak());
}

void do_echo(state *st) {
  encode_ok_reply(st, echo());
}

void do_noecho(state *st) {
  encode_ok_reply(st, noecho());
}

void do_raw(state *st) {
  encode_ok_reply(st, raw());
}
void do_noraw(state *st) {
  encode_ok_reply(st, noraw());
}

void do_keypad(state *st) {
  int arity, bf;
  long slot;

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, keypad(st->win[slot], bf));
}

void do_nodelay(state *st) {
  int arity, bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, nodelay(st->win[slot], bf));
}

void do_halfdelay(state *st) {
  long delay;
  ei_decode_long(st->args, &(st->index), &delay);
  encode_ok_reply(st, halfdelay(delay));
}

void do_notimeout(state *st) {
  int arity, bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, notimeout(st->win[slot], bf));
}

void do_wtimeout(state *st) {
  int arity;
  long slot, delay;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &delay);
  wtimeout(st->win[slot], delay);
  encode_ok_reply(st, OK);
}


// do character, string and line output
void do_waddch(state *st) {
  int arity;
  long slot, y, x;
  chtype ch=0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity == 2 ) {
    ei_decode_long(st->args, &(st->index), (long *)&ch);
    encode_ok_reply(st, waddch(st->win[slot], ch));
  } else if( arity == 4 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), (long *)&ch);
    encode_ok_reply(st, mvwaddch(st->win[slot], (int)y, (int)x, ch));
  }
}

void do_wechochar(state *st) {
  int arity;
  long slot;
  chtype ch = 0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), (long *)&ch);
  encode_ok_reply(st, wechochar(st->win[slot], ch));
}

void do_wdelch(state *st) {
  int arity;
  long slot, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  if( arity == 1 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    encode_ok_reply(st, wdelch(st->win[slot]));
  } else if( arity == 3 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    encode_ok_reply(st, mvwdelch(st->win[slot], (int)y, (int)x));
  }
}

void do_waddnstr(state *st) {
  int arity;
  long slot, y, x, strlen;
  char *str;

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity == 3 ) {
    ei_decode_long(st->args, &(st->index), &strlen);
    str = (char *)driver_alloc(strlen);
    ei_decode_string(st->args, &(st->index), str);
    encode_ok_reply(st, waddnstr(st->win[slot], str, (int)strlen));
    driver_free(str);
  } else if( arity == 5 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &strlen);
    str = (char *)driver_alloc(strlen);
    ei_decode_string(st->args, &(st->index), str);
    encode_ok_reply(
        st, mvwaddnstr(st->win[slot], (int)y, (int)x, str, (int)strlen));
    driver_free(str);
  }
}

void do_waddchnstr(state *st) {
  int arity, code, i;
  long slot, y, x, strlen;
  chtype *chstr;
  char *str;

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity == 3 ) {
    ei_decode_long(st->args, &(st->index), &strlen);
    chstr = (chtype *)driver_alloc(strlen * sizeof(chtype));
    str = (char *)driver_alloc(strlen);
    code = ei_decode_string(st->args, &(st->index), str);
    if(code == OK) {
      for(i=0; i<strlen; i++) chstr[i] = (chtype) str[i];
    } else {
      decode_string_chtype(st, chstr);
    }
    encode_ok_reply(st,
        waddchnstr(st->win[slot], (const chtype*)chstr, (int)strlen));
    driver_free(chstr);
    driver_free(str);
  } else if( arity == 5 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &strlen);
    chstr = (chtype *)driver_alloc(strlen * sizeof(chtype));
    str = (char *)driver_alloc(strlen);
    code = ei_decode_string(st->args, &(st->index), str);
    if(code == OK) {
      for(i=0; i<strlen; i++) chstr[i] = (chtype) str[i];
    } else {
      decode_string_chtype(st, chstr);
    }
    encode_ok_reply( st,
        mvwaddchnstr(st->win[slot], (int)y, (int)x, (const chtype*)chstr,
                     (int)strlen));
    driver_free(chstr);
    driver_free(str);
  }
}

void do_wdeleteln(state *st) {
  int arity;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wdeleteln(st->win[slot]));
}

void do_winsdelln(state *st) {
  int arity;
  long slot, n;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &n);
  encode_ok_reply(st, winsdelln(st->win[slot], n));
}

void do_winsertln(state *st) {
  int arity;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, winsertln(st->win[slot]));
}

void do_winsch(state *st) {
  int arity;
  long slot, y, x;
  chtype ch=0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  if( arity == 2 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), (long *)&ch);
    encode_ok_reply(st, winsch(st->win[slot], (chtype)ch));
  } else if( arity == 4 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), (long *)&ch);
    encode_ok_reply(st, mvwinsch(st->win[slot], (int)y, (int)x, (chtype)ch));
  }
}

void do_winsnstr(state *st) {
  int arity;
  long slot, y, x, strlen;
  char str[MAX_STRLEN];

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  if( arity == 3 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &strlen);
    ei_decode_string(st->args, &(st->index), str);
    encode_ok_reply(st, winsnstr(st->win[slot], str, (int)strlen));
  } else if( arity == 5 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &strlen);
    ei_decode_string(st->args, &(st->index), str);
    encode_ok_reply(
        st, mvwinsnstr(st->win[slot], (int)y, (int)x, str, (int)strlen));
  }
}


// do attribute settings
void do_color_set(state *st) {
  int arity;
  long slot, pair;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &pair);
  encode_ok_reply(st, wcolor_set(st->win[slot], (short)pair, NULL));
}

void do_attrset(state *st) {
  int arity;
  long slot, attr;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attr);
  encode_ok_reply(st, wattrset(st->win[slot], (int)attr));
}

void do_attroff(state *st) {
  int arity;
  long slot, attr;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attr);
  encode_ok_reply(st, wattroff(st->win[slot], (int)attr));
}

void do_attron(state *st) {
  int arity;
  long slot, attr;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attr);
  encode_ok_reply(st, wattron(st->win[slot], (int)attr));
}

void do_attr_get(state *st) {
  int arity, code;
  long slot;
  attr_t attr;
  short colorpair;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  code = wattr_get(st->win[slot], &attr, &colorpair, (void *)NULL); 
  encode_attr_get_reply(st, code, (int)attr, colorpair);
}

void do_chgat(state *st) {
  int arity;
  long slot, y, x, n, attr, pair;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  if( arity == 4 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &n);
    ei_decode_long(st->args, &(st->index), &attr);
    ei_decode_long(st->args, &(st->index), &pair);
    encode_ok_reply(st, 
      wchgat(st->win[slot], (int)n, (attr_t)attr, (short)pair,
             (const void*)NULL));
  } else if( arity == 6 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &n);
    ei_decode_long(st->args, &(st->index), &attr);
    ei_decode_long(st->args, &(st->index), &pair);
    encode_ok_reply( st,
      mvwchgat(st->win[slot], (int)y, (int)x,
               (int)n, (attr_t)attr, (short)pair, (const void*)NULL));
  } 
}

void do_has_colors(state *st) {     // do color settings
  boolean(st, has_colors());
}

void do_can_change_color(state *st) {
  boolean(st, can_change_color());
}

void do_start_color(state *st) {
  encode_ok_reply(st, start_color());
}

void do_init_pair(state *st) {
  int arity;
  long pairnum, fcolor, bcolor;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &pairnum);
  ei_decode_long(st->args, &(st->index), &fcolor);
  ei_decode_long(st->args, &(st->index), &bcolor);
  encode_ok_reply(st, init_pair((int)pairnum, (int)fcolor, (int)bcolor));
}

void do_color_content(state *st) {
  long n;
  short r, g, b;
  int code;
  ei_decode_long(st->args, &(st->index), &n);
  code = color_content(n, &r, &g, &b);
  if (code == OK) {
    tuple(&(st->eixb), 3);
    integer(&(st->eixb), r);
    integer(&(st->eixb), g);
    integer(&(st->eixb), b);
  } else {
    error_tuple(st, code);
  }
}


// input related
void do_ungetch(state *st) {
  int ch;
  ei_decode_long(st->args, &(st->index), (long *)&ch);
  encode_ok_reply(st, ungetch(ch));
}

void do_has_key(state *st) {
  int ch;
  ei_decode_long(st->args, &(st->index), (long *)&ch);
  boolean(st, has_key(ch));
}

void do_inch(state *st) {
  int arity;
  long slot, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  if( arity == 1 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    integer(&(st->eixb), (int)winch(st->win[slot]));
  } else if( arity == 3 ) {
    ei_decode_long(st->args, &(st->index), &slot);
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    integer(&(st->eixb), (int)mvwinch(st->win[slot], y, x));
  } 
}

void do_innstr(state *st) {
  int arity;
  long slot, y, x, n;
  char str[MAX_STRLEN];
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity == 2 ) {
    ei_decode_long(st->args, &(st->index), &n);
    winnstr(st->win[slot], str, n);
    str[n] = 0;
    encode_string_reply(st, str);
  } else if( arity == 4 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &n);
    mvwinnstr(st->win[slot], y, x, str, n);
    str[n] = 0;
    encode_string_reply(st, str);
  } 
}

void do_inchnstr(state *st) {
  int arity, code, i;
  long slot, y, x, n;
  chtype str_attr[MAX_STRLEN];
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity == 2 ) {
    ei_decode_long(st->args, &(st->index), &n);
    code = winchnstr(st->win[slot], str_attr, n);
  } else if( arity == 4 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &n);
    code = mvwinchnstr(st->win[slot], y, x, str_attr, n);
  } 
  if( code == OK ) {
    list(&(st->eixb), n);
    for(i=0; i<n; i++)
        integer(&(st->eixb), (int)str_attr[i]);
    empty_list(&(st->eixb));
  } else {
    error_tuple(st, code);
  }
}

void do_wmove(state *st) {
  int arity;
  long slot, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  encode_ok_reply(st, wmove(st->win[slot], (int)y, (int)x));
}


void do_curs_set(state *st) {
  long flag;
  ei_decode_long(st->args, &(st->index), &flag);
  curs_set((int)flag);
  ok(st);
}

void do_wattron(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattron(st->win[slot], (int)attrs));
}

void do_wattroff(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattroff(st->win[slot], (int)attrs));
}

void do_nl(state *st) {
  encode_ok_reply(st, nl());
}

void do_nonl(state *st) {
  encode_ok_reply(st, nonl());
}

void do_scrollok(state *st) {
  int arity;
  int bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, scrollok(st->win[slot], bf));
}

void do_newwin(state *st) {
  int slot = findfreewindowslot(st);
  if (slot > 0) {
    int arity;
    long height, width, starty, startx;
    ei_decode_tuple_header(st->args, &(st->index), &arity);
    ei_decode_long(st->args, &(st->index), &height);
    ei_decode_long(st->args, &(st->index), &width);
    ei_decode_long(st->args, &(st->index), &starty);
    ei_decode_long(st->args, &(st->index), &startx);
    st->win[slot] = newwin(height, width, starty, startx);
    integer(&(st->eixb), slot);
  } else {
    integer(&(st->eixb), -1);
  }
}

void do_delwin(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  if (slot == 0) {
    boolean(st, FALSE);
  } else if (st->win[slot] == NULL) {
    boolean(st, FALSE);
  } else if (st->win[slot] != NULL) {
    delwin(st->win[slot]);
    st->win[slot] = NULL;
    boolean(st, TRUE);
  }
}

void do_hline(state *st) {
  int arity;
  long slot, ch, y, x, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity==3 ) {
    ei_decode_long(st->args, &(st->index), &ch);
    ei_decode_long(st->args, &(st->index), &max);
    encode_ok_reply(st, whline(st->win[slot], (chtype)ch, (int)max));
  } else if ( arity==5 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &ch);
    ei_decode_long(st->args, &(st->index), &max);
    encode_ok_reply(st,
        mvwhline(st->win[slot], (int)y, (int)x, (chtype)ch, (int)max));
  }
}

void do_vline(state *st) {
  int arity;
  long slot, ch, y, x, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  if( arity==3 ) {
    ei_decode_long(st->args, &(st->index), &ch);
    ei_decode_long(st->args, &(st->index), &max);
    encode_ok_reply(st, wvline(st->win[slot], (chtype)ch, (int)max));
  } else if( arity==5 ) {
    ei_decode_long(st->args, &(st->index), &y);
    ei_decode_long(st->args, &(st->index), &x);
    ei_decode_long(st->args, &(st->index), &ch);
    ei_decode_long(st->args, &(st->index), &max);
    encode_ok_reply(st,
        mvwvline(st->win[slot], (int)y, (int)x, (chtype)ch, (int)max));
  }
}

void do_wborder(state *st) {
  int arity;
  long slot, ls, rs, ts, bs, tl, tr, bl, br;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ls);
  ei_decode_long(st->args, &(st->index), &rs);
  ei_decode_long(st->args, &(st->index), &ts);
  ei_decode_long(st->args, &(st->index), &bs);
  ei_decode_long(st->args, &(st->index), &tl);
  ei_decode_long(st->args, &(st->index), &tr);
  ei_decode_long(st->args, &(st->index), &bl);
  ei_decode_long(st->args, &(st->index), &br);
  encode_ok_reply(st, wborder(st->win[slot], (chtype)ls, (chtype)rs, (chtype)ts,
			      (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl,
			      (chtype)br));
}

void do_box(state *st) {
  int arity;
  long slot, verch, horch;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &verch);
  ei_decode_long(st->args, &(st->index), &horch);
  encode_ok_reply(st, box(st->win[slot], (chtype)verch, (chtype)horch));
}


// =============================================================================
// Utility functions
// =============================================================================
void init_state(state *st, char *args, int argslen) {
  st->index = 0;
  st->version = 0;
  st->args = args;
  st->argslen = argslen;
  ei_decode_version(st->args, &(st->index), &(st->version));
  assert(st->version != 0);
  assert(st->index != 0);
  ei_x_new_with_version(&(st->eixb));
}

void ok(state *st) {
  atom(&(st->eixb), "ok", 2);
}

void error_tuple(state *st, int code) {
  tuple(&(st->eixb), 2);
  atom(&(st->eixb), "error", 5);
  integer(&(st->eixb), code);
}

void boolean(state *st, int code) {
  if (code == TRUE)
    atom(&(st->eixb),"true",4);
  else
    atom(&(st->eixb),"false",5);
}

void tuple(ei_x_buff *eixb, int size) {
  ei_x_encode_tuple_header(eixb, size);
}

void list(ei_x_buff *eixb, int len) {
  ei_x_encode_list_header(eixb, len);
}

void empty_list(ei_x_buff *eixb) {
  ei_x_encode_empty_list(eixb);
}

void atom(ei_x_buff *eixb, const char *str, int size) {
  ei_x_encode_atom_len(eixb, str, size);
}

void integer(ei_x_buff *eixb, int integer) {
  ei_x_encode_long(eixb, (long)integer);
}

void string(ei_x_buff *eixb, const char *str) {
  ei_x_encode_string(eixb, str);
}

void encode_ok_reply(state *st, int code) {
  if (code == OK) {
    ok(st);
  } else {
    error_tuple(st, code);
  }
}

void encode_string_reply(state *st, const char* str) {
  if (str == NULL) {
    error_tuple(st, 1);
  } else {
    string(&(st->eixb), str);
  }
}

void encode_attr_get_reply(state *st, int code, int attr, int colorpair) {
  if (code == OK) {
    tuple(&(st->eixb), 3);
    atom(&(st->eixb), "ok", 2);
    integer(&(st->eixb), attr);
    integer(&(st->eixb), colorpair);
  } else {
    error_tuple(st, code);
  }
}

int findfreewindowslot(state *st) {
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    if (st->win[i] == NULL) return i;
  return -1;
}

// =============================================================================
// Erlang driver_entry Specification
// ===========================================================================
ErlDrvEntry driver_entry = {
  NULL,
  start,
  stop,
  NULL,
  do_getch,
  NULL,
  "ncdrv",
  NULL,
  NULL,
  control,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION
};

// =============================================================================
// Erlang Driver Name
// =============================================================================
DRIVER_INIT(ncdrv) {
  return &driver_entry;
}
