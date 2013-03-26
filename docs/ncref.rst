* use `-lncurses` option to link your ncurses application with ncurses library.
  Linking with `-lncurses_g` generates a trace log.

* the ncurses package supports:

  * overall screen, window and pad manipulation;
  * output to windows and pads;
  * reading terminal  input;
  * control  over terminal and curses input and output options;
  * environment query routines;
  * color manipulation;
  * use of soft label keys;
  * terminfo capabilities;
  * access to low-level terminal-manipulation routines.

* **ncurses**, the "normal" library, which handles 8-bit characters. The normal
  (8-bit)  library  stores  characters combined  with attributes in chtype
  data.

* **ncursesw**, the so-called "wide" library, which handles multibyte characters
  (see the section on ALTERNATE CONFIGURATIONS). The "wide" library includes
  all of the calls from the "normal" library.  It adds about one third more
  calls using data types which store multibyte characters.
                                                         
* function `initscr()` or `newterm()` must be called to initialize the library
  before any of the other routines that deal with windows and screens are used.
  The routine `endwin()` must be called before exiting.

* before a curses program is run, the `tab stops` of the terminal should be set
  and its initialization strings, if defined, must be out‐put. This can be done
  by executing the ``tput init`` command after the shell environment variable
  TERM has been exported.

* The routines prefixed with `w` require a window argument, those prefixed with
  `mv` moves cursor to y,x co-ordinate, those prefixed with `p` require a pad
  argument. Those without a prefix generally use `stdscr`.
  
* (y,x) = (row, column), upper left corner is always (0,0).

screen manipulation:
--------------------

* the routines not beginning with `w` affect stdscr.

* this means that you can either use stdscr or divide the screen into tiled
  windows and not using stdscr at all.

* `baudrate()` returns the output speed of the terminal in bits per second. 

* `erasechar()`, with variant `killchar()`, with its prefix `w` can be used to
  identify the user's current erase or kill character.

* `has_ic()` with its `_il` variant, checks if the terminal has insert/delete
  character/line capabilities.

* `longname()`, gives verbose description of the current terminal.

* `beep()`, `flash()`

* `cbreak()`, `nocbreak()` to get, not get character wise input.

* `echo()`, `noecho()`, to get echo, not echo characters as it gets typed.

* `halfdelay()`, `nodelay()`, `notimeout()`, `timeout()`, `wtimeout()` sets
  whether input should block and its timeout.

* `raw()`, `noraw()`, get interrupt, quite, suspend signals are not.

* `noqiflush()`, `qiflush()`, `intrflush()`, `keypad()`, `meta()`,
  `typeahead()` refer to their man pages.

* `clearok()`, `idlok()`, `idcok()`, `immedok()`, `leaveok()`,
  `setscrreg()`, `wsetscrreg()`, `scrollok()` window scroll region
  `nl()`, `nonl()` interpret new lines or not

window manipulation:
--------------------

* `border()`, with `w` prefix can draw a border for window, using ls, rs, ts,
  bs, tl, tr, bl, br. The `_set` variant does not move the curser and do not
  wrap.

* `box()`, a more simpler version of border. The `_set` variant does not move
  the cursor and do not wrap.

* `hline()` and `vline()`, with its `mv` and `w` prefixes can draw lines. The
  `_set` variant does not move the cursor and do not wrap.

pad manipulation:
-----------------

* these are windows which are not constrained to the size of the screen and
  whose contents need not be completely displayed.

rendering and color:
-------------------

* the characters  in a window are actually of type `chtype`, (character and
  attribute data) so that other information about the character, like video
  attributes and color, may also be stored with each character.

* video attributes begin with `A_` like in `A_REVERSE`, line drawing characters
  begin with `ACS_` like in `ACS_HLINE`, defined in ``curses.h``

* attributes are a property of the character, and move with the character
  through any scrolling and insert/delete  line/character operations. 

* `addch()`, with variants `_wch`, prefixes `mv`, `w`, adds character at
  current position.

* `addstr()`, with variants `wstr`, prefixes `mv`, `w`, adds character at
  current position.

* `addnstr()`, with variants `wstr`, prefixes `mv`, `w`, adds character at
  current position.

* `addchstr()`, with variants `_wch`, prefixes `mv`, `w`, adds character at
  current position. does not move the cursor position.

* `addchnstr()`, with variants `_wch`, prefixes `mv`, `w`, adds character at
  current position. does not move the cursor position.

* `attr_get()`, `attrset()`, `attron()`, `attroff()`, with its `w` prefix gets,
  sets, adds and removes current window attribute.

* `attr_off()`, `attr_on()`, `attr_set()`, same as above variants, except that
  work with attr_t instead of int type.

* A_NORMAL, A_STANDOUT, A_UNDERLINE, A_REVERSE, A_BLINK, A_DIM, A_BOLD,
  A_PROTECT, A_INVIS, A_ALTCHARSET, A_CHARTEXT.

* `color_set()`, with its `w`, sets the current color of the given window to
  the foreground/background combination described by the color_pair_number.

* `chgat()`, changes the attributes of a given number of characters starting
  at the current cursor location of stdscr. It does not update the cursor
  and does not perform wrapping. Refer to its man page for more details.

* `erase()` and `werase()` routines copy blanks to every position in the
  window, clearing the screen.

* `clear()`  and  `wclear()` routines are like erase and werase, but they also
  call clearok, so that the screen is cleared completely on the next call to
  wrefresh for that window and repainted from scratch.

* `clrtobot()` and `wclrtobot()` routines erase from the cursor to the end of
  screen.  That is, they erase all lines below the cursor in the window.
  Also, the current line to the right of the cursor, inclusive, is erased.

* `clrtoeol()` and `wclrtoeol()` routines erase the current line to the right
  of the cursor, inclusive, to the end of the current line.

**colors**

* `has_color()` can tells wether color is supported or not.

* ``color``, there can be COLORS number of maximum colors. `init_color()` if
  allowed, use `can_change_color()` to know that, can initialize a color
  number to sepcified RGB values. `color_content()` is like a reverse if
  init_color without any side effects.

* ``color_pair``, is foreground,background. A color-pair is identified by a
  number and can be initialised with `init_pair()` routine. And COLOR_PAIR(n)
  macro can be used for video-attribute. PAIR_NUMBER(attrs) is reverse of
  COLOR_PAIR.

* `start_color()` to use color, must be called right after initscr.

* `use_default_colors()`  tells the curses library to
  assign terminal default foreground/background colors to color number -1.
  so init_pair(x,COLOR_RED,-1) will initialize pair x as red on default
  background and init_pair(x,-1,COLOR_BLUE) will initialize pair x as
  default foreground on blue.

* `assume_default_colors()` is  a refinement which tells which
  colors to paint for color pair 0.  This function recognizes a special color
  number -1, which denotes the default terminal color.


inputs:
-------
 
* curses to translate arrow and function keys that transmit escape sequences
  into single values.

* input values begin with begin with `KEY_` like in `KEY_LEFT`, defined in
  ``curses.h``

* `getch()`, and its `w` and `mv` variant.

* `ungetch()`, and its `w` and `mv` variant. There is only one input queue for
   all windows.

* `has_key()`.

environment query:
------------------

Looks like background is not that straightforward.
                                   
       * refresh(3NCURSES)        doupdate                
       * refresh(3NCURSES)        refresh                 
       * refresh(3NCURSES)        wnoutrefresh            
       * refresh(3NCURSES)        wrefresh                
         refresh(3NCURSES)        redrawwin               
         refresh(3NCURSES)        wredrawln               

       * inopts(3NCURSES)         raw                     
       * inopts(3NCURSES)         noraw                   
       * inopts(3NCURSES)         cbreak                  
       * inopts(3NCURSES)         nocbreak                
       * inopts(3NCURSES)         echo                    
       * inopts(3NCURSES)         noecho                  
       * inopts(3NCURSES)         timeout                 
       * inopts(3NCURSES)         wtimeout                
       * inopts(3NCURSES)         notimeout               
       * inopts(3NCURSES)         nodelay                 
       * inopts(3NCURSES)         halfdelay               
       * inopts(3NCURSES)         keypad                  
         inopts(3NCURSES)         typeahead               
         inopts(3NCURSES)         intrflush               
         inopts(3NCURSES)         meta                    
         inopts(3NCURSES)         noqiflush               
         inopts(3NCURSES)         qiflush                 
         
       * addch(3NCURSES)          addch                   
       * addch(3NCURSES)          mvaddch                 
       * addch(3NCURSES)          mvwaddch                
       * addch(3NCURSES)          waddch                  
       * addch(3NCURSES)          echochar                
       * addch(3NCURSES)          wechochar               

         add_wch(3NCURSES)        add_wch                 
         add_wch(3NCURSES)        echo_wchar              
         add_wch(3NCURSES)        mvadd_wch               
         add_wch(3NCURSES)        mvwadd_wch              
         add_wch(3NCURSES)        wadd_wch                
         add_wch(3NCURSES)        wecho_wchar             

       * addstr(3NCURSES)         addstr                  
       * addstr(3NCURSES)         waddstr                 
       * addstr(3NCURSES)         mvaddstr                
       * addstr(3NCURSES)         mvwaddstr               
       * addstr(3NCURSES)         addnstr                 
       * addstr(3NCURSES)         waddnstr                
       * addstr(3NCURSES)         mvaddnstr               
       * addstr(3NCURSES)         mvwaddnstr              

         addwstr(3NCURSES)        addnwstr                
         addwstr(3NCURSES)        addwstr                 
         addwstr(3NCURSES)        mvaddnwstr              
         addwstr(3NCURSES)        mvaddwstr               
         addwstr(3NCURSES)        mvwaddnwstr             
         addwstr(3NCURSES)        mvwaddwstr              
         addwstr(3NCURSES)        waddnwstr               
         addwstr(3NCURSES)        waddwstr                

       * addchstr(3NCURSES)       addchstr                
       * addchstr(3NCURSES)       waddchstr               
       * addchstr(3NCURSES)       mvaddchstr              
       * addchstr(3NCURSES)       mvwaddchstr             
       * addchstr(3NCURSES)       addchnstr               
       * addchstr(3NCURSES)       waddchnstr              
       * addchstr(3NCURSES)       mvaddchnstr             
       * addchstr(3NCURSES)       mvwaddchnstr            

         add_wchstr(3NCURSES)     add_wchnstr             
         add_wchstr(3NCURSES)     add_wchstr              
         add_wchstr(3NCURSES)     mvadd_wchnstr           
         add_wchstr(3NCURSES)     mvadd_wchstr            
         add_wchstr(3NCURSES)     mvwadd_wchnstr          
         add_wchstr(3NCURSES)     mvwadd_wchstr           
         add_wchstr(3NCURSES)     wadd_wchnstr            
         add_wchstr(3NCURSES)     wadd_wchstr             

       * attr(3NCURSES)           color_set               
       * attr(3NCURSES)           wcolor_set              
       * attr(3NCURSES)           attrset                 
       * attr(3NCURSES)           wattrset                
       * attr(3NCURSES)           attroff                 
       * attr(3NCURSES)           wattroff                
       * attr(3NCURSES)           attron                  
       * attr(3NCURSES)           wattron                 
       * attr(3NCURSES)           PAIR_NUMBER             
       * attr(3NCURSES)           attr_get                
       * attr(3NCURSES)           wattr_get               
       * attr(3NCURSES)           chgat                   
       * attr(3NCURSES)           mvchgat                 
       * attr(3NCURSES)           mvwchgat                
       * attr(3NCURSES)           wchgat                  
         attr(3NCURSES)           standend                
         attr(3NCURSES)           standout                
         attr(3NCURSES)           wstandend               
         attr(3NCURSES)           wstandout               
         attr(3NCURSES)           attr_off                
         attr(3NCURSES)           wattr_off               
         attr(3NCURSES)           attr_on                 
         attr(3NCURSES)           wattr_on                
         attr(3NCURSES)           attr_set                
         attr(3NCURSES)           wattr_set               
         attr(3NCURSES)           getattrs                

       * beep(3NCURSES)           beep                    
       * beep(3NCURSES)           flash                   

         bkgd(3NCURSES)           bkgd                    
         bkgd(3NCURSES)           wbkgd                   
         bkgd(3NCURSES)           bkgdset                 
         bkgd(3NCURSES)           wbkgdset                
         bkgd(3NCURSES)           getbkgd                 

         bkgrnd(3NCURSES)         bkgrnd                  
         bkgrnd(3NCURSES)         bkgrndset               
         bkgrnd(3NCURSES)         getbkgrnd               
         bkgrnd(3NCURSES)         wbkgrnd                 
         bkgrnd(3NCURSES)         wbkgrndset              
         bkgrnd(3NCURSES)         wgetbkgrnd              

         border(3NCURSES)         border                  
         border(3NCURSES)         box                     
         border(3NCURSES)         hline                   
         border(3NCURSES)         mvhline                 
         border(3NCURSES)         mvvline                 
         border(3NCURSES)         mvwhline                
         border(3NCURSES)         mvwvline                
         border(3NCURSES)         vline                   
         border(3NCURSES)         wborder                 
         border(3NCURSES)         whline                  
         border(3NCURSES)         wvline                  

         border_set(3NCURSES)     border_set              
         border_set(3NCURSES)     box_set                 
         border_set(3NCURSES)     hline_set               
         border_set(3NCURSES)     mvhline_set             
         border_set(3NCURSES)     mvvline_set             
         border_set(3NCURSES)     mvwhline_set            
         border_set(3NCURSES)     mvwvline_set            
         border_set(3NCURSES)     vline_set               
         border_set(3NCURSES)     wborder_set             
         border_set(3NCURSES)     whline_set              
         border_set(3NCURSES)     wvline_set              

       * clear(3NCURSES)          erase                   
       * clear(3NCURSES)          werase                  
       * clear(3NCURSES)          clear                   
       * clear(3NCURSES)          wclear                  
       * clear(3NCURSES)          clrtobot                
       * clear(3NCURSES)          wclrtobot               
       * clear(3NCURSES)          clrtoeol                
       * clear(3NCURSES)          wclrtoeol               

       * color(3NCURSES)          has_colors              
       * color(3NCURSES)          start_color             
       * color(3NCURSES)          init_pair               
         color(3NCURSES)          can_change_color        
         color(3NCURSES)          init_color              
         color(3NCURSES)          color_content           
         color(3NCURSES)          pair_content            
         color(3NCURSES)          COLOR_PAIR              

         default_colors(3NCURSES)*assume_default_colors   
         default_colors(3NCURSES)*use_default_colors      

         define_key(3NCURSES)*    define_key              

       * delch(3NCURSES)          delch                   
       * delch(3NCURSES)          mvdelch                 
       * delch(3NCURSES)          mvwdelch                
       * delch(3NCURSES)          wdelch                  

       * deleteln(3NCURSES)       deleteln                
       * deleteln(3NCURSES)       insdelln                
       * deleteln(3NCURSES)       insertln                
       * deleteln(3NCURSES)       wdeleteln               
       * deleteln(3NCURSES)       winsdelln               
       * deleteln(3NCURSES)       winsertln               

       * extensions(3NCURSES)*    curses_version          
         extensions(3NCURSES)*    use_extended_names      

         get_wch(3NCURSES)        get_wch                 
         get_wch(3NCURSES)        mvget_wch               
         get_wch(3NCURSES)        mvwget_wch              
         get_wch(3NCURSES)        unget_wch               
         get_wch(3NCURSES)        wget_wch                

         get_wstr(3NCURSES)       get_wstr                
         get_wstr(3NCURSES)       getn_wstr               
         get_wstr(3NCURSES)       mvget_wstr              
         get_wstr(3NCURSES)       mvgetn_wstr             
         get_wstr(3NCURSES)       mvwget_wstr             
         get_wstr(3NCURSES)       mvwgetn_wstr            
         get_wstr(3NCURSES)       wget_wstr               
         get_wstr(3NCURSES)       wgetn_wstr              

         getcchar(3NCURSES)       getcchar                
         getcchar(3NCURSES)       setcchar                

       * getch(3NCURSES)          getch                   
         getch(3NCURSES)          wgetch                  
         getch(3NCURSES)          mvgetch                 
         getch(3NCURSES)          mvwgetch                
       * getch(3NCURSES)          ungetch                 
       * getch(3NCURSES)*         has_key                 

         getstr(3NCURSES)         getnstr                 
         getstr(3NCURSES)         getstr                  
         getstr(3NCURSES)         mvgetnstr               
         getstr(3NCURSES)         mvgetstr                
         getstr(3NCURSES)         mvwgetnstr              
         getstr(3NCURSES)         mvwgetstr               
         getstr(3NCURSES)         wgetnstr                
         getstr(3NCURSES)         wgetstr                 

       * getyx(3NCURSES)          getbegyx                
       * getyx(3NCURSES)          getmaxyx                
       * getyx(3NCURSES)          getparyx                
       * getyx(3NCURSES)          getyx                   

       * inch(3NCURSES)           inch                    
       * inch(3NCURSES)           mvinch                  
       * inch(3NCURSES)           mvwinch                 
       * inch(3NCURSES)           winch                   

         in_wch(3NCURSES)         in_wch                  
         in_wch(3NCURSES)         mvin_wch                
         in_wch(3NCURSES)         mvwin_wch               
         in_wch(3NCURSES)         win_wch                 

       * instr(3NCURSES)          innstr                  
       * instr(3NCURSES)          winnstr                 
       * instr(3NCURSES)          mvinnstr                
       * instr(3NCURSES)          mvwinnstr               
         instr(3NCURSES)          instr                   
         instr(3NCURSES)          winstr                  
         instr(3NCURSES)          mvinstr                 
         instr(3NCURSES)          mvwinstr                

         inwstr(3NCURSES)         innwstr                 
         inwstr(3NCURSES)         inwstr                  
         inwstr(3NCURSES)         mvinnwstr               
         inwstr(3NCURSES)         mvinwstr                
         inwstr(3NCURSES)         mvwinnwstr              
         inwstr(3NCURSES)         mvwinwstr               
         inwstr(3NCURSES)         winnwstr                
         inwstr(3NCURSES)         winwstr                 

       * inchstr(3NCURSES)        inchnstr                
       * inchstr(3NCURSES)        winchnstr               
       * inchstr(3NCURSES)        mvinchnstr              
       * inchstr(3NCURSES)        mvwinchnstr             
         inchstr(3NCURSES)        inchstr                 
         inchstr(3NCURSES)        winchstr                
         inchstr(3NCURSES)        mvinchstr               
         inchstr(3NCURSES)        mvwinchstr              

         in_wchstr(3NCURSES)      in_wchnstr              
         in_wchstr(3NCURSES)      in_wchstr               
         in_wchstr(3NCURSES)      mvin_wchnstr            
         in_wchstr(3NCURSES)      mvin_wchstr             
         in_wchstr(3NCURSES)      mvwin_wchnstr           
         in_wchstr(3NCURSES)      mvwin_wchstr            
         in_wchstr(3NCURSES)      win_wchnstr             
         in_wchstr(3NCURSES)      win_wchstr              

       * insch(3NCURSES)          insch                   
       * insch(3NCURSES)          mvinsch                 
       * insch(3NCURSES)          mvwinsch                
       * insch(3NCURSES)          winsch                  

         ins_wch(3NCURSES)        ins_wch                 
         ins_wch(3NCURSES)        mvins_wch               
         ins_wch(3NCURSES)        mvwins_wch              
         ins_wch(3NCURSES)        wins_wch                

       * insstr(3NCURSES)         insstr                  
       * insstr(3NCURSES)         winsstr                 
       * insstr(3NCURSES)         mvinsstr                
       * insstr(3NCURSES)         mvwinsstr               
       * insstr(3NCURSES)         insnstr                 
       * insstr(3NCURSES)         winsnstr                
       * insstr(3NCURSES)         mvinsnstr               
       * insstr(3NCURSES)         mvwinsnstr              

         ins_wstr(3NCURSES)       ins_nwstr               
         ins_wstr(3NCURSES)       ins_wstr                
         ins_wstr(3NCURSES)       mvins_nwstr             
         ins_wstr(3NCURSES)       mvins_wstr              
         ins_wstr(3NCURSES)       mvwins_nwstr            
         ins_wstr(3NCURSES)       mvwins_wstr             
         ins_wstr(3NCURSES)       wins_nwstr              
         ins_wstr(3NCURSES)       wins_wstr               

         initscr(3NCURSES)        delscreen               
         initscr(3NCURSES)        endwin                  
         initscr(3NCURSES)        initscr                 
         initscr(3NCURSES)        isendwin                
         initscr(3NCURSES)        newterm                 
         initscr(3NCURSES)        set_term                

         kernel(3NCURSES)         curs_set                
         kernel(3NCURSES)         def_prog_mode           
         kernel(3NCURSES)         def_shell_mode          
         kernel(3NCURSES)         getsyx                  
         kernel(3NCURSES)         napms                   
         kernel(3NCURSES)         reset_prog_mode         
         kernel(3NCURSES)         reset_shell_mode        
         kernel(3NCURSES)         resetty                 
         kernel(3NCURSES)         ripoffline              
         kernel(3NCURSES)         savetty                 
         kernel(3NCURSES)         setsyx                  

         key_defined(3NCURSES)*   key_defined             
         keybound(3NCURSES)*      keybound                
         keyok(3NCURSES)*         keyok                   

         legacy(3NCURSES)*        getbegx                 
         legacy(3NCURSES)*        getbegy                 
         legacy(3NCURSES)*        getcurx                 
         legacy(3NCURSES)*        getcury                 
         legacy(3NCURSES)*        getmaxx                 
         legacy(3NCURSES)*        getmaxy                 
         legacy(3NCURSES)*        getparx                 
         legacy(3NCURSES)*        getpary                 

         legacy_coding(3NCURSES)* use_legacy_coding       

         memleaks(3NCURSES)*      _nc_free_and_exit       
         memleaks(3NCURSES)*      _nc_freeall             

         mouse(3NCURSES)*         getmouse                
         mouse(3NCURSES)*         mouse_trafo             
         mouse(3NCURSES)*         mouseinterval           
         mouse(3NCURSES)*         mousemask               
         mouse(3NCURSES)*         ungetmouse              
         mouse(3NCURSES)*         wenclose                
         mouse(3NCURSES)*         wmouse_trafo            

         move(3NCURSES)           move                    
         move(3NCURSES)           wmove                   

         opaque(3NCURSES)*        is_cleared              
         opaque(3NCURSES)*        is_idcok                
         opaque(3NCURSES)*        is_idlok                
         opaque(3NCURSES)*        is_immedok              
         opaque(3NCURSES)*        is_keypad               
         opaque(3NCURSES)*        is_leaveok              
         opaque(3NCURSES)*        is_nodelay              
         opaque(3NCURSES)*        is_notimeout            
         opaque(3NCURSES)*        is_scrollok             
         opaque(3NCURSES)*        is_syncok               

         outopts(3NCURSES)        clearok                 
         outopts(3NCURSES)        idcok                   
         outopts(3NCURSES)        idlok                   
         outopts(3NCURSES)        immedok                 
         outopts(3NCURSES)        leaveok                 
         outopts(3NCURSES)        nl                      
         outopts(3NCURSES)        nonl                    
         outopts(3NCURSES)        scrollok                
         outopts(3NCURSES)        setscrreg               
         outopts(3NCURSES)        wsetscrreg              

         overlay(3NCURSES)        copywin                 
         overlay(3NCURSES)        overlay                 
         overlay(3NCURSES)        overwrite               

         pad(3NCURSES)            newpad                  
         pad(3NCURSES)            pechochar               
         pad(3NCURSES)            pnoutrefresh            
         pad(3NCURSES)            prefresh                
         pad(3NCURSES)            subpad                  

         print(3NCURSES)*         mcprint                 
         printw(3NCURSES)         mvprintw                
         printw(3NCURSES)         mvwprintw               
         printw(3NCURSES)         printw                  
         printw(3NCURSES)         vw_printw               
         printw(3NCURSES)         vwprintw                
         printw(3NCURSES)         wprintw                 

         resizeterm(3NCURSES)*    is_term_resized         
         resizeterm(3NCURSES)*    resizeterm              

         scanw(3NCURSES)          mvscanw                 
         scanw(3NCURSES)          mvwscanw                
         scanw(3NCURSES)          scanw                   
         scanw(3NCURSES)          vw_scanw                
         scanw(3NCURSES)          vwscanw                 
         scanw(3NCURSES)          wscanw                  

         scr_dump(3NCURSES)       scr_dump                
         scr_dump(3NCURSES)       scr_init                
         scr_dump(3NCURSES)       scr_restore             
         scr_dump(3NCURSES)       scr_set                 

         scroll(3NCURSES)         scrl                    
         scroll(3NCURSES)         scroll                  
         scroll(3NCURSES)         wscrl                   

         slk(3NCURSES)            slk_attr_off            
         slk(3NCURSES)            slk_attr_on             
         slk(3NCURSES)            slk_attr_set            
         slk(3NCURSES)            slk_attroff             
         slk(3NCURSES)            slk_attron              
         slk(3NCURSES)            slk_attrset             
         slk(3NCURSES)            slk_clear               
         slk(3NCURSES)            slk_color               
         slk(3NCURSES)            slk_init                
         slk(3NCURSES)            slk_label               
         slk(3NCURSES)            slk_noutrefresh         
         slk(3NCURSES)            slk_refresh             
         slk(3NCURSES)            slk_restore             
         slk(3NCURSES)            slk_set                 
         slk(3NCURSES)            slk_touch               
         slk(3NCURSES)*           slk_attr                

         termattrs(3NCURSES)      baudrate                
         termattrs(3NCURSES)      erasechar               
         termattrs(3NCURSES)      erasewchar              
         termattrs(3NCURSES)      has_ic                  
         termattrs(3NCURSES)      has_il                  
         termattrs(3NCURSES)      killchar                
         termattrs(3NCURSES)      killwchar               
         termattrs(3NCURSES)      longname                
         termattrs(3NCURSES)      term_attrs              
         termattrs(3NCURSES)      termattrs               
         termattrs(3NCURSES)      termname                

         termcap(3NCURSES)        tgetent                 
         termcap(3NCURSES)        tgetflag                
         termcap(3NCURSES)        tgetnum                 
         termcap(3NCURSES)        tgetstr                 
         termcap(3NCURSES)        tgoto                   
         termcap(3NCURSES)        tputs                   

         terminfo(3NCURSES)       del_curterm             
         terminfo(3NCURSES)       mvcur                   
         terminfo(3NCURSES)       putp                    
         terminfo(3NCURSES)       restartterm             
         terminfo(3NCURSES)       set_curterm             
         terminfo(3NCURSES)       setterm                 
         terminfo(3NCURSES)       setupterm               
         terminfo(3NCURSES)       tigetflag               
         terminfo(3NCURSES)       tigetnum                
         terminfo(3NCURSES)       tigetstr                
         terminfo(3NCURSES)       tparm                   
         terminfo(3NCURSES)       tputs                   
         terminfo(3NCURSES)       vid_attr                
         terminfo(3NCURSES)       vid_puts                
         terminfo(3NCURSES)       vidattr                 
         terminfo(3NCURSES)       vidputs                 

         touch(3NCURSES)          is_linetouched          
         touch(3NCURSES)          is_wintouched           
         touch(3NCURSES)          touchline               
         touch(3NCURSES)          touchwin                
         touch(3NCURSES)          untouchwin              
         touch(3NCURSES)          wtouchln                

         trace(3NCURSES)*         _nc_tracebits           
         trace(3NCURSES)*         _traceattr              
         trace(3NCURSES)*         _traceattr2             
         trace(3NCURSES)*         _tracechar              
         trace(3NCURSES)*         _tracechtype            
         trace(3NCURSES)*         _tracechtype2           
         trace(3NCURSES)*         _tracedump              
         trace(3NCURSES)*         _tracef                 
         trace(3NCURSES)*         _tracemouse             
         trace(3NCURSES)*         trace                   

         util(3NCURSES)           delay_output            
         util(3NCURSES)           filter                  
         util(3NCURSES)           flushinp                
         util(3NCURSES)           getwin                  
         util(3NCURSES)           key_name                
         util(3NCURSES)           keyname                 
         util(3NCURSES)           putwin                  
         util(3NCURSES)           unctrl                  
         util(3NCURSES)           use_env                 
         util(3NCURSES)           wunctrl                 
         util(3NCURSES)*          nofilter                

         window(3NCURSES)         delwin                  
         window(3NCURSES)         derwin                  
         window(3NCURSES)         dupwin                  
         window(3NCURSES)         mvderwin                
         window(3NCURSES)         mvwin                   
         window(3NCURSES)         newwin                  
         window(3NCURSES)         subwin                  
         window(3NCURSES)         syncok                  
         window(3NCURSES)         wcursyncup              
         window(3NCURSES)         wsyncdown               
         window(3NCURSES)         wsyncup                 

         wresize(3NCURSES)*       wresize                 
