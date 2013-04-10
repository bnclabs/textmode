-module(nc_examples).
-compile(export_all).

-include("ncurses.hrl").

%%
%% Simple countdown which shows how to print, move and get coordinates
%%
countdown() ->
    application:start(ncurses),
    ncdrv:cbreak(),
    ncdrv:noecho(),
    ncdrv:curs_set(?CURS_INVISIBLE),
    ncdrv:move(1, 1),
    Flag = ncdrv:has_colors(),
    ncdrv:addstr(io_lib:format("Has color: ~p",[Flag])),
    print_colors(Flag),
    ncdrv:move(10, 10),
    ncdrv:addstr("Countdown: "),
    ncdrv:refresh(),
    count_it_down(10),
    ncdrv:curs_set(?CURS_NORMAL),
    timer:sleep(2000),
    application:stop(ncurses).

count_it_down(S) when S =< 0 ->
    ncdrv:move(10, 22),
    ncdrv:addstr("BOOOOM!"),
    ncdrv:refresh();
count_it_down(S) ->
    ncdrv:move(10+S, 22),
    {X, Y} = ncdrv:getyx(),
    {MX, MY} = ncdrv:getmaxyx(),
    ncdrv:addstr(io_lib:format("~p",[S])),
    ncdrv:move(22,22),
    ncdrv:addstr(io_lib:format("~p:~p (~p:~p)",[X,Y,MX,MY])),
    ncdrv:refresh(),
    timer:sleep(1000),
    count_it_down(S-1).

print_colors(false) -> ok;
print_colors(true) ->
    ncdrv:start_color(),
    ncdrv:init_pair(1, ?COLOR_RED, ?COLOR_BLACK),
    ncdrv:attron(?A_BOLD bor ?COLOR_PAIR(1)),
    ncdrv:move(2,1),
    ncdrv:addstr("Colored!"),
    ncdrv:refresh(),
    ncdrv:attroff(?A_BOLD bor ?COLOR_PAIR(1)),
    ok.

%%
%% Simple example to show usage
%%
simple() ->
    application:start(ncurses),
    ok = ncdrv:nocbreak(),
    ok = ncdrv:cbreak(),
    ok = ncdrv:echo(),
    ok = ncdrv:noecho(),
    ok = ncdrv:curs_set(?CURS_INVISIBLE),
    ok = ncdrv:move(7, 10),
    ok = ncdrv:addch(45),
    ok = ncdrv:addch(45),
    ok = ncdrv:move(7, 12),
    ok = ncdrv:addstr(" Information ----"),
    ok = ncdrv:move(8, 10),
    {Row, Col} = ncdrv:getyx(),
    {MRow, MCol} = ncdrv:getmaxyx(),
    ok = ncdrv:addstr(io_lib:format("Row:~p Col:~p MaxRow:~p MaxCol:~p",
				    [Row,Col,MRow,MCol])),
    case ncdrv:has_colors() of
	true ->
	    ncdrv:start_color(),
	    ok = ncdrv:init_pair(1, ?COLOR_BLUE, ?COLOR_WHITE),
	    ok = ncdrv:init_pair(2, ?COLOR_GREEN, ?COLOR_YELLOW), 
	    ncdrv:move(9, 10),
	    ncdrv:attron(?COLOR_PAIR(1) bor ?A_BOLD bor ?A_UNDERLINE),
	    ncdrv:addstr(" Has Colors! "),
	    ncdrv:attron(?COLOR_PAIR(2)),
	    ncdrv:addstr(" Yes!! "),
	    ncdrv:attroff(?COLOR_PAIR(1) bor ?COLOR_PAIR(2) bor ?A_BOLD bor ?A_UNDERLINE);
	false ->
	    ncdrv:move(9, 10),
	    ncdrv:addstr(" No colors :( ")
    end,		     
    ncdrv:addch($!),
    ok = ncdrv:refresh(),
    timer:sleep(5000),
    ncdrv:curs_set(?CURS_NORMAL),
    application:stop(ncurses).

%%
%% Fun example
%%
colors() ->
    application:start(ncurses),
    ok = ncdrv:cbreak(),
    ok = ncdrv:noecho(),
    ok = ncdrv:curs_set(?CURS_INVISIBLE),
    ok = ncdrv:start_color(),
    ok = ncdrv:init_pair(1, ?COLOR_BLACK, ?COLOR_RED),
    ok = ncdrv:init_pair(2, ?COLOR_BLACK, ?COLOR_GREEN),
    ok = ncdrv:init_pair(3, ?COLOR_BLACK, ?COLOR_YELLOW),
    ok = ncdrv:init_pair(4, ?COLOR_BLACK, ?COLOR_BLUE),
    ok = ncdrv:init_pair(5, ?COLOR_BLACK, ?COLOR_MAGENTA),
    ok = ncdrv:init_pair(6, ?COLOR_BLACK, ?COLOR_CYAN),
    ok = ncdrv:init_pair(7, ?COLOR_BLACK, ?COLOR_WHITE),
    ok = ncdrv:init_pair(8, ?COLOR_BLACK, ?COLOR_BLACK),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {MaxRow, MaxCol} = ncdrv:getmaxyx(),
    ncdrv:move(10,10),
    ncdrv:addstr(io_lib:format("Max Row: ~p, Max Col: ~p",[MaxRow, MaxCol])),
    ncdrv:move(0, 0),
    ncdrv:addch($@),
    ncdrv:move(MaxRow-1, 0),
    ncdrv:addch($@),
    ncdrv:move(0, MaxCol-1),
    ncdrv:addch($@),
    ncdrv:move(MaxRow-1, MaxCol-1),
    ncdrv:addch($@),
    ncdrv:refresh(),
    timer:sleep(2000),
    do_colors(MaxRow, MaxCol, 2000),
    application:stop(ncurses).

do_colors(_,_,0) -> ok;
do_colors(MR,MC,N) ->
    ch_colors(MR,MC,1000),
    ncdrv:refresh(),
    timer:sleep(100),
    do_colors(MR, MC, N-1).

ch_colors(_,_,0) -> ok;
ch_colors(MR, MC, N) ->
    R = random:uniform(MR)-1,
    C = random:uniform(MC)-1,
    CN = random:uniform(8),
    ncdrv:attron(?COLOR_PAIR(CN)),
    ncdrv:move(R, C),
    ncdrv:addch($ ),
    ncdrv:move(R, C),
    ch_colors(MR, MC, N-1).

%%
%% Simply puts an @ character somewhere. If you go out of bounds you crash
%% 
pos(Y, X) ->
    application:start(ncurses),
    ok = ncdrv:cbreak(),
    ok = ncdrv:curs_set(?CURS_INVISIBLE),
    ncdrv:move(Y, X),
    ncdrv:addstr("@"),
    ncdrv:refresh(),
    timer:sleep(2000),
    application:stop(ncurses).

%% 
%% Prints a number continuously as another io thread is waiting for keyinput
%%
input() ->
    application:start(ncurses),
    ok = ncdrv:cbreak(),
    ok = ncdrv:noecho(),
    ok = ncdrv:curs_set(?CURS_INVISIBLE),
    ok = ncdrv:keypad(?W_STDSCR, true),
    spawn_link(?MODULE, input_counter, [0]),
    ncdrv:addstr(9, 10, "Enter:    "),
    ncdrv:refresh(),
    input_reader().

input_reader() ->
    P = ncdrv:getch(),
    case P of
	$q ->
	    application:stop(ncurses);
	?KEY_F(1) -> 
	    halt();
	_ ->
	    ncdrv:addstr(9, 17, io_lib:format("~p  ",[P])),
	    ncdrv:refresh(),
	    input_reader()
    end.

input_counter(N) ->
    ncdrv:addstr(10, 10, io_lib:format("# ~p",[N])),
    ncdrv:refresh(),
    timer:sleep(100),
    input_counter(N+1).

%%
%% cursmove - move the '@' around the screen with the arrow keys. 'q' to quit.
%%
cursmove() ->
    application:start(ncurses),
    ncdrv:cbreak(),
    ncdrv:noecho(),
    ncdrv:curs_set(?CURS_INVISIBLE),
    ncdrv:keypad(?W_STDSCR, true),
    ncdrv:addch(10, 10, $@),
    ncdrv:move(10,10),
    ncdrv:refresh(),
    moveloop(ncdrv:getch()).
moveloop(K) when K == ?KEY_F(1) ->
    halt();
moveloop(?KEY_ESC) ->
    application:stop(ncurses);
moveloop(C) ->
    case C of
	?KEY_UP -> mv(-1, 0);
	?KEY_DOWN -> mv(1, 0);
	?KEY_RIGHT -> mv(0, 1);
	?KEY_LEFT -> mv(0, -1);
	_ -> ok
    end,
    ncdrv:refresh(),
    moveloop(ncdrv:getch()).

mv(OffsetY, OffsetX) ->
    {CY, CX} = ncdrv:getyx(),
    FinalY = CY+(OffsetY),
    FinalX = CX+(OffsetX),
    ncdrv:addch(FinalY,FinalX,$@),
    ncdrv:move(FinalY, FinalX).

%%
%% helloworld - bounce "Hello World!" on the end of the screen
%%
helloworld() ->
    %% Start application
    application:start(ncurses),
    %% Set attributes
    ncdrv:cbreak(),
    ncdrv:noecho(),
    ncdrv:curs_set(?CURS_INVISIBLE),
    %% Write initial string...
    ncdrv:addstr(0, 0, "Hello World!"),
    ncdrv:refresh(),
    %% Start the process that will "move" the string
    Mover = spawn(fun() -> mvhello() end),
    ctrl(Mover).

ctrl(Mover) ->
    %% get key-input
    C = ncdrv:getch(),
    case C of
	$q -> 
	    %% If we get a 'q' then exit the mover and stop ncurses
	    exit(Mover, normal),
	    application:stop(ncurses),
	    erlang:halt();
	_ ->
	    %% ignore anything else
	    ctrl(Mover)
    end.

%% start the mover
mvhello() -> mvhello(0, 0, 1, 1).
%% take previous pos and direction and print out new string
mvhello(PrevY, PrevX, DirY, DirX) ->
    %% "erase" previous position
    ncdrv:addstr(PrevY, PrevX, "            "),
    %% calculate new position and direction
    {NewY, NewX, NewDirY, NewDirX} =
	calc_new_pos(PrevY, PrevX, DirY, DirX),
    %% "move" the text to new position
    ncdrv:addstr(NewY, NewX, "Hello World!"),
    %% update the screen to show the change
    ncdrv:refresh(),
    %% do it again!
    timer:sleep(1),
    mvhello(NewY, NewX, NewDirY, NewDirX).

calc_new_pos(Py, Px, Dy, Dx) ->
    %% get max coords of the screen
    {My, Mx} = ncdrv:getmaxyx(),
    %% calc new vertical position and new direction
    {NewPy, NewDy} =
	if (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
		{Py+(Dy*-1), Dy*-1};
	   true ->
		{Py+(Dy), Dy}
	end,
    %% calc new horizontal position and new direction
    %% take string length into account
    {NewPx, NewDx} =
	if (Px+(Dx)+12 >= Mx) orelse (Px+(Dx) < 0) ->
		{Px+(Dx*-1), Dx*-1};
	   true ->
		{Px+(Dx), Dx}
	end,
    {NewPy, NewPx, NewDy, NewDx}.
