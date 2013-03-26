-module(nc_examples).
-compile(export_all).

-include("ncurses.hrl").

%%
%% Simple countdown which shows how to print, move and get coordinates
%%
countdown() ->
    application:start(ncurses),
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:curs_set(?CURS_INVISIBLE),
    nc_srv:move(1, 1),
    Flag = nc_srv:has_colors(),
    nc_srv:addstr(io_lib:format("Has color: ~p",[Flag])),
    print_colors(Flag),
    nc_srv:move(10, 10),
    nc_srv:addstr("Countdown: "),
    nc_srv:refresh(),
    count_it_down(10),
    nc_srv:curs_set(?CURS_NORMAL),
    timer:sleep(2000),
    application:stop(ncurses).

count_it_down(S) when S =< 0 ->
    nc_srv:move(10, 22),
    nc_srv:addstr("BOOOOM!"),
    nc_srv:refresh();
count_it_down(S) ->
    nc_srv:move(10+S, 22),
    {X, Y} = nc_srv:getyx(),
    {MX, MY} = nc_srv:getmaxyx(),
    nc_srv:addstr(io_lib:format("~p",[S])),
    nc_srv:move(22,22),
    nc_srv:addstr(io_lib:format("~p:~p (~p:~p)",[X,Y,MX,MY])),
    nc_srv:refresh(),
    timer:sleep(1000),
    count_it_down(S-1).

print_colors(false) -> ok;
print_colors(true) ->
    nc_srv:start_color(),
    nc_srv:init_pair(1, ?COLOR_RED, ?COLOR_BLACK),
    nc_srv:attron(?A_BOLD bor ?COLOR_PAIR(1)),
    nc_srv:move(2,1),
    nc_srv:addstr("Colored!"),
    nc_srv:refresh(),
    nc_srv:attroff(?A_BOLD bor ?COLOR_PAIR(1)),
    ok.

%%
%% Simple example to show usage
%%
simple() ->
    application:start(ncurses),
    ok = nc_srv:nocbreak(),
    ok = nc_srv:cbreak(),
    ok = nc_srv:echo(),
    ok = nc_srv:noecho(),
    ok = nc_srv:curs_set(?CURS_INVISIBLE),
    ok = nc_srv:move(7, 10),
    ok = nc_srv:addch(45),
    ok = nc_srv:addch(45),
    ok = nc_srv:move(7, 12),
    ok = nc_srv:addstr(" Information ----"),
    ok = nc_srv:move(8, 10),
    {Row, Col} = nc_srv:getyx(),
    {MRow, MCol} = nc_srv:getmaxyx(),
    ok = nc_srv:addstr(io_lib:format("Row:~p Col:~p MaxRow:~p MaxCol:~p",
				    [Row,Col,MRow,MCol])),
    case nc_srv:has_colors() of
	true ->
	    nc_srv:start_color(),
	    ok = nc_srv:init_pair(1, ?COLOR_BLUE, ?COLOR_WHITE),
	    ok = nc_srv:init_pair(2, ?COLOR_GREEN, ?COLOR_YELLOW), 
	    nc_srv:move(9, 10),
	    nc_srv:attron(?COLOR_PAIR(1) bor ?A_BOLD bor ?A_UNDERLINE),
	    nc_srv:addstr(" Has Colors! "),
	    nc_srv:attron(?COLOR_PAIR(2)),
	    nc_srv:addstr(" Yes!! "),
	    nc_srv:attroff(?COLOR_PAIR(1) bor ?COLOR_PAIR(2) bor ?A_BOLD bor ?A_UNDERLINE);
	false ->
	    nc_srv:move(9, 10),
	    nc_srv:addstr(" No colors :( ")
    end,		     
    nc_srv:addch($!),
    ok = nc_srv:refresh(),
    timer:sleep(5000),
    nc_srv:curs_set(?CURS_NORMAL),
    application:stop(ncurses).

%%
%% Fun example
%%
colors() ->
    application:start(ncurses),
    ok = nc_srv:cbreak(),
    ok = nc_srv:noecho(),
    ok = nc_srv:curs_set(?CURS_INVISIBLE),
    ok = nc_srv:start_color(),
    ok = nc_srv:init_pair(1, ?COLOR_BLACK, ?COLOR_RED),
    ok = nc_srv:init_pair(2, ?COLOR_BLACK, ?COLOR_GREEN),
    ok = nc_srv:init_pair(3, ?COLOR_BLACK, ?COLOR_YELLOW),
    ok = nc_srv:init_pair(4, ?COLOR_BLACK, ?COLOR_BLUE),
    ok = nc_srv:init_pair(5, ?COLOR_BLACK, ?COLOR_MAGENTA),
    ok = nc_srv:init_pair(6, ?COLOR_BLACK, ?COLOR_CYAN),
    ok = nc_srv:init_pair(7, ?COLOR_BLACK, ?COLOR_WHITE),
    ok = nc_srv:init_pair(8, ?COLOR_BLACK, ?COLOR_BLACK),
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {MaxRow, MaxCol} = nc_srv:getmaxyx(),
    nc_srv:move(10,10),
    nc_srv:addstr(io_lib:format("Max Row: ~p, Max Col: ~p",[MaxRow, MaxCol])),
    nc_srv:move(0, 0),
    nc_srv:addch($@),
    nc_srv:move(MaxRow-1, 0),
    nc_srv:addch($@),
    nc_srv:move(0, MaxCol-1),
    nc_srv:addch($@),
    nc_srv:move(MaxRow-1, MaxCol-1),
    nc_srv:addch($@),
    nc_srv:refresh(),
    timer:sleep(2000),
    do_colors(MaxRow, MaxCol, 2000),
    application:stop(ncurses).

do_colors(_,_,0) -> ok;
do_colors(MR,MC,N) ->
    ch_colors(MR,MC,1000),
    nc_srv:refresh(),
    timer:sleep(100),
    do_colors(MR, MC, N-1).

ch_colors(_,_,0) -> ok;
ch_colors(MR, MC, N) ->
    R = random:uniform(MR)-1,
    C = random:uniform(MC)-1,
    CN = random:uniform(8),
    nc_srv:attron(?COLOR_PAIR(CN)),
    nc_srv:move(R, C),
    nc_srv:addch($ ),
    nc_srv:move(R, C),
    ch_colors(MR, MC, N-1).

%%
%% Simply puts an @ character somewhere. If you go out of bounds you crash
%% 
pos(Y, X) ->
    application:start(ncurses),
    ok = nc_srv:cbreak(),
    ok = nc_srv:curs_set(?CURS_INVISIBLE),
    nc_srv:move(Y, X),
    nc_srv:addstr("@"),
    nc_srv:refresh(),
    timer:sleep(2000),
    application:stop(ncurses).

%% 
%% Prints a number continuously as another io thread is waiting for keyinput
%%
input() ->
    application:start(ncurses),
    ok = nc_srv:cbreak(),
    ok = nc_srv:noecho(),
    ok = nc_srv:curs_set(?CURS_INVISIBLE),
    ok = nc_srv:keypad(?W_STDSCR, true),
    spawn_link(?MODULE, input_counter, [0]),
    nc_srv:addstr(9, 10, "Enter:    "),
    nc_srv:refresh(),
    input_reader().

input_reader() ->
    P = nc_srv:getch(),
    case P of
	$q ->
	    application:stop(ncurses);
	?KEY_F(1) -> 
	    halt();
	_ ->
	    nc_srv:addstr(9, 17, io_lib:format("~p  ",[P])),
	    nc_srv:refresh(),
	    input_reader()
    end.

input_counter(N) ->
    nc_srv:addstr(10, 10, io_lib:format("# ~p",[N])),
    nc_srv:refresh(),
    timer:sleep(100),
    input_counter(N+1).

%%
%% cursmove - move the '@' around the screen with the arrow keys. 'q' to quit.
%%
cursmove() ->
    application:start(ncurses),
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:curs_set(?CURS_INVISIBLE),
    nc_srv:keypad(?W_STDSCR, true),
    nc_srv:addch(10, 10, $@),
    nc_srv:move(10,10),
    nc_srv:refresh(),
    moveloop(nc_srv:getch()).
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
    nc_srv:refresh(),
    moveloop(nc_srv:getch()).

mv(OffsetY, OffsetX) ->
    {CY, CX} = nc_srv:getyx(),
    FinalY = CY+(OffsetY),
    FinalX = CX+(OffsetX),
    nc_srv:addch(FinalY,FinalX,$@),
    nc_srv:move(FinalY, FinalX).

%%
%% helloworld - bounce "Hello World!" on the end of the screen
%%
helloworld() ->
    %% Start application
    application:start(ncurses),
    %% Set attributes
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:curs_set(?CURS_INVISIBLE),
    %% Write initial string...
    nc_srv:addstr(0, 0, "Hello World!"),
    nc_srv:refresh(),
    %% Start the process that will "move" the string
    Mover = spawn(fun() -> mvhello() end),
    ctrl(Mover).

ctrl(Mover) ->
    %% get key-input
    C = nc_srv:getch(),
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
    nc_srv:addstr(PrevY, PrevX, "            "),
    %% calculate new position and direction
    {NewY, NewX, NewDirY, NewDirX} =
	calc_new_pos(PrevY, PrevX, DirY, DirX),
    %% "move" the text to new position
    nc_srv:addstr(NewY, NewX, "Hello World!"),
    %% update the screen to show the change
    nc_srv:refresh(),
    %% do it again!
    timer:sleep(1),
    mvhello(NewY, NewX, NewDirY, NewDirX).

calc_new_pos(Py, Px, Dy, Dx) ->
    %% get max coords of the screen
    {My, Mx} = nc_srv:getmaxyx(),
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
