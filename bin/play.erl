#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noinput -smp disable

-include_lib("ncurses/include/ncurses.hrl").

main([What]) ->
    application:start(ncurses),
    play(What),
    %try play(What)
    %catch
    %    X:Y ->
    %        io:format( "Exception type : ~p~n", [X] ),
    %        io:format( "Exception : ~p~n", [Y] );
    %    _:_ ->
    %        io:format( "Unknown exception" )
    %end,
    application:stop(ncurses),
    erlang:halt();

main(_) ->
    usage().

usage() ->
    io:format("usage: play\n"),
    halt(1).

play(What) ->
    case What of
        "curses" -> curses();
        "addch" -> addch();
        "addstr" -> addstr();
        "addchstr" -> addchstr();
        "attrs" -> attrs()
    end.

% Initialize
curses() ->
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:addstr( "NCurses version : " ),
    nc_srv:addstr( nc_srv:curses_version() ),
    nc_srv:addch($\n),
    nc_srv:addstr( "Has color : " ),
    nc_srv:addstr( io_lib:format( "~p~n", [nc_srv:has_colors()] )),
    nc_srv:addstr( "Can change color : " ),
    nc_srv:addstr( io_lib:format( "~p~n", [nc_srv:can_change_color()] )),
    nc_srv:addstr( "RGB ... \n" ),
    nc_srv:addstr( "  BLACK   : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_BLACK)] )),
    nc_srv:addstr( "  RED     : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_RED)] )),
    nc_srv:addstr( "  GREEN   : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_GREEN)] )),
    nc_srv:addstr( "  YELLOW  : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_YELLOW)] )),
    nc_srv:addstr( "  BLUE    : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_BLUE)] )),
    nc_srv:addstr( "  MAGENTA : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n",[nc_srv:color_content(?COLOR_MAGENTA)] )),
    nc_srv:addstr( "  CYAN    : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_CYAN)] )),
    nc_srv:addstr( "  WHITE   : " ),
    nc_srv:addstr(
        io_lib:format( "~p~n", [nc_srv:color_content(?COLOR_WHITE)] )),
    nc_srv:refresh(),
    nc_srv:addstr( "Press any key to hear a beep\n" ),
    nc_srv:refresh(),
    nc_srv:getch(),
    nc_srv:beep(),
    nc_srv:addstr( "Press any key to see a flash\n" ),
    nc_srv:refresh(),
    nc_srv:getch(),
    nc_srv:flash(),
    nc_srv:getch(),
    ok.

addch() ->
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:addstr( "Testing with cbreak / noecho - line buffered ... \n" ),
    nc_srv:addch(?CH(?A_UNDERLINE, 0, 97)),
    nc_srv:addch(97),
    nc_srv:addch(4,5,97),
    nc_srv:addch(4,5,?CH(?A_UNDERLINE, 0, 97)), % Cursor will be at (4,6)
    nc_srv:addch($\n),
    nc_srv:refresh(),
    Line1 = intill(10, []),
    timer:sleep(1000),
    nc_srv:addstr(Line1),
    cursyx(),
    nc_srv:addch($\n),
    nc_srv:refresh(),

    nc_srv:getch(),
    ok.

addstr() ->
    %% Set attributes
    nc_srv:cbreak(),
    nc_srv:noecho(),
    %% Write initial string...
    nc_srv:addstr("Testing add(n)str ...\n"),
    nc_srv:addstr(lists:duplicate(80, $H)),
    nc_srv:addch($\n),
    nc_srv:addstr(4, 2, lists:duplicate(8, $H)),
    nc_srv:addch($\n),
    nc_srv:addnstr(lists:duplicate(80, $H), 80),
    nc_srv:addnstr(8, 5, lists:duplicate(23, $H), 23),
    nc_srv:addch($\n),
    nc_srv:addch($\n),
    cursyx(),
    nc_srv:refresh(),
    nc_srv:getch(),
    ok.

addchstr() ->
    %% Set attributes
    nc_srv:cbreak(),
    nc_srv:noecho(),
    %% Write initial string...
    nc_srv:addstr("Testing addch(n)str ...\n"),
    nc_srv:addchstr(
        lists:map( nc_srv:chattr(?A_BOLD), lists:duplicate(25, $H) )),
    nc_srv:addchstr(lists:duplicate(23, $H)),
    nc_srv:refresh(),
    nc_srv:addch($\n),
    nc_srv:addchstr(
        lists:map( nc_srv:chattr(?A_UNDERLINE), lists:duplicate(20, $H) )),
    nc_srv:addchstr(4, 2, lists:duplicate(8, $H)),
    nc_srv:addchnstr(8, 5, lists:duplicate(23, $H), 23),
    nc_srv:addchnstr(lists:duplicate(80, $H), 80),
    cursyx(),
    nc_srv:refresh(),
    nc_srv:getch(),
    ok.

attrs() ->
    nc_srv:cbreak(),
    nc_srv:noecho(),
    nc_srv:addstr( "Testing color_set ... \n" ),
    nc_srv:refresh(),
    nc_srv:init_pairs(),
    Fn = fun(CPair) -> nc_srv:color_set(CPair), nc_srv:addch($a) end,
    lists:map(Fn, lists:seq(0,64)),
    nc_srv:refresh(),
    nc_srv:getch(),
    ok.

intill(Expect, Expect, Acc) ->
    nc_srv:echochar(Expect),
    lists:reverse(Acc);
intill(Expect, Val, Acc) ->
    nc_srv:echochar(Val),
    Val_ = nc_srv:getch(), 
    intill(Expect, Val_, [Val | Acc]).

intill(Expect, Acc) ->
    intill(Expect, nc_srv:getch(), Acc).

cursyx() ->
    nc_srv:addstr( io_lib:format( "~p", [nc_srv:getyx()] )).
