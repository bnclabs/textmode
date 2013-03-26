.PHONY : ncurses

compile : compile-ncurses

clean :
	@cd ncurses; ./rebar clean;

compile-ncurses :
	@cd ncurses; ./rebar clean; ./rebar compile;
