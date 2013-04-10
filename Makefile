.PHONY : ncurses

compile : compile-ncurses compile-sudoku

clean :
	@cd ncurses; ./rebar clean;
	@cd sudoku; ./rebar clean;

compile-ncurses :
	@cd ncurses; ./rebar clean; ./rebar compile;

compile-sudoku :
	@cd sudoku; ./rebar clean; ./rebar compile;
