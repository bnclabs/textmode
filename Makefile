.PHONY : ncurses sudoku snake

compile : compile-ncurses compile-sudoku compile-snake

clean :
	@cd ncurses; ./rebar clean;
	@cd sudoku; ./rebar clean;
	@cd snake; ./rebar clean;

compile-ncurses :
	@cd ncurses; ./rebar compile;

compile-sudoku :
	@cd sudoku; ./rebar compile;

compile-snake :
	@cd snake; ./rebar compile;

pushcode: push-github

push-github:
	git push git@github.com:prataprc/textmode.git
