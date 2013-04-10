-module(ncbuf).

%newbuffer(Name, Lines) ->
%    % ets ordered set.
%
%readch(Buf, Row, Col) ->
%
%insertch(Buf, Row, Col, Ch) ->
%
%deletech(Buf, Row, Col, Ch) ->
%
%changech(Buf, Row, Col, Ch) ->
%
%
%readstr(Buf, Row, Col, N) ->
%
%insertstr(Buf, Row, Col, Str) ->
%
%deletestr(Buf, Row, Col, N) ->
%
%changestr(Buf, Row, Col, N, Str) ->
%
%
%readline(Buf, Row, Col) ->
%
%insertline(Buf, Row, Line) ->
%
%deleteline(Buf, Row, Line) ->
%
%changeline(Buf, Row, Line) ->
%
%
%render(Buf) ->
%
%% _filename / url, can also be buffer-table name.
%% _swp, intermediate swap file. For automatic draft persistance.
%% _windows, list of {Node, Pid} interfacing this buffer with user-terminal.
%% _inf, callback module to handle events on this buffer.
%option(Buf, Key) ->
%
%option(Buf, Key, Value) ->
%
%
%save('_filename', Buf) ->
%
%save('_swp', Buf) ->
