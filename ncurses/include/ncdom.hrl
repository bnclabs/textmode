-record(nodew, {
            pproc,
            cprocs,
            pid,
            rootnode,
            node,
            box,
            view
       }).

-record(node, {
            name,
            pagepath,
            pid,
            tag,
            attributes,
            content,
            box
       }).

-record(text, {
            content
       }).

% box corresponds to a node after taking Node's margin attribute into account.
-record(box, {
            y,
            x,
            rows,
            cols,
            margin,
            border,
            padding,
            view
       }).

% view will take into account border and padding of the box.
-record(view, {
            y,      % window relative row offset where the border starts.
            x,      % window relative column offset where the border starts.
            rows,   % number of rows starting from y, including y.
            cols    % number of columns starting from x, including x.
       }).

% :margin: is a tuple of four integer offset relative to parent element's 
% view port.
%   * A value of 0 aligns the box's border with parent element's view port
%   * Negative values are allowed.
-record(margin, {top, right, bottom, left}).

% :borderall: is a tuple of four borderone records.
-record(borderall, {top, right, bottom, left}).
-record(borderone, {
            y,      % window relative row offset where the border starts.
            x,      % window relative column offset where the border starts.
            len,    % length in columns or rows for the border.
            char,   % character to use for drawing the border.
            color   % border color.
       }).

% :padding: is a tuple of four integer offset relative to box's border.
% view port. Negative values are allowed for margin.
-record(padding, {top, right, bottom, left}).
