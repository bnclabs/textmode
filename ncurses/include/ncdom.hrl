-record( tag, {
            name,       % Atom of process name constructed using appname,
                        % pagepath and id-attribute. Node process is
                        % registered with this name.
            docpath,
            tagname,    % tag's tag-name
            attributes, % List of attributes, [{name, value}, ...]
            content,    % Child elements as list of #text and #tag records
            box,        % #box record
            view        % #view record.
       }).

-record( text, {
            content     % Text string inside the tag.
       }).

% box corresponds to a tag-node after taking Node's margin attribute into 
% account.
-record( box, {
            y,          % Row offset, indexed from 0.
            x,          % Column offset, indexed from 0.
            rows,       % Number of rows starting from Y offset.
            cols,       % Number of columns starting from X offset.
            border      % Tuple of four border elements, refer borderall record.
       }).

% view will take into account border and padding of the box.
-record( view, {
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
