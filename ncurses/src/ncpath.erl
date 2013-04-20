-module(ncpath).
-author('prataprc@gmail.com').

% module API
-export([docpath/3, docnode/1, docapp/1, pagepath/1, nodename/2, tagid/1]).

-include("ncurses.hrl").
-include("ncdom.hrl").

% Path = ncapp://<Node>/<App>/<Path>#<node-id>

docpath(Node, Appname, XmlFile) when is_atom(Node) ->
    docpath(atom_to_list(Node), Appname, XmlFile);

docpath(Node, Appname, XmlFile) when is_atom(Appname) ->
    docpath(Node, atom_to_list(Appname), XmlFile);

docpath(Node, Appname, XmlFile) ->
    filename:join([ "ncapp://"++Node, Appname, filename:basename(XmlFile) ]).

docnode("ncapp://" ++ DocPath) ->
    [Node | _ ] = string:tokens(DocPath, "/"),
    list_to_atom(Node).

docapp("ncapp://" ++ DocPath) ->
    [_Node, Appname | _ ] = string:tokens(DocPath, "/"),
    list_to_atom(Appname).

pagepath("ncapp://" ++ DocPath) ->
    [_Node, _Appname, Path | _] = string:tokens(DocPath, "/"),
    Path.

nodename(DocPath, Tag) ->
    case ncdom:attr(id, Tag) of
        {id, Id} ->
            list_to_atom(DocPath ++ "#" ++ Id);
        false -> 
            error_logger:error_msg("Element without `id` attribute ~n"),
            none
    end.

tagid(NodeName) ->
    [_ | TagId] = string:tokens(NodeName, "#"),
    TagId.

