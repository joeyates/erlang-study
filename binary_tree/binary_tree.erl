-module(binary_tree).

-include("node.hrl").

-compile(export_all).

from_bfs_dfs(_, []) ->
    % Done
    undefined;
from_bfs_dfs(Bfs, Dfs) ->
    [BHead | BTail] = Bfs,
    case spliton(BHead, Dfs) of
	{Left, Right} ->
	    #node{value = BHead,
		  left = from_bfs_dfs(BTail, Left),
		  right = from_bfs_dfs(BTail, Right)};
	not_found ->
	    %% Try the next Dfs value
	    from_bfs_dfs(BTail, Dfs)
    end.

spliton(Elem, List) ->
    {Before, From} = lists:splitwith(fun(A) -> A /= Elem end, List),
    case From of
	[Elem | After] ->
	    {Before, After};
	_Else ->
	    not_found
    end.
