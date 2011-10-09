-module(tree).

-include("node.hrl").

-compile(export_all).

from_bfs_dfs(_,[]) ->
    % Done
    undefined;
from_bfs_dfs(Bfs,Dfs) ->
    [BHead|BTail] = Bfs,
    case list:spliton(BHead,Dfs) of
	{Left,Right} ->
	    #node{value=BHead,
		  left=from_bfs_dfs(BTail,Left),
		  right=from_bfs_dfs(BTail,Right)};
	not_found ->
	    %% Try the next Dfs value
	    from_bfs_dfs(BTail,Dfs)
    end.
