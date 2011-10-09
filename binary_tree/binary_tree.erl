-module(binary_tree).

-include("node.hrl").

-export([bfs/1, dfs/1, from_bfs_dfs/2]).

bfs(Node) ->
  lists:flatten(internal_bfs(Node)).

dfs({node, Value, undefined, undefined}) ->
    [Value];
dfs({node, Value, Left, undefined}) ->
    dfs(Left) ++ [Value];
dfs({node, Value, undefined, Right}) ->
    [Value] ++ dfs(Right);
dfs({node, Value, Left, Right}) ->
    dfs(Left) ++ [Value] ++ dfs(Right).

from_bfs_dfs(_, []) ->
    undefined;
from_bfs_dfs(Bfs, Dfs) ->
    [BHead | BTail] = Bfs,
    case spliton(BHead, Dfs) of
	{Left, Right} ->
	    #node{value = BHead,
		  left = from_bfs_dfs(BTail, Left),
		  right = from_bfs_dfs(BTail, Right)};
	not_found ->
	    %% Try the next Bfs value
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

% create lists of lists of elements at a certain depth
internal_bfs({node, Value, undefined, undefined}) ->
    [[Value]];
internal_bfs({node, Value, Left, undefined}) ->
    [[Value]] ++ internal_bfs(Left);
internal_bfs({node, Value, undefined, Right}) ->
    [[Value]] ++ internal_bfs(Right);
internal_bfs({node, Value, Left, Right}) ->
    LLLeft = internal_bfs(Left),
    LLRight = internal_bfs(Right),
    [[Value]] ++ zip_lists_of_lists(LLLeft, LLRight).

% zip unequal length lists of lists
zip_lists_of_lists(LL1, []) ->
    LL1;
zip_lists_of_lists([], LL2) ->
    LL2;
zip_lists_of_lists(LL1, LL2) ->
    [Head1 | Rest1] = LL1,
    [Head2 | Rest2] = LL2,
    Head = case [Head1, Head2] of
	       [L1, []] -> L1;
	       [[], L2] -> L2;
	       [L1, L2] -> L1 ++ L2
	   end,
    [Head] ++ zip_lists_of_lists(Rest1, Rest2).
