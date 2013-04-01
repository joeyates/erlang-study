-module(binary_tree).

-export([bfs/1, dfs/1, from_bfs_dfs/2]).

-include("node.hrl").
-include_lib("eunit/include/eunit.hrl").

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
from_bfs_dfs([], _) ->
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

% return two lists:
% - elements before the first occurence of Elem,
% - elements after the first occurence of Elem.
spliton(Elem, List) ->
  {Before, From} = lists:splitwith(fun(A) -> A /= Elem end, List),
  case From of
  % If Elem is in List, it will be the first element in From
  [Elem | After] ->
    {Before, After};
	_ ->
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

%% Tests for private functions

spliton_test_() ->
    [?_assert(spliton(2, [1, 2, 3]) =:= {[1],   [3]}),
     ?_assert(spliton(1, [1, 2, 3]) =:= {[],    [2, 3]}),
     ?_assert(spliton(3, [1, 2, 3]) =:= {[1,2], []}),
     ?_assert(spliton(4, [1, 2, 3]) =:= not_found)].

internal_bfs_test_() ->
    Root = #node{value = 1},
    Left = #node{value = 1, left = #node{value = 2}},
    Right = #node{value = 1, right = #node{value = 3}},
    Both = #node{value = 1,
		 left = #node{value = 2},
		 right = #node{value = 3}},
    Large = #node{value = 1,
                  left = #node{value = 2,
			       left = #node{value = 7,
					    left = #node{value = 4},
					    right = #node{value = 5}},
			       right = #node{value = 3}},
                  right = #node{value = 6,
				left = #node{value = 8}}},
    [?_assert(internal_bfs(Root)  =:= [[1]]),
     ?_assert(internal_bfs(Left)  =:= [[1], [2]]),
     ?_assert(internal_bfs(Right) =:= [[1], [3]]),
     ?_assert(internal_bfs(Both)  =:= [[1], [2, 3]]),
     ?_assert(internal_bfs(Large) =:= [[1],[2,6],[7,3,8],[4,5]])].

zip_lists_of_lists_test_() ->
    [?_assert(zip_lists_of_lists([],            [])            =:= []),
     ?_assert(zip_lists_of_lists([[1]],         [])            =:= [[1]]),
     ?_assert(zip_lists_of_lists([[1, 2]],      [])            =:= [[1, 2]]),
     ?_assert(zip_lists_of_lists([[1], [2]],    [])            =:= [[1], [2]]),
     ?_assert(zip_lists_of_lists([[1, 2], [3]], [])            =:= [[1, 2], [3]]),
     ?_assert(zip_lists_of_lists([],            [[1]])         =:= [[1]]),
     ?_assert(zip_lists_of_lists([],            [[1, 2]])      =:= [[1, 2]]),
     ?_assert(zip_lists_of_lists([],            [[1], [2]])    =:= [[1], [2]]),
     ?_assert(zip_lists_of_lists([],            [[1, 2], [3]]) =:= [[1, 2], [3]])].
