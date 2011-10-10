-module(binary_tree_tests).

-include("node.hrl").
-include_lib("eunit/include/eunit.hrl").

bfs_test() ->
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
    ?assertEqual([1],               binary_tree:bfs(Root)),
    ?assertEqual([1, 2],            binary_tree:bfs(Left)),
    ?assertEqual([1, 3],            binary_tree:bfs(Right)),
    ?assertEqual([1, 2, 3],         binary_tree:bfs(Both)),
    ?assertEqual([1, 2, 6, 7, 3, 8, 4, 5],
		 binary_tree:bfs(Large)).

dfs_test() ->
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
    ?assertEqual([1],               binary_tree:dfs(Root)),
    ?assertEqual([2, 1],            binary_tree:dfs(Left)),
    ?assertEqual([1, 3],            binary_tree:dfs(Right)),
    ?assertEqual([2, 1, 3],         binary_tree:dfs(Both)),
    ?assertEqual([4, 7, 5, 2, 3, 1, 8, 6],
		 binary_tree:dfs(Large)).
