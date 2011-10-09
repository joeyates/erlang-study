This repository is for the Erlang programs that I write to learn the language.

Programs
--------

binary_trees
============
Reconstruct a binary tree from its breadth-first and depth-first search.

## Example

From this tree:
             1
           /  \
          /    \
         /      \
        2        6
       / \      /
      7   3    8
     /     \
    4       5

* breadth-first search: 1,2,6,7,3,8,4,5
* depth-first search: 4,7,5,2,3,1,8,6

```erlang
1> Bfs=[1,2,6,7,3,8,4,5].
[1,2,6,7,3,8,4,5]
2> Dfs=[4,7,5,2,3,1,8,6].
[4,7,5,2,3,1,8,6]
3> tree:from_bfs_dfs(Bfs,Dfs).
{node,1,
      {node,2,
            {node,7,
                  {node,4,undefined,undefined},
                  {node,5,undefined,undefined}},
            {node,3,undefined,undefined}},
      {node,6,{node,8,undefined,undefined},undefined}}
```
