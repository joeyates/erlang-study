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
     / \
    4   5

* breadth-first search: 1,2,6,7,3,8,4,5
* depth-first search: 4,7,5,2,3,1,8,6

The method `tree:from_bfs_dfs/2` returns a tree of `node`s.
Each `node` is composed of a value, a left branch and a right branch.
If a branch is empty, the node has the atom `undefined`.
E.g.
```erlang
{node,5,undefined,undefined}
```
This node has value 5 and is a leaf node (i.e. it has no children).

```erlang
1> c(tree).
{ok,tree}
2> tree:from_bfs_dfs([1,2,6,7,3,8,4,5],[4,7,5,2,3,1,8,6]).
{node,1,
      {node,2,
            {node,7,
                  {node,4,undefined,undefined},
                  {node,5,undefined,undefined}},
            {node,3,undefined,undefined}},
      {node,6,{node,8,undefined,undefined},undefined}}
```
