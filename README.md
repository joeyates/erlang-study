This repository is for the Erlang programs that I write to learn the language.

Programs
--------

binary_tree
===========

The method `binary_tree:from_bfs_dfs/2` reconstructs a binary tree from its breadth-first and depth-first searches.

It returns a binary tree of `node`s.
Each `node` is composed of a value, a left branch and a right branch.
If a branch is empty, the node has the atom `undefined`.

E.g.

```erlang
{node,5,undefined,undefined}
```

This node has value 5 and is a leaf node (i.e. it has no children).

## Example

From this binary tree:

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

```erlang
1> c(binary_tree).
{ok,binary_tree}
2> binary_tree:from_bfs_dfs([1,2,6,7,3,8,4,5],[4,7,5,2,3,1,8,6]).
{node,1,
      {node,2,
            {node,7,
                  {node,4,undefined,undefined},
                  {node,5,undefined,undefined}},
            {node,3,undefined,undefined}},
      {node,6,{node,8,undefined,undefined},undefined}}
```
