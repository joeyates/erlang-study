This repository is for the Erlang programs that I write to learn the language.

Programs
--------

binary_tree
===========

## node record

A `node` is composed of a value, a left branch and a right branch.
If a branch is empty, the node has the atom `undefined`.

A single node, with value `5` and no branches:

```erlang
{node,5,undefined,undefined}
```

This binary tree:

             1
           /  \
          /    \
         /      \
        2        6
       / \      /
      7   3    8
     / \
    4   5

can be represented as

```erlang
>Tree = {node,1,
>             {node,2,
>                   {node,7,
>                         {node,4,undefined,undefined},
>                         {node,5,undefined,undefined}},
>                   {node,3,undefined,undefined}},
>             {node,6,{node,8,undefined,undefined},undefined}}.
```

## bfs/1

Converts a `node` into an array of elements in breadth-first search order.

```erlang
> binary_tree:bfs(Tree).
[1,2,6,7,3,8,4,5]
```

## dfs/1

Converts a `node` into an array of elements in depth-first search order.

```erlang
> binary_tree:dfs(Tree).
[4,7,5,2,3,1,8,6]
```

## from_bfs_dfs/2

Reconstructs a binary tree from its breadth-first and depth-first searches.

```erlang
> binary_tree:from_bfs_dfs([1,2,6,7,3,8,4,5],[4,7,5,2,3,1,8,6]).
{node,1,
      {node,2,
            {node,7,
                  {node,4,undefined,undefined},
                  {node,5,undefined,undefined}},
            {node,3,undefined,undefined}},
      {node,6,{node,8,undefined,undefined},undefined}}
```
