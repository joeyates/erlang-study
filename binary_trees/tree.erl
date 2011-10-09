-module(tree).

-include("node.hrl").

-compile(export_all).

rebuild_from_dfs_bfs( Dfs, Bfs ) ->
  [BHead|_] = Bfs,
  {DHead,DTail} = list:spliton(BHead,Dfs),
  #node{value=BHead,
        left=DHead,
        right=DTail}.

%% D=[4,7,5,2,3,1,8,6].
%% B=[1,2,6,7,3,8,4,5].
%% tree:rebuild_from_dfs_bfs(D,B).
