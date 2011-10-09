-module(list).

-compile(export_all).

spliton( Elem, List ) ->
  {Before,From} = lists:splitwith(fun(A) -> A /= Elem end, List),
  %% skip the element (i.e. get the tail - isn't there a method for this?)
  [Elem|After] = From,
  {Before,After}.
