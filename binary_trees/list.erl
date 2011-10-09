-module(list).

-compile(export_all).

spliton( Elem, List ) ->
    {Before,From} = lists:splitwith(fun(A) -> A /= Elem end, List),
    case From of
	[Elem|After] ->
	    {Before,After};
	_Else ->
	    not_found
    end.
