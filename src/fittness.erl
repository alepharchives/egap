% Return the fn which calculates the fittness of a given string (Citizen) vs
% the Target string.
-module(fittness).

-compile(export_all).

fit_fn(Target) ->
    fun(Citizen) ->
	    Fn = fun(Pos, Acc) ->
			 Acc +
			     -1 * (
				abs(lists:nth(Pos, Citizen) -
					lists:nth(Pos, Target)))
		 end,
	    
	    CharDiff = lists:foldl(Fn, 0, lists:seq(1, min(length(Citizen), length(Target)))),
	    CharDiff - abs(length(Citizen) - length(Target))
    end.

success(Target) ->
    fun(Candidate) ->
	    Candidate == Target
    end.
    


seed(Target)->
    fun() -> random_str(length(Target)) end.


random_str(0) -> [];
random_str(Length) -> [random_char() | random_str(Length-1)].
random_char() -> crypto:rand_uniform(32, 127).
