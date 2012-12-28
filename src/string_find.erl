-module(string_find).

-export([crossover/0, mutate/1, fit_fn/1, success/1, seed/1]).


crossover() ->
    fun(M, F) ->
	    Pt = crypto:rand_uniform(2, length(M)),
	    {M1, M2} = lists:split(Pt, M),
	    {F1, F2} = lists:split(Pt, F),
	    {M1++F2, F1++M2}
    end.


mutate(Probability) ->
    fun(Chromosome) ->
	    case crypto:rand_uniform(0, 100) of
		X when X > Probability ->
		    Chromosome;
		_ ->
		    Pos = crypto:rand_uniform(0, length(Chromosome)),
		    NewChar = crypto:rand_uniform(32, 127),
		    {Left, Right} = lists:split(Pos, Chromosome),
		    Left ++ [NewChar] ++ tl(Right)
	    end
    end.


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
    fun(_Score, Candidate) ->
	    Candidate == Target
    end.
    


seed(Target)->
    fun() -> random_str(length(Target)) end.


random_str(0) -> [];
random_str(Length) -> [random_char() | random_str(Length-1)].
random_char() -> crypto:rand_uniform(32, 127).



