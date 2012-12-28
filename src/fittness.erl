% Return the fn which calculates the fittness of a given string (Citizen) vs
% the Target string.
-module(fittness).

-compile(export_all).
-define(PI, 3.14159265358979323846264).


mutate(Probability) ->
    fun(Chromosome) ->
	    case crypto:rand_uniform(0, 100) of
		X when X > Probability ->
		    Chromosome;
		_ ->
		    Pos1 = crypto:rand_uniform(1, length(Chromosome)+1),
		    Pos2 = crypto:rand_uniform(1, length(Chromosome)+1),
		    {N1,_,_} = E1 = lists:nth(Pos1, Chromosome),
		    {N2,_,_} = E2 = lists:nth(Pos2, Chromosome),
		    C1 = lists:keyreplace(N1, 1, Chromosome, {x,x,x}),
		    C2 = lists:keyreplace(N2, 1, C1, {y,y,y}),

		    C3 = lists:keyreplace(x, 1, C2, E2),
		    lists:keyreplace(y, 1, C3, E1)
	    end
    end.


crossover()->
    fun(M,F) ->
	    Ma = array:from_list(M),
	    Fa = array:from_list(F),
	    Ch = array:from_list(lists:seq(0, length(M)-1)),
	    Child1 = cycle_crossover(Ma, Fa,0,Ch),		
	    Child2 = cycle_crossover(Fa, Ma,0,Ch),
	    {Child1, Child2}
    end.

cycle_crossover(M, F, Index, Child)->
    {N,_,_}=Elt = array:get(Index, M), %1
    case whereis(N,0,Child) of
	undefined ->
	    {N2,_,_} = array:get(Index, F), %8
	    C1=array:set(Index, Elt, Child), %put 1 in 0th place

	    Pos = whereis(N2, 0, M), %find8 in M
	    cycle_crossover(M, F, Pos, C1);
	_  ->
	    ChildList = array:to_list(Child),
	    FatherList= array:to_list(F),
	    lists:zipwith(fun(X,Y) ->
				  case X of
				      {_, _, _} -> X;
				      _ -> Y
				  end
			  end,
			  ChildList, FatherList)
    end.

whereis(N2, Index, M) ->
    case Index > array:size(M) of
	true -> undefined;
	false ->
	    case array:get(Index, M) of
		{N2, _, _}  -> Index;
		_  -> whereis(N2, Index + 1, M)
	    end
    end.

	    
success(_) ->
    fun(Score, _Rout) ->
	    Score > -27605
    end.
    

seed(_)->
    Data = 
	[
	 {1, 20833.3333, 17100.0000},
	 {2, 20900.0000, 17066.6667},
	 {3, 21300.0000, 13016.6667},
	 {4, 21600.0000, 14150.0000},
	 {5, 21600.0000, 14966.6667},
	 {6, 21600.0000, 16500.0000},
	 {7, 22183.3333, 13133.3333},
	 {8, 22583.3333, 14300.0000},
	 {9, 22683.3333, 12716.6667},
	 {10, 23616.6667, 15866.6667},
	 {11, 23700.0000, 15933.3333},
	 {12, 23883.3333, 14533.3333},
	 {13, 24166.6667, 13250.0000},
	 {14, 25149.1667, 12365.8333},
	 {15, 26133.3333, 14500.0000},
	 {16, 26150.0000, 10550.0000},
	 {17, 26283.3333, 12766.6667},
	 {18, 26433.3333, 13433.3333},
	 {19, 26550.0000, 13850.0000},
	 {20, 26733.3333, 11683.3333},
	 {21, 27026.1111, 13051.9444},
	 {22, 27096.1111, 13415.8333},
	 {23, 27153.6111, 13203.3333},
	 {24, 27166.6667, 9833.3333},
	 {25, 27233.3333, 10450.0000},
	 {26, 27233.3333, 11783.3333},
	 {27, 27266.6667, 10383.3333},
	 {28, 27433.3333, 12400.0000},
	 {29, 27462.5000, 12992.2222}
	],
    fun() ->
	    [{A,B,C} || {_,{A,B,C}} <- lists:sort([ {crypto:rand_uniform(1, 30), {A,B,C}} || {A,B,C} <- Data])]
    end.



fit_fn(_)->
    fun(Root) -> -1*(calc_cost(Root, hd(Root), 0)) end.

calc_cost([], _, Cost) -> Cost;
calc_cost([{_Nd, Xi, Yi},{_Nd2, Xj,Yj}|Rest], Start, Cost) ->
    calc_cost(Rest, Start, Cost + cost({Xi, Yi},{Xj,Yj}));
calc_cost([{_Nd, Xi, Yi}|Rest], {_Nd2, Xj, Yj}=Start, Cost) ->
    calc_cost(Rest, Start, Cost + cost({Xi, Yi},{Xj,Yj})).

    
cost({Xi, Yi}, {Xj, Yj}) ->
     Lati = ?PI * Xi / 180.0,
     Latj = ?PI * Xj / 180.0,

     Longi = ?PI * Yi / 180.0,
     Longj = ?PI * Yj / 180.0,

     Q1 = math:cos(Latj) * math:sin(Longi - Longj),
     Q3 = math:sin((Longi - Longj)/2.0),
     Q4 = math:cos((Longi - Longj)/2.0),
     Q2 = math:sin(Lati + Latj) * Q3 * Q3 - math:sin(Lati - Latj) * Q4 * Q4,
     Q5 = math:cos(Lati - Latj) * Q4 * Q4 - math:cos(Lati + Latj) * Q3 * Q3,
     (6378388.0 * math:atan2(math:sqrt(Q1*Q1 + Q2*Q2), Q5) + 1.0).
    
