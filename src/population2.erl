-module(population).
-compile(export_all).



create_population(Size, Target)->
    FitFn = fittness:fit_fn(Target),
    N = [begin
	     {ok, Pid} = citizen:start_link([list_to_atom([X,Y]),
					     make_chromosome(Target),
					     FitFn,
					     fun mutate/1,
					     fun crossover/2,
					     0.80,
					     0.20,
					     Size*Size]),
	     Pid
	 end || X<-lists:seq(1, Size), Y<-lsits:seq(1,Size)
%    N = [make_chromosome(Target) || _ <- lists:seq(1, Size)],
    loop(N, Target, FitFn, Size, 0).

loop(Population, Target, FitFn, Size, Generation) ->
    SortedPop = sort_population(FitFn, Population),
    case hd(SortedPop) of
	{_, Target} ->
	    io:format("~nFound target ~p after ~p generations~n",[Target, Generation]);
	{Score, Chromosome} ->
	    case Generation rem 10 of
		0 -> io:format("~n Generation ~p: size:~p, best: {~p, ~p}~n",[Generation, length(Population), Score, Chromosome]);
		_ -> io:format(".",[])
	    end,

	    NewPopulation = new_population(SortedPop, [], Size),
	    loop(NewPopulation, Target, FitFn, Size, Generation+1)
    end.


sort_population(FitFn, Population)->
    Tuples = [{FitFn(Chromosome), Chromosome} || Chromosome<-Population],
    lists:reverse(lists:keysort(1, Tuples)).

new_population(_Population, NewPop, TargetSize) when length(NewPop) >= TargetSize ->
    NewPop;

new_population(Population, NewPop, TargetSize) ->
    {Top, Bottom} = lists:split(erlang:trunc(TargetSize * 0.1), Population),
    Tournament = tournament(Bottom),
    Elite = mate(Tournament ++ Top, []),

    new_population(Population, Elite ++ NewPop, TargetSize).
    

tournament(Bottom) ->
    select(Bottom, []).

select([], Selected) -> 
    Selected;
select(_Pop, Selected) when length(Selected) == 2 ->
    Selected;
select(Pop, Selected) ->
    {F1, P1} = lists:nth(crypto:rand_uniform(1, length(Pop) + 1), Pop),
    Pop2 = lists:delete({F1, P1}, Pop),
    case length(Pop2) of
	N when N > 0 ->
	    {F2, P2} = lists:nth(crypto:rand_uniform(1, length(Pop2) + 1), Pop2),
	    Pop3 = lists:delete({F2, P2}, Pop2),
	    case F1 > F2 of
		true  -> select(Pop3, [{F1, P1} | Selected]);
		false -> select(Pop3, [{F2, P2} | Selected])
	    end;
	N when N == 0 ->
	    select(Pop2, [{F1, P1} | Selected])
    end.
		     
    


mate([], Children) ->
    mutate(Children);

mate([{_, M}, {_, F} |Rest], Children) ->
    case crypto:rand_uniform(0, 100) of
	X when X =< 10 ->
	    % clone
	    mate(Rest, [M | [F | Children]]);
	_ ->
	    mate(Rest, crossover(M, F) ++ Children)
    end;

mate([{_, M}|Rest], Children) ->
    mate(Rest, [M|Children]).

crossover(M, F) ->
    Pt = crypto:rand_uniform(2, length(M)),
    {M1,M2} = lists:split(Pt, M),
    {F1,F2} = lists:split(Pt, F),
    [M1 ++ F2, F1 ++ M2].

mutate(Children) ->
    F = fun(C)->
	    case crypto:rand_uniform(0, 100) of
		X when X =< 80 ->
		    C;
		_ ->
		    Pos = crypto:rand_uniform(0, length(C)),
		    NewChar = crypto:rand_uniform(32, 127),
		    {Left, Right} = lists:split(Pos, C),
		    Left ++ [NewChar] ++ tl(Right)
	    end
	end,
    lists:map(F, Children).



make_chromosome(Target)->
    random_str(length(Target)).


random_str(0) -> [];
random_str(Length) -> [random_char() | random_str(Length-1)].
random_char() -> crypto:rand_uniform(32, 127).