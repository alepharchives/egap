-module(population).
-compile(export_all).


-record(state, {
	  pop_size           ::integer(),
	  elite_per=10       ::integer(),
	  crossover_per=90   ::integer(),
	  tournament_size=2  ::integer(),
	  success_fn         ::fun(),
	  generation         ::integer()
	 }).


create_population(Size, Target)->    
    create_population(Size, Target, 80, 10, 90, 2, fittness).

create_population(Size, Target, MutationRate, ElitePer, Crossover, TournamentSize, FitCB)->
    worker_sup:start_link(),
    FitFn = FitCB:fit_fn(Target),
    SuccessFn = FitCB:success(Target),
    ChromGen = FitCB:seed(Target),
    N = [begin
	     {ok, Pid} = worker_sup:start_worker(citizen, [self(), ChromGen, FitFn, MutationRate]),
	     Pid
	 end || _ <- lists:seq(1, Size)],
    [gen_server:cast(APid, calculate) || APid <- N],
    State = #state{pop_size=Size, 
		   elite_per=ElitePer, 
		   crossover_per=Crossover,
		   tournament_size=TournamentSize,
		   success_fn=SuccessFn,
		   generation=0},
    loop([], 0, State).


loop(Scores, Reporting, #state{pop_size=Size, success_fn=SFn, generation=Gen}=State) when Reporting == Size->
    Sorted = sort(Scores),
    {_, BestPid} = hd(Sorted),
    BestGuess = citizen:get_chromosome(BestPid),
    case SFn(BestGuess) of
	true ->
	    io:format("~nFound ~p in ~p generations.~n",[BestGuess, Gen]);
	false ->
	    case Gen rem 10 of
		0 -> io:format("~nGen. ~p, ~p chidren, best:~p.~n", [Gen, length(Sorted), BestGuess]);
		_ -> io:format(".",[])
	    end,
	    
	    choreograph(Sorted, State),
	    [gen_server:cast(APid, calculate) || {_, APid} <- Scores],
	    loop([], 0, State#state{generation=Gen+1})
    end;
        
loop(Scores, Reporting, State) ->
    receive
	{fittness, From, Score} ->
	    loop([{Score, From} |Scores], Reporting + 1, State);
	_ ->
	    loop(Scores, Reporting, State)
    end.
	

choreograph(Sorted, #state{pop_size=Size, elite_per=ElitePer, crossover_per=CrossoverPer, tournament_size=TS})->
    {Best, Worst} = lists:split(erlang:trunc(Size * (ElitePer/100)), Sorted),
    Elite = [citizen:get_chromosome(Pid) || {_, Pid} <- Best],
    Lucky = [citizen:get_chromosome(Pid) || {_,Pid} <- tournament(Worst, TS)],
    Citizens = [Pid || {_, Pid} <- Sorted],
    mate(Lucky ++ Elite, Citizens, Lucky++ Elite, CrossoverPer).

mate(_, [], _, _) -> ok;
mate([], Remaining, DNA, _C) -> mate(DNA, Remaining, DNA, _C);

mate([M, F |Rest], [C1, C2| Cs], DNA, CrossoverPer) ->
    case crypto:rand_uniform(0, 100) of
	X when X =< CrossoverPer ->
	    % clone
	    citizen:inject(C1, M),
	    citizen:inject(C2, F),
	    mate(Rest, Cs, DNA, CrossoverPer);
	_ ->
	    crossover(M, F, C1, C2),
	    mate(Rest, Cs, DNA, CrossoverPer)
    end;

mate([M|Rest], [C|Cs], DNA, _C) ->
    citizen:inject(C, M),
    mate(Rest, Cs, DNA, _C).

crossover(M, F, C1, C2) ->
    Pt = crypto:rand_uniform(2, length(M)),
    {M1, M2} = lists:split(Pt, M),
    {F1, F2} = lists:split(Pt, F),
    citizen:inject(C1, M1++F2),
    citizen:inject(C2, F1++M2).

        

tournament(Bottom, Size) ->
    select(Bottom, [], Size).
select([], Selected, _Size) -> 
    Selected;
select(_Pop, Selected, Size) when length(Selected) >= Size ->
    Selected;
select(Pop, Selected, Size) ->
    {F1, P1} = lists:nth(crypto:rand_uniform(1, length(Pop) + 1), Pop),
    Pop2 = lists:delete({F1, P1}, Pop),
    case length(Pop2) of
	N when N > 0 ->
	    {F2, P2} = lists:nth(crypto:rand_uniform(1, length(Pop2) + 1), Pop2),
	    Pop3 = lists:delete({F2, P2}, Pop2),
	    case F1 > F2 of
		true  -> select(Pop3, [{F1, P1} | Selected], Size);
		false -> select(Pop3, [{F2, P2} | Selected], Size)
	    end;
	N when N == 0 ->
	    select(Pop2, [{F1, P1} | Selected], Size)
    end.


sort(Scores)->
    lists:reverse(lists:keysort(1, Scores)).




