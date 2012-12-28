-module(population).
-compile(export_all).


-record(state, {
	  n                  ::integer(),   % Population size
	  elite_per=10       ::integer(),   % Percentage whose genes move on as-is
	  mate_per=20        ::integer(),   % Top X% that mate
	  crossover_per=90   ::integer(),   % Crossover rate of non-elites
	  tournament_per=2   ::integer(),   % Number of tournament winners as a % of N
	  success_fn         ::fun(),
	  crossover_fn       ::fun(),
	  gen                ::integer()
	 }).


ga(Size, Target, Callback)->
    FitFn = Callback:fit_fn(Target),
    SuccessFn = Callback:success(Target),
    SeedFn = Callback:seed(Target),
    MutateFn = Callback:mutate(20),
    CrossoverFn = Callback:crossover(),
    seek(Size, FitFn, SuccessFn, SeedFn, MutateFn, CrossoverFn).


seek(Size, FitFn, SuccessFn, SeedFn, MutateFn, CrossoverFn) ->
    worker_sup:start_link(),
    start_workers(SeedFn, FitFn, MutateFn, Size),
    State = #state{n=Size, 
		   gen=0,
		   elite_per=10, 
		   mate_per=30,
		   tournament_per=2,
		   crossover_per=100,
		   success_fn=SuccessFn,
		   crossover_fn=CrossoverFn},
    loop([], 0, State).


start_workers(SeedFn, FitFn, MutateFn, Size)->
    Args = [self(), SeedFn(), FitFn, MutateFn],
    Workers = [begin {ok, Pid} = worker_sup:start_worker(citizen, Args), Pid end
	       || _ <- lists:seq(1, Size)],
    [gen_server:cast(APid, calculate) || APid <- Workers].



loop(Scores, Cnt, #state{n=N} = State) when Cnt /= N->
    receive
	{score, From, Score} ->
	    loop([{Score, From} |Scores], Cnt + 1, State);
	_ ->
	    loop(Scores, Cnt, State)
    end;

loop(Scores, Cnt, #state{n=N, success_fn=SFn, gen=Gen}=State) when Cnt == N->
    Sorted = sort(Scores),
    {BestScore, BestPid} = hd(Sorted),
    BestGuess = citizen:get_chromosome(BestPid),
    case SFn(BestScore, BestGuess) of
	true ->
	    io:format("~nFound: score ~p~n chromosome: ~p in ~p gens.~n",
		      [BestScore, BestGuess, Gen]);
	false ->
	    case Gen rem 1000 of
		0 -> 
		    io:format("~nBest score ~p~nchromosome: ~p in ~p gens.~n",
			      [BestScore, BestGuess, Gen]);
		_ -> ok
	    end,
	    case Gen rem 100 of
		0 -> io:format("~nGen. ~p, ~p chidren, best:~p.~n", 
			       [Gen, length(Sorted), BestScore]);
		_ -> io:format(".",[])
	    end,
	    
	    choreograph(Sorted, State),
	    [gen_server:cast(APid, calculate) || {_, APid} <- Scores],
	    loop([], 0, State#state{gen=Gen+1})
    end.
        

	

choreograph(Sorted, #state{n=N, elite_per=ElitePer, mate_per=MatePer, 
			   crossover_per=CrossoverPer, tournament_per=TPer, 
			   crossover_fn=CFn})->

    TournSize = erlang:truc(N*(TPer/100)),
    {_Elites, NotElite} = lists:split(erlang:trunc(N * (ElitePer/100)), Sorted),
    {Best, Worst} = lists:split(erlang:trunc(N * (MatePer/100)), Sorted),
    Bests = [citizen:get_chromosome(Pid) || {_, Pid} <- Best],
    Lucky = [citizen:get_chromosome(Pid) || {_,Pid} <- tournament(Worst, TournSize)],
    Citizens = [Pid || {_, Pid} <- NotElite], %Elites left alone == no crossover
    Breeders = Lucky ++ Bests,
    mate(Breeders, Citizens, Breeders, CrossoverPer, CFn).

%take each breeder, two at a time, and mate them. Keep doing this until entire 
%population is re-seeded
mate(_, [], _, _, _) -> ok;
mate([], Remaining, Breeders, _C, _CFn) -> mate(Breeders, Remaining, Breeders, _C, _CFn);

mate([M, F |Rest], [C1, C2| Cs], Breeders, CrossoverPer, CFn) ->
    case crypto:rand_uniform(0, 100) of
	X when X > CrossoverPer ->
	    % clone
	    citizen:inject(C1, M),
	    citizen:inject(C2, F),
	    mate(Rest, Cs, Breeders, CrossoverPer, CFn);
	_ ->
	    {Chrom1, Chrom2} = CFn(M, F),
	    citizen:inject(C1, Chrom1),
	    citizen:inject(C2, Chrom2),
	    mate(Rest, Cs, Breeders, CrossoverPer, CFn)
    end;

mate([M|Rest], [C|Cs], Breeders, _C, _CFn) ->
    citizen:inject(C, M),
    mate(Rest, Cs, Breeders, _C, _CFn).


tournament(Population, Size) ->
    select(Population, [], Size).

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



