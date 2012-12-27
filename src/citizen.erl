
-module(citizen).

-behaviour(gen_server).

%% API
-export([start_link/1, get_chromosome/1, inject/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {coordinator,
		chromosome   ::string(),
		fittness     :: integer(),
		fit_fn       ::fun(),
		mutation_rate
	       }).

%%%===================================================================
%%% API
%%%===================================================================

get_chromosome(Server) ->
    gen_server:call(Server, get_chromosome).


inject(Server, Chromosome) ->
    gen_server:call(Server, {inject, Chromosome}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ParamList) ->
    gen_server:start_link(?MODULE, ParamList, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Parent, ChromFn, FitFn, MutationRate]) ->
    Chromosome = ChromFn(),
    io:format("[~p] up: ~p~n",[self(), Chromosome]),
    {ok, #state{coordinator=Parent,
		chromosome=Chromosome,
		fit_fn=FitFn,
		mutation_rate=MutationRate}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_chromosome, _From, #state{chromosome=Ch}= State) ->
    {reply, Ch, State};

handle_call({inject, Chromosome}, _From, #state{mutation_rate=M} = State) ->
    {reply, ok, State#state{chromosome=mutate(Chromosome, M), fittness=undefined}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(calculate, #state{fit_fn=Fn, chromosome=Chrom, coordinator= Parent}=State) ->
    Fittness = Fn(Chrom),
    Parent ! {fittness, self(), Fittness},
    {noreply, State#state{fittness=Fittness}};

handle_cast(stop, State)->
    {stop, received_stop, State};

handle_cast(_msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mutate(Chromosome, Probability) ->
    case crypto:rand_uniform(0, 100) of
	X when X =< Probability ->
	    Chromosome;
	_ ->
	    Pos = crypto:rand_uniform(0, length(Chromosome)),
	    NewChar = crypto:rand_uniform(32, 127),
	    {Left, Right} = lists:split(Pos, Chromosome),
	    Left ++ [NewChar] ++ tl(Right)
    end.
