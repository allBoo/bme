%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Контроллер AI noob level
%%% ====================================================================


-module(ai0).
-behaviour(gen_fsm).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1,
		 waiting/2,
		 alive/2,
		 handle_event/3,
		 handle_sync_event/4,
		 handle_info/3,
		 terminate/3,
		 code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).


%% start_link/1
%% ====================================================================
%% регистрирует процесс нового AI
start_link(Unit) when is_record(Unit, b_unit) ->
	case reg:find({unit, Unit#b_unit.id}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UnitPid   -> start_link(UnitPid)
	end;

start_link(UnitPid) when is_pid(UnitPid) ->
	gen_fsm:start_link(?MODULE, UnitPid, []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {unit_pid}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init(UnitPid) when is_pid(UnitPid) ->
	?DBG("Start AI for ~p~n", [UnitPid]),
	process_flag(trap_exit, true),
	random:seed(now()),

	%% получаем инфу об юните
	case unit:get_state(UnitPid) of
		Unit when is_record(Unit, b_unit) ->
			%% подписываемся на его ивенты и ждем выбора противника
			subscribe(Unit),
			{ok, waiting, #state{unit_pid = UnitPid}};
		%% если что-то не так - отключаемся
		_ ->
			{stop, normal}
	end.


%% alive/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec alive(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

%% находимся в ждущем режиме
waiting(_Event, StateData) ->
	{next_state, waiting, StateData}.


%% в активном режиме
alive(_Event, StateData) ->
	?DBG("AI ~p start new hit~n", [StateData#state.unit_pid]),
	%% получаем инфу по юниту
	Unit = unit:get_state(StateData#state.unit_pid),
	case Unit#b_unit.alive of
		%% если юнит жив, то выставляем рандомный удар
		%% и уходим в ожидание следующего противника
		true ->
			unit:hit(StateData#state.unit_pid, get_hits(Unit), get_block(Unit)),
			{next_state, waiting, StateData};
		%% если нет, то стопаем
		false ->
			{stop, normal, StateData}
	end.



%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.


%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================

%% юнит выбрал противника
handle_info({unit, {new_opponent, Opponent}}, StateName, StateData) ->
	%% получаем инфу по оппоненту
	case Opponent of
		undefined -> {next_state, StateName, StateData};
		_ ->
			%%_Opponent = unit:get_state(Opponent#b_opponent.pid),
			%% определяем таймаут для выставления удара
			%% @todo если оппонент - бот, то тайм 8-10 сек, иначе 1-2 сек
			Timeout = case Opponent#b_opponent.ai of
						  true  -> 10;%8000 + random:uniform(2000);
						  false -> 1000 + random:uniform(1000)
					  end,
			%% используем свой таймер, т.к. state может быть переписан в handle_info
			timer:apply_after(Timeout, gen_fsm, send_event, [self(), hit]),
			{next_state, alive, StateData}
	end;


%% уведомление о убитом юните
handle_info({unit, {killed, UnitPid}}, _, StateData) when StateData#state.unit_pid == UnitPid ->
	{stop, normal, StateData};


%% unknown request
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.


%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _StateName, _StatData) ->
	ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% subscribe/1
%% ====================================================================
%% подписка на изменения юнита
subscribe(Unit) ->
	reg:bind({unit, Unit#b_unit.id}).


%% get_hits/1
%% ====================================================================
%% генерирует рандомный удар
get_hits(Unit) ->
	random_hits(((Unit#b_unit.user)#user.battle_spec)#u_battle_spec.hit_points).

random_hits(0) -> [];

random_hits(L) ->
	[random_hit() | random_hits(L-1)].


%% get_block/1
%% ====================================================================
%% генерирует рандомный блок
get_block(_Unit) ->
	random_hit().

random_hit() ->
	lists:nth(random:uniform(5), [head, torso, paunch, belt, legs]).
