%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех приемов
%%% ====================================================================


-module(gen_trick).
-behaviour(gen_event).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).


%% start_link/4
%% ====================================================================
%% запуск процессов баффа
start_link(Ev, TrickId, Unit) ->
	%% запускаем хендлер
	gen_event:add_handler(Ev, {?MODULE, TrickId}, {TrickId, Unit}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:init-1">gen_event:init/1</a>
-spec init(InitArgs) -> Result when
	InitArgs :: Args | {Args, Term :: term()},
	Args :: term(),
	Result :: {ok, State}
			| {ok, State, hibernate}
			| {error, Reason :: term()},
	State :: term().
%% ====================================================================

init({TrickId, Unit}) when is_atom(TrickId),
						   is_pid(Unit) ->
	try
		%% получаем описание приема
		Trick = tricks:TrickId(),

		%% формируем стейт
		State = #b_trick{
			trick   = Trick,
			unit    = Unit,
			active  = false,
			delay   = Trick#trick.initial_delay,
			mana    = Trick#trick.mana,
			tactics = Trick#trick.tactics
		},
		{ok, State}
	catch
		error:undef ->
			{error, ?ERROR_TRICK_NOT_EXISTS}
	end;


init(_) ->
	{error, undef}.


%% handle_event/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_event-2">gen_event:handle_event/2</a>
-spec handle_event(Event :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handlers, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================

%% обработка конца хода
handle_event({swap_done, Unit}, #b_trick{delay = Delay} = State) ->
	%% уменьшаем задержку и делаем проверку на доступность приема
	{ok, check_active(State#b_trick{delay = max(Delay - 1, 0)}, Unit)};


%% изменение параметров юнита
handle_event({unit_changed, Unit}, State) ->
	{ok, check_active(State, Unit)};


%% Пересчет доступности приемов
handle_event({recalc, Unit}, State) ->
	{ok, check_active(State, Unit)};


%% Уведомление о выполненном приеме
%% принимается приемами с классовой задержкой
handle_event({applied, AT}, #b_trick{trick = Trick} = State) when Trick#trick.class_delay == true,
																  AT =/= Trick#trick.id ->
	AppliedTrick = tricks:AT(),
	NewState = case AppliedTrick#trick.class == Trick#trick.class of
				   true ->
					   ?DBG("Deactivate trick ~p~n", [Trick#trick.id]),
					   State#b_trick{delay = AppliedTrick#trick.delay, active = false};
				   _ -> State
			   end,
	{ok, NewState};


%% unknown request
handle_event(_Event, State) ->
	{ok, State}.


%% handle_call/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_call-2">gen_event:handle_call/2</a>
-spec handle_call(Request :: term(), State :: term()) -> Result when
	Result :: {ok, Reply, NewState}
			| {ok, Reply, NewState, hibernate}
			| {swap_handler, Reply, Args1, NewState, Handler2, Args2}
			| {remove_handler, Reply},
	Reply :: term(),
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================

%% возвращает информацию по приему
handle_call(get_trick, #b_trick{trick = Trick} = State) ->
	{ok, Trick, State};


%% возвращает всю информацию по текущему состоянию приема
handle_call(get_state, State) ->
	{ok, State, State};


%% Пересчет доступности приемов
handle_call({recalc, Unit}, State) ->
	{ok, ok, check_active(State, Unit)};


%% выполнение приема
handle_call(apply, #b_trick{trick = Trick} = State) when State#b_trick.active == true,
														 Trick#trick.require_target == false ->
	case do_apply(State) of
		{ok, State0}  -> {ok, ok, State0};
		{Err, State0} -> {ok, Err, State0};
		Err -> {ok, Err, State}
	end;


handle_call({apply, Recipient}, #b_trick{trick = Trick} = State) when State#b_trick.active == true,
																	  Trick#trick.require_target == true ->
	case do_apply(State, Recipient) of
		{ok, State0}  -> {ok, ok, State0};
		{Err, State0} -> {ok, Err, State0};
		Err -> {ok, Err, State}
	end;

handle_call(apply, State) ->
	{ok, ?ERROR_TRICK_NOT_APPLICABLE, State};

handle_call({apply, _}, State) ->
	{ok, ?ERROR_TRICK_NOT_APPLICABLE, State};


%% unknown request
handle_call(_Request, State) ->
	{ok, ok, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_info-2">gen_event:handle_info/2</a>
-spec handle_info(Info :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handler, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================
handle_info(_Info, State) ->
	{ok, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:terminate-2">gen_event:terminate/2</a>
-spec terminate(Arg, State :: term()) -> term() when
	Arg :: Args
		| {stop, Reason}
		| stop
		| remove_handler
		| {error, {'EXIT', Reason}}
		| {error, Term :: term()},
	Args :: term(), Reason :: term().
%% ====================================================================

%% завершение действия приема
terminate(remove_handler, State) ->
	?DBG("Terminate trick ~p~n", [State#b_trick.trick]),
	ok;

terminate(_Arg, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:code_change-3">gen_event:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> {ok, NewState :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% check_active/2
%% ====================================================================
%% @doc Проверка доступности приема
check_active(#b_trick{trick = Trick} = State, Unit) ->
	Active = all([fun() -> State#b_trick.delay == 0 end,
				  fun() -> ?level(Unit#b_unit.user) >= Trick#trick.level end,
				  fun() -> ?mana(Unit#b_unit.user) >= State#b_trick.mana end,
				  fun() -> check_values(State#b_trick.tactics, Unit#b_unit.tactics) end,
				  fun() -> check_values(Trick#trick.stats, ?stats(Unit#b_unit.user)) end,
				  fun() -> check_values(Trick#trick.skills, ?skills(Unit#b_unit.user)) end]),
	?DBG("Check active ~p with result ~p~n", [Trick#trick.id, Active]),
	State#b_trick{active = Active}.

check_active(State) ->
	Unit = unit:get(State#b_trick.unit),
	User = user_state:get(Unit#b_unit.id),
	check_active(State, Unit#b_unit{user = User}).


check_values(TrickValues, UnitValues) when is_tuple(TrickValues),
										   is_tuple(UnitValues) ->
	check_values(erlang:tuple_to_list(TrickValues), erlang:tuple_to_list(UnitValues));

check_values([T | TV], [U | UV]) ->
	case U >= T of
		true  -> check_values(TV, UV);
		false -> false
	end;

check_values([], []) ->
	true.


all([Fn | Fns]) ->
	case Fn() of
		true  -> all(Fns);
		false -> false
	end;

all([]) -> true.


%% do_apply/1
%% ====================================================================
%% @doc Выполнение приема
do_apply(State) ->
	Opponent = unit:get(State#b_trick.unit, 'opponent.pid'),
	do_apply(check_active(State), State#b_trick.unit, Opponent).

%% do_apply/2
%% ====================================================================
%% @doc Выполнение приема с указанием цели
do_apply(#b_trick{trick = Trick} = State, RecipientPid) ->
	%% проверяем правильность указания цели
	UnitTeam      = unit:get(State#b_trick.unit, 'team_id'),
	RecipientTeam = unit:get(RecipientPid, 'team_id'),

	R = case Trick#trick.target_type of
			friendly ->
				case UnitTeam == RecipientTeam of
					true  -> ok;
					false -> ?ERROR_TRICK_FRIEND_ONLY
				end;
			enemy    ->
				case UnitTeam == RecipientTeam of
					false -> ok;
					true  -> ?ERROR_TRICK_ENEMY_ONLY
				end;
			_        -> ok
		end,
	case R of
		ok ->
			%% перепроверка доступности приема
			do_apply(check_active(State), State#b_trick.unit, RecipientPid);
		Err -> Err
	end.


do_apply(#b_trick{trick = Trick} = State, UnitPid, RecipientPid) when State#b_trick.active == true ->
	%% вычитаем тактики и ману
	unit:reduce(UnitPid, [{'tactics', State#b_trick.tactics}]),
	if
		State#b_trick.mana > 0 ->
			user_state:reduce(?puserpid(UnitPid), [{'vitality.mana', State#b_trick.mana}]);
		true -> ok
	end,

	%% порядок действий прерывается если что-то возвращает не ok
	Res = allok([
		%% накладываем бафф на себя
		fun() -> do_buff(Trick#trick.self_buff, UnitPid) end,
		%% накладываем бафф на противника
		fun() -> do_buff(Trick#trick.enemy_buff, RecipientPid) end,
		%% вызов калбека
		fun() -> do_call(Trick#trick.action, UnitPid, RecipientPid) end
	]),

	%% ставим задержку после использования
	if
		Res == ok -> {Res, State#b_trick{delay = max(Trick#trick.delay, 1)}};
		true -> {Res, State}
	end;


do_apply(_State, _UnitPid, _RecipientPid) ->
	?ERROR_TRICK_NOT_APPLICABLE.


do_buff(Buff0, UnitPid) ->
	case Buff0 of
		undefined -> ok;
		BuffId when is_atom(BuffId) ->
			UBuff = #u_buff{id = BuffId},
			buff_mgr:apply(unit:get_id(UnitPid), UBuff);
		Buff when is_record(Buff, u_buff) ->
			buff_mgr:apply(unit:get_id(UnitPid), Buff)
	end.

do_call(Action, UnitPid, RecipientPid) ->
	if
		is_function(Action, 0) ->
			Action();

		is_function(Action, 1) ->
			Action(UnitPid);

		is_function(Action, 2) ->
			Action(UnitPid, RecipientPid);

		true -> ok
	end.


allok([Fn | Fns]) ->
	case Fn() of
		ok  -> allok(Fns);
		Err -> Err
	end;

allok([]) -> ok.
