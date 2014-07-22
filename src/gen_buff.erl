%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех баффов
%%% ====================================================================


-module(gen_buff).
-behaviour(gen_event).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(default(X1, X2), case X1 of undefined -> X2; _ -> X1 end).
-define(callback(M, F, State), case is_callable(F, State) of true  -> M:F(State#state.buff); false -> {ok, State#state.buff} end).
-define(callback(M, F, Args, State), case is_callable(F, State) of true  -> M:F(Args, State#state.buff); false -> {ok, State#state.buff} end).
-define(result(R), case R of {ok, Buff0} -> {ok, State#state{buff = Buff0}}; {error, Error} -> {error, Error}; _ -> {error, undef} end).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, calc_charges/1]).

%% создание нового баффа
%% ф-я должна вернуть актуальный статус баффа
-callback new(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% обновление баффа
%-callback renew(Buff0 :: #buff{}, Buff :: #buff{}) -> BuffNew ::#buff{}.

%% ф-я вызывается по окончании хода пользователя
%% но после пересчета оставшихся зарядов
%-callback on_swap_done(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% ф-я вызывается при начале действия эффекта
%-callback on_start(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% ф-я вызывается при завершении действия эффекта
%-callback on_end(Buff0 :: #buff{}) -> Buff ::#buff{}.


%% start_link/4
%% ====================================================================
%% запуск процессов баффа
start_link(Ev, Module, Unit, Options) ->
	Buff = #buff{
		id      = Module,
		unit    = Unit,
		level   = proplists:get_value(level, Options),
		owner   = proplists:get_value(owner, Options, Unit),
		value   = proplists:get_value(value, Options),
		time    = proplists:get_value(time, Options),
		charges = proplists:get_value(charges, Options),
		state   = proplists:get_value(state, Options, [])
	},

	Id = {Module, Buff#buff.unit, Buff#buff.owner},
	%% если бафф уникальный, ищем существующий
	case Buff#buff.uniq andalso find_exists(Ev, Id) of
		%% если нету, запускаем новый
		false ->
			?DBG("Start buff ~p~n", [{Ev, Buff#buff.unit, Buff}]),
			case proplists:get_value(exists, Options) of
				true  -> gen_event:add_handler(Ev, {?MODULE, Id}, {exists, Module, Buff});
				_     -> gen_event:add_handler(Ev, {?MODULE, Id}, {Module, Buff})
			end;
		%% если есть и бафф поддерживает ф-ю renew, вызываем ее
		%% иначе возвращаем ошибку
		true ->
			?DBG("Renew buff ~p~n", [Buff]),
			gen_event:call(Ev, {?MODULE, Id}, {renew, Buff})
	end.


%% calc_charges/4
%% ====================================================================
%% расчет кол-ва зарядов на основе времени действия баффа
calc_charges(Time) when is_integer(Time) ->
	trunc(Time / 15);

calc_charges(_) -> 0.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {mod, callbacks, buff}).

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

init({exists, Module, Buff}) ->
	case Module:new(Buff) of
		{ok, DefBuff} when is_record(DefBuff, buff) ->
			%% получаем определенные коллбеки в модуле
			CallBacks = Module:module_info(exports),
			{ok, #state{mod = Module, callbacks = CallBacks, buff = DefBuff}};
		{error, Error} ->
			{error, Error};
		_ ->
			{error, no_config}
	end;

init({Module, Buff}) ->
	case Module:new(Buff) of
		{ok, DefBuff} when is_record(DefBuff, buff) ->
			CallBacks = Module:module_info(exports),
			State = #state{mod = Module, callbacks = CallBacks, buff = DefBuff},
			call_on_start(State);
		{error, Error} ->
			{error, Error};
		_ ->
			{error, undef}
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
handle_event(swap_done, #state{mod = Module, buff = Buff} = State) ->
	Buff0 = decrease_charges(Buff),
	case ?callback(Module, on_swap_done, State#state{buff = Buff0}) of
		{ok, Buff1} ->
			case check_charges(Buff1) of
				0 -> remove_handler;
				_ -> {ok, State#state{buff = Buff1}}
			end;
		_ ->
			remove_handler
	end;

%% изменение параметров юнита
handle_event({change_state, Change}, #state{mod = Module} = State) ->
	case ?callback(Module, on_unit_state_change, Change, State) of
		{ok, Buff1} ->
			{ok, State#state{buff = Buff1}};
		{swap, Module1, Buff1} ->
			{swap_handler, swap, State#state{buff = Buff1}, gen_buff, Module1};	%% eg init({Module, Buff})
		_ ->
			remove_handler
	end;


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

%% возвращает всю информацию по баффу
handle_call(get_buff, #state{buff = Buff} = State) ->
	{ok, Buff, State};

%% обработка обновления эффекта
handle_call({renew, Buff}, #state{mod = Module} = State) ->
	%% если бафф поддерживает ф-ю обновления вызываем
	%% иначе возвращаем ошибку
	case is_callable(renew, State) of
		true  ->
			case Module:renew(Buff, State#state.buff) of
				{ok, Buff1} ->
					{ok, ok, State#state{buff = Buff1}};
				{swap, Module1, Buff1} ->
					{swap_handler, ok, swap, State#state{buff = Buff1}, gen_buff, Module1};
				_ ->
					{ok, ?ERROR_UNDEFINED, State}
			end;
		false ->
			{ok, ?ERROR_BUFF_EXISTS, State}
	end;


%% обработка баффов на получение урона юнитом
handle_call({on_before_got_damage, HitResult}, State) ->
	apply_data_callback(on_unit_before_damage, HitResult, State);

%% обработка баффов на получение урона юнитом
handle_call({on_after_got_damage, HitResult}, State) ->
	apply_data_callback(on_unit_after_damage, HitResult, State);

%% обработка баффов перед нанесением удара
handle_call({on_before_hit, HitQueue}, State) ->
	apply_data_callback(on_unit_before_hit, HitQueue, State);

%% обработка баффов перед отражением удара
handle_call({on_before_defend, HitQueue}, State) ->
	apply_data_callback(on_unit_before_defend, HitQueue, State);

%% обработка баффов на нанесение урона юнитом
handle_call({on_hit_damage, HitResult}, State) ->
	apply_data_callback(on_unit_hit_damage, HitResult, State);

%% обработка баффов на избегание урона юнитом
handle_call({on_avoid_damage, HitResult}, State) ->
	apply_data_callback(on_unit_avoid_damage, HitResult, State);

%% обработка баффов на хилл юнита
handle_call({on_before_got_heal, Heal}, State) ->
	apply_data_callback(on_unit_before_heal, Heal, State);

%% обработка баффов влияющих на базовый урон
handle_call({on_before_calc_damage, HitData}, State) ->
	apply_data_callback(on_unit_before_calc_damage, HitData, State);

%% обработка при расчете наносимого урона
handle_call({on_calc_damage, Damage}, State) ->
	apply_data_callback(on_unit_calc_damage, Damage, State);

%% обработка баффов, перенаправляющих урон при разгадайке
handle_call({on_before_unravel, Data}, State) ->
	apply_data_callback(on_unit_before_unravel, Data, State);

%% обработка баффов, защищающих от разгадайки
handle_call(is_unravel_protected, State) ->
	apply_bool_callback(is_unravel_protected, State);

%% обработка баффов, защищающих от ставки
handle_call(is_steal_protected, State) ->
	apply_bool_callback(is_steal_protected, State);


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

%% завершение действия баффа
terminate(remove_handler, State) ->
	?DBG("Terminate buff ~p~n", [State#state.mod]),
	call_on_end(State),
	ok;

terminate(swap, State) ->
	?DBG("Swap buff ~p~n", [State#state.mod]),
	{ok, State1} = call_on_end(State),
	State1#state.buff;

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

call_on_start(#state{mod = Module, buff = Buff} = State) ->
	R = case is_callable(on_start, State) of
		true  -> Module:on_start(Buff);
		false -> do_on_start(Buff)
	end,
	?result(R).

call_on_end(#state{mod = Module, buff = Buff} = State) ->
	R = case is_callable(on_end, State) of
		true  -> Module:on_end(Buff);
		false -> do_on_end(Buff)
	end,
	?result(R).

do_on_start(Buff) when is_list(Buff#buff.value) ->
	user_state:increase(unit:get_user_pid(Buff#buff.unit), Buff#buff.value),
	{ok, Buff};
do_on_start(_) ->
	{error, undef}.

do_on_end(Buff) when is_list(Buff#buff.value) ->
	user_state:reduce(unit:get_user_pid(Buff#buff.unit), Buff#buff.value),
	{ok, Buff};
do_on_end(_) ->
	{error, undef}.


decrease_charges(Buff) ->
	case Buff#buff.charges of
		undefined -> Buff;
		X when is_integer(X) -> Buff#buff{charges = X - 1}
	end.

check_charges(Buff) ->
	case Buff#buff.charges of
		X when X =< 0 -> 0;
		_ -> Buff#buff.charges
	end.


is_callable(Fn, State) ->
	case lists:keyfind(Fn, 1, State#state.callbacks) of
		false -> false;
		_     -> true
	end.


apply_data_callback(Callback, Data, #state{mod = Module} = State) ->
	case ?callback(Module, Callback, Data, State) of
		{ok, Data1, Buff1} ->
			{ok, Data1, State#state{buff = Buff1}};
		{ok, Buff1} ->
			{ok, Data, State#state{buff = Buff1}};
		{swap, Data1, Module1, Buff1} ->
			{swap_handler, Data1, swap, State#state{buff = Buff1}, gen_buff, Module1};
		{remove_handler, Data1} ->
			{remove_handler, Data1};
		_ ->
			{remove_handler, Data}
	end.


apply_bool_callback(Callback, #state{mod = Module} = State) ->
	case ?callback(Module, Callback, State) of
		{ok, true, Buff} ->
			{ok, true, State#state{buff = Buff}};
		{ok, false, Buff} ->
			{ok, false, State#state{buff = Buff}};
		{ok, Buff} ->
			{ok, false, State#state{buff = Buff}};
		_ ->
			{ok, ?ERROR_UNDEFINED, State}
	end.

find_exists(Ev, BuffId) ->
	Buffs = gen_event:which_handlers(Ev),
	lists:keymember(BuffId, 2, Buffs).

