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

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, calc_charges/1]).

%% создание нового баффа
%% ф-я должна вернуть актуальный статус баффа
-callback new(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% ф-я вызывается по окончании хода пользователя
%% но после пересчета оставшихся зарядов
%-callback on_swap_done(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% ф-я вызывается при начале действия эффекта
-callback on_start(Buff0 :: #buff{}) -> Buff ::#buff{}.

%% ф-я вызывается при завершении действия эффекта
-callback on_end(Buff0 :: #buff{}) -> Buff ::#buff{}.


%% start_link/4
%% ====================================================================
%% запуск процессов баффа
start_link(Ev, Module, Unit, Options) ->
	Buff = #buff{
		id      = Module,
		unit    = Unit,
		owner   = proplists:get_value(owner, Options, Unit),
		value   = proplists:get_value(value, Options),
		time    = proplists:get_value(time, Options),
		charges = proplists:get_value(charges, Options)
	},

	Id = {Buff#buff.unit, Buff#buff.owner},
	case proplists:get_value(exists, Options) of
		true  -> gen_event:add_handler(Ev, {?MODULE, Id}, {exists, Module, Buff});
		_     -> gen_event:add_handler(Ev, {?MODULE, Id}, {Module, Buff})
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
			case Module:on_start(DefBuff) of
				{ok, Buff0} ->
					CallBacks = Module:module_info(exports),
					{ok, #state{mod = Module, callbacks = CallBacks, buff = Buff0}};
				{error, Error} ->
					{error, Error};
				_ ->
					{error, undef}
			end;
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
terminate(remove_handler, #state{mod = Module, buff = Buff} = _State) ->
	?DBG("Terminate buff ~p~n", [Module]),
	Module:on_end(Buff),
	ok;

terminate(swap, #state{mod = Module, buff = Buff} = _State) ->
	?DBG("Swap buff ~p~n", [Module]),
	{ok, Buff1} = Module:on_end(Buff),
	Buff1;

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
