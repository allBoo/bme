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

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, calc_charges/1]).

-callback new(Buff0 :: #buff{}) -> Buff ::#buff{}.


%% start_link/4
%% ====================================================================
%% запуск процессов баффа
start_link(Ev, Module, Unit, Options) ->
	Buff = #buff{
		id = Module,
		unit = Unit,
		owner = proplists:get_value(owner, Options, Unit),
		time = proplists:get_value(time, Options),
		charges = proplists:get_value(charges, Options)
	},

	R = gen_event:add_handler(Ev, Module, [exists, Module, Buff]),
	?DBG("Event result = ~p~n", [R]).

%% 	case proplists:get_value(exists, Options) of
%% 		true  -> gen_event:add_handler(Ev, Module, [exists, Module, Buff]);
%% 		_     -> gen_event:add_handler(Ev, Module, [Module, Buff])
%% 	end.


%% calc_charges/4
%% ====================================================================
%% расчет кол-ва зарядов на основе времени действия баффа
calc_charges(Time) when is_integer(Time) ->
	trunc(Time / 15);

calc_charges(_) -> 0.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {mod, buff}).

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

init([exists, Module, Buff]) ->
	?DBG("Start exists buff ~p ~p", [Module, Buff]),
	case Module:new(Buff) of
		{ok, DefBuff} when is_record(DefBuff, buff) ->
			{ok, #state{mod = Module, buff = DefBuff}};
		{error, Error} ->
			{error, Error};
		_ ->
			{error, no_config}
	end;

init([Module, Buff]) ->
	?DBG("Start new buff ~p ~p", [Module, Buff]),
	case Module:new(Buff) of
		{ok, DefBuff} when is_record(DefBuff, buff) ->
			{ok, #state{mod = Module, buff = DefBuff}};
		{error, Error} ->
			{error, Error};
		_ ->
			{error, no_config}
	end;

init(_) ->
	{error, undefined_call}.


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


