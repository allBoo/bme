%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% менеджер баффов
%%% ====================================================================


-module(buff_mgr).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CAST(BuffMgr, Cmd), case is_pid(BuffMgr) of
								 true -> gen_server:cast(BuffMgr, Cmd);
								 false ->
									 case reg:find({buff_mgr, BuffMgr}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 BuffMgrPid -> gen_server:cast(BuffMgrPid, Cmd) end end).
-define(CALL(BuffMgr, Cmd), case is_pid(BuffMgr) of
								 true -> gen_server:call(BuffMgr, Cmd);
								 false ->
									 case reg:find({buff_mgr, BuffMgr}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 BuffMgrPid -> gen_server:call(BuffMgrPid, Cmd) end end).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_ev/1,
		 start_mgr/1,
		 apply/3,
		 apply/2,
		 notify/2,
		 list/1]).

%% callbacks
-export([on_before_got_damage/2,
		 on_after_got_damage/2,
		 on_hit_damage/2,
		 on_before_got_heal/2,
		 on_calc_damage/2]).


%% start_mgr/1
%% ====================================================================
%% регистрирует процесс event-manager
start_ev(Unit) ->
	{ok, Pid} = gen_event:start_link(),
	reg:set({buff_ev, Unit#b_unit.id}, Pid),
	{ok, Pid}.


%% start_mgr/1
%% ====================================================================
%% регистрирует процесс нового участника боя
start_mgr(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% apply/3
%% ====================================================================
%% применение баффа на юнита
apply(BuffMgr, Unit, Buff) when (is_record(Buff, u_buff) or is_record(Buff, buff)) ->
	?CALL(BuffMgr, {apply, Unit, Buff}).


apply(UnitId, Buff) when is_integer(UnitId),
						 (is_record(Buff, u_buff) or is_record(Buff, buff)) ->
	?CALL(UnitId, {apply, UnitId, Buff}).


%% notify/2
%% ====================================================================
%% асинхронное уведомление баффов
notify(BuffMgr, Event) ->
	?CAST(BuffMgr, {notify, Event}).


%% list/1
%% ====================================================================
%% возвращает список запущенных баффов
list(BuffMgr) ->
	?CALL(BuffMgr, list).


%% callbacks
on_before_got_damage(Unit, HitResult) ->
	?CALL(Unit, {on_before_got_damage, HitResult}).


on_after_got_damage(Unit, HitResult) ->
	?CALL(Unit, {on_after_got_damage, HitResult}).


on_hit_damage(Unit, HitResult) ->
	?CALL(Unit, {on_hit_damage, HitResult}).


on_before_got_heal(Unit, Heal) ->
	?CALL(Unit, {on_before_got_heal, Heal}).

on_calc_damage(Unit, Damage) ->
	?CALL(Unit, {on_calc_damage, Damage}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {unit :: pid(), event_mgr :: pid()}).


%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(Unit) ->
	process_flag(trap_exit, true),
	case reg:find({unit, Unit#b_unit.id}) of
		undefined -> {stop, unit_not_found};
		UnitPid   ->
			case reg:get({buff_ev, Unit#b_unit.id}) of
				undefined -> {stop, buff_event_mgr_not_found};
				BuffEv    ->
					ok = reg:name({buff_mgr, Unit#b_unit.id}),
					reg:bind({unit, Unit#b_unit.id}),

					%% запускаем уже наложенные баффы
					%% @attention используем b_unit.id т.к. сюда приходим из супервайзера, где нет unit pid
					lists:foreach(fun(Buff) -> apply_exists(BuffEv, UnitPid, Buff) end, user_state:get(Unit#b_unit.id, 'buffs')),
					{ok, #state{unit = UnitPid, event_mgr = BuffEv}}
			end
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

%% наложение баффа на юнита
handle_call({apply, UnitId, Buff}, From, State) when is_integer(UnitId) ->
	case reg:find({unit, UnitId}) of
		undefined -> {reply, ?ERROR_NOT_IN_BATTLE, State};
		UnitPid   -> handle_call({apply, UnitPid, Buff}, From, State)
	end;

handle_call({apply, UnitPid, Buff}, _From, State) when is_pid(UnitPid) ->
	R = apply_buff(State#state.event_mgr, UnitPid, Buff),
	{reply, R, State};


%% возвращает список активных баффов юнита
handle_call(list, _From, State) ->
	List = gen_event:which_handlers(State#state.event_mgr),
	{reply, List, State};


% обработка перед получением урона юнитом
handle_call({on_before_got_damage, HitResult}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitResult1 = lists:foldl(fun(Buff, HitResult0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_got_damage, HitResult0})
				end, HitResult, Buffs),
	{reply, HitResult1, State};


% обработка после получения урона юнитом
handle_call({on_after_got_damage, HitResult}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitResult1 = lists:foldl(fun(Buff, HitResult0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_after_got_damage, HitResult0})
				end, HitResult, Buffs),
	{reply, HitResult1, State};


% обработка нанесения урона юнитом
handle_call({on_hit_damage, HitResult}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitResult1 = lists:foldl(fun(Buff, HitResult0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_hit_damage, HitResult0})
				end, HitResult, Buffs),
	{reply, HitResult1, State};


% обработка перед хиллом юнита
handle_call({on_before_got_heal, Heal}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	Heal1 = lists:foldl(fun(Buff, Heal0) ->
					gen_event:call(State#state.event_mgr, Buff, {on_before_got_heal, Heal0})
			end, Heal, Buffs),
	{reply, Heal1, State};


% обработка при расчете наносимого урона
handle_call({on_calc_damage, Damage}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	Damage1 = lists:foldl(fun(Buff, Damage0) ->
					gen_event:call(State#state.event_mgr, Buff, {on_calc_damage, Damage0})
			  end, Damage, Buffs),
	{reply, Damage1, State};


%% unknown request
handle_call(_Request, _From, State) ->
	{reply, ok, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast({notify, Event}, State) ->
	?DBG("NOTYFY ~p~n", [Event]),
	gen_event:notify(State#state.event_mgr, Event),
	{noreply, State};


%% unknown request
handle_cast(_Msg, State) ->
	{noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

%% messages from unit
handle_info({unit, Msg}, State) ->
	?DBG("START NOTIFY ~p~n", [Msg]),
	gen_event:notify(State#state.event_mgr, Msg),
	?DBG("DONE NOTIFY ~p~n", [Msg]),
	{noreply, State};


%% unknows message
handle_info(_Info, State) ->
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% apply_exists/3
%% ====================================================================
%% запуск существующего баффа
apply_exists(Ev, UnitPid, Buff) when is_pid(Ev),
									 is_record(Buff, u_buff) ->
	apply_buff(Ev, UnitPid, Buff#u_buff{exists = true}).


%% запуск нового баффа
apply_buff(Ev, UnitPid, Buff) when is_pid(Ev),
								   is_record(Buff, u_buff) ->
	Options = [{time, Buff#u_buff.time},
			   {value, Buff#u_buff.value},
			   {level, Buff#u_buff.level},
			   {exists, Buff#u_buff.exists}],
	start_buff(Ev, Buff#u_buff.id, UnitPid, Options);

apply_buff(Ev, UnitPid, Buff) when is_pid(Ev),
								   is_record(Buff, buff) ->
	Options = [{time, Buff#buff.time},
			   {value, Buff#buff.value},
			   {level, Buff#buff.level}],
	start_buff(Ev, Buff#u_buff.id, UnitPid, Options).


start_buff(Ev, BuffId, UnitPid, BuffOptions) ->
	gen_buff:start_link(Ev, BuffId, UnitPid, BuffOptions).
