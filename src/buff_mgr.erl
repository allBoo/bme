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

-define(CAST(Unit, Cmd), case reg:find({buff_mgr, Unit}) of
							 undefined   -> ?ERROR_NOT_IN_BATTLE;
							 BuffMgrPid -> gen_server:cast(BuffMgrPid, Cmd)
						 end).
-define(CALL(Unit, Cmd), case reg:find({buff_mgr, Unit}) of
							 undefined   -> ?ERROR_NOT_IN_BATTLE;
							 BuffMgrPid -> gen_server:call(BuffMgrPid, Cmd)
						 end).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_ev/1,
		 start_mgr/1,
		 apply/3,
		 apply/2,
		 notify/2,
		 list/1,
		 find/2]).

%% callbacks
-export([on_before_got_damage/2,
		 on_after_got_damage/2,
		 on_before_hit/2,
		 on_before_defend/2,
		 on_hit_damage/2,
		 on_avoid_damage/2,
		 on_before_got_heal/2,
		 on_before_calc_damage/2,
		 on_calc_damage/2]).

%% actions
-export([unravel/2, steal/2]).


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
apply(Unit, BuffId, Options) when is_atom(BuffId),
								  is_list(Options) ->
	?CALL(Unit, {apply, Unit, BuffId, Options}).

apply(Unit, Buff) when is_atom(Buff) ->
	buff_mgr:apply(Unit, Buff, []);

apply(Unit, Buff) when is_record(Buff, buff) ->
	?CALL(Unit, {apply, Unit, Buff}).

%% notify/2
%% ====================================================================
%% асинхронное уведомление баффов
notify(Unit, Event) ->
	?CAST(Unit, {notify, Event}).


%% list/1
%% ====================================================================
%% возвращает список запущенных баффов
list(Unit) ->
	case reg:find({buff_mgr, Unit}) of
		undefined  -> ?ERROR_NOT_IN_BATTLE;
		BuffMgrPid -> get_buffs_list(BuffMgrPid)
	end.


%% find/2
%% ====================================================================
%% возвращает список запущенных баффов
find(Unit, Id) ->
	UnitId = unit:get_id(Unit),
	case reg:find({buff_mgr, Unit}) of
		undefined  -> ?ERROR_NOT_IN_BATTLE;
		BuffMgrPid ->
			case reg:get({buff_ev, UnitId}) of
				undefined  -> ?ERROR_NOT_IN_BATTLE;
				Ev -> find_buff(BuffMgrPid, Ev, Id)
			end
	end.


%% callbacks
on_before_got_damage(Unit, HitResult) ->
	?CALL(Unit, {on_before_got_damage, HitResult}).


on_after_got_damage(Unit, HitResult) ->
	?CALL(Unit, {on_after_got_damage, HitResult}).


on_before_hit(Unit, HitQueue) ->
	?CALL(Unit, {on_before_hit, HitQueue}).


on_before_defend(Unit, HitQueue) ->
	?CALL(Unit, {on_before_defend, HitQueue}).


on_hit_damage(Unit, HitResult) ->
	?CALL(Unit, {on_hit_damage, HitResult}).


on_avoid_damage(Unit, HitResult) ->
	?CALL(Unit, {on_avoid_damage, HitResult}).


on_before_got_heal(Unit, Heal) ->
	?CALL(Unit, {on_before_got_heal, Heal}).


on_before_calc_damage(Unit, HitData) ->
	?CALL(Unit, {on_before_calc_damage, HitData}).


on_calc_damage(Unit, Damage) ->
	?CALL(Unit, {on_calc_damage, Damage}).


%% unravel/2
%% ====================================================================
%% @doc "Разгадать тактику"
unravel(Unit, FromUnit) ->
	?CAST(Unit, {unravel, FromUnit}).


%% steal/2
%% ====================================================================
%% @doc "Ставка на опережение"
steal(Unit, FromUnit) ->
	?CAST(Unit, {steal, FromUnit}).


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
					ok = reg:name([{buff_mgr, Unit#b_unit.id},
								   {buff_mgr, UnitPid}]),
					reg:bind({unit, Unit#b_unit.id}),

					%% запускаем уже наложенные баффы
					%% @attention используем b_unit.id т.к. сюда приходим из супервайзера, где нет user pid
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
handle_call({apply, UnitId, Buff}, From, State) when is_integer(UnitId),
													 is_record(Buff, buff) ->
	case reg:find({unit, UnitId}) of
		undefined -> {reply, ?ERROR_NOT_IN_BATTLE, State};
		UnitPid   -> handle_call({apply, UnitPid, Buff}, From, State)
	end;
handle_call({apply, UnitId, BuffId, Options}, From, State) when is_integer(UnitId) ->
	case reg:find({unit, UnitId}) of
		undefined -> {reply, ?ERROR_NOT_IN_BATTLE, State};
		UnitPid   -> handle_call({apply, UnitPid, BuffId, Options}, From, State)
	end;

handle_call({apply, UnitPid, Buff}, _From, State) when is_pid(UnitPid),
													   is_record(Buff, buff) ->
	R = apply_buff(State#state.event_mgr, UnitPid, Buff),
	{reply, R, State};

handle_call({apply, UnitPid, BuffId, Options}, _From, State) when is_pid(UnitPid) ->
	R = apply_buff(State#state.event_mgr, UnitPid, BuffId, Options),
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


% обработка перед ударом
handle_call({on_before_hit, HitQueue}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitQueue1 = lists:foldl(fun(Buff, HitQueue0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_hit, HitQueue0})
				end, HitQueue, Buffs),
	{reply, HitQueue1, State};


% обработка перед отражением удара
handle_call({on_before_defend, HitQueue}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitQueue1 = lists:foldl(fun(Buff, HitQueue0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_defend, HitQueue0})
				end, HitQueue, Buffs),
	{reply, HitQueue1, State};


% обработка нанесения урона юнитом
handle_call({on_hit_damage, HitResult}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitResult1 = lists:foldl(fun(Buff, HitResult0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_hit_damage, HitResult0})
				end, HitResult, Buffs),
	{reply, HitResult1, State};


% обработка при избегании урона
handle_call({on_avoid_damage, HitResult}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitResult1 = lists:foldl(fun(Buff, HitResult0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_avoid_damage, HitResult0})
				end, HitResult, Buffs),
	{reply, HitResult1, State};


% обработка перед хиллом юнита
handle_call({on_before_got_heal, Heal}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	Heal1 = lists:foldl(fun(Buff, Heal0) ->
					gen_event:call(State#state.event_mgr, Buff, {on_before_got_heal, Heal0})
			end, Heal, Buffs),
	{reply, Heal1, State};


% обработка перед расчетом наносимого урона
handle_call({on_before_calc_damage, HitData}, _From, State) ->
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	HitData1 = lists:foldl(fun(Buff, HitData0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_calc_damage, HitData0})
				end, HitData, Buffs),
	{reply, HitData1, State};


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


%% обработка "Разгадать тактику"
handle_cast({unravel, FromUnit}, State) ->
	?DBG("Start unravel from ~p~n", [FromUnit]),
	%% обработка баффов, защищающих от разгадывания
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	Protected = lists:any(fun(Buff) ->
						gen_event:call(State#state.event_mgr, Buff, is_unravel_protected)
				end, Buffs),
	case Protected of
		true  ->
			%% обработка баффов, перенаправляющих урон
			{ToUnit, _FromUnit} =
				lists:foldl(fun(Buff, Data0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_unravel, Data0})
				end, {State#state.unit, FromUnit}, Buffs),
			%% если ничего не изменилось, то ничего не делаем
			%% иначе снимаем с другого юнита
			case ToUnit =:= State#state.unit of
				true  -> ok;
				false ->
					buff_mgr:unravel(unit:get_id(ToUnit), State#state.unit)
			end;
		false ->
			%% снимаем все приемы
			do_unravel(State)
	end,
	{noreply, State};


%% обработка "Ставки на опережение"
handle_cast({steal, FromUnit}, State) when FromUnit =/= State#state.unit ->
	?DBG("Start steal from ~p~n", [FromUnit]),
	%% обработка баффов, защищающих от ставки
	Buffs = gen_event:which_handlers(State#state.event_mgr),
	Protected = lists:any(fun(Buff) ->
						gen_event:call(State#state.event_mgr, Buff, is_steal_protected)
				end, Buffs),
	case Protected of
		true  ->
			%% обработка баффов, перенаправляющих урон
			{ToUnit, _FromUnit} =
				lists:foldl(fun(Buff, Data0) ->
						gen_event:call(State#state.event_mgr, Buff, {on_before_unravel, Data0})
				end, {State#state.unit, FromUnit}, Buffs),
			%% если ничего не изменилось, то ничего не делаем
			%% иначе снимаем с другого юнита
			case ToUnit =:= State#state.unit of
				true  -> ok;
				false ->
					buff_mgr:steal(unit:get_id(ToUnit), FromUnit)
			end;
		false ->
			%% воруем все приемы
			do_steal(State, FromUnit)
	end,
	{noreply, State};

handle_cast({steal, FromUnit}, State) when FromUnit =:= State#state.unit ->
	?DBG("Try steal yourself ~p~n", [FromUnit]),
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
	gen_event:notify(State#state.event_mgr, Msg),
	{noreply, State};


%% messages from buffs
handle_info({gen_event_EXIT, {gen_buff, Id}, Reason}, State) ->
	drop_buff_from_list(self(), Id),
	case Reason of
		{swapped, {gen_buff, NewId}, _} ->
			add_buff_to_list(self(), NewId);
		_ -> ok
	end,
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
	Options = [{time, Buff#u_buff.time},
			   {value, Buff#u_buff.value},
			   {level, Buff#u_buff.level},
			   {exists, true}],
	apply_buff(Ev, UnitPid, Buff#u_buff.id, Options).


%% запуск нового баффа
apply_buff(Ev, UnitPid, BuffId, Options) when is_pid(Ev),
											  is_list(Options) ->
	start_buff(Ev, BuffId, UnitPid, Options).

apply_buff(Ev, UnitPid, Buff) when is_pid(Ev),
								   is_record(Buff, buff) ->
	Options = [{owner, Buff#buff.owner},
			   {time, Buff#buff.time},
			   {value, Buff#buff.value},
			   {level, Buff#buff.level},
			   {charges, Buff#buff.charges},
			   {state, Buff#buff.state}],
	start_buff(Ev, Buff#buff.id, UnitPid, Options).


start_buff(Ev, BuffId, UnitPid, BuffOptions) ->
	case gen_buff:start_link(Ev, BuffId, UnitPid, BuffOptions) of
		ok ->
			%% сохраняем ID баффа в ETS
			Owner = proplists:get_value(owner, BuffOptions, UnitPid),
			Id = {BuffId, UnitPid, Owner},
			add_buff_to_list(self(), Id);
		Err -> Err
	end.


%% get_list/1
%% ====================================================================
%% Возвращает список запущенных баффов в данном менеджере
get_buffs_list(Pid) ->
	case reg:get({buff_list, Pid}, []) of
		 L when is_list(L) -> L;
		 _ -> []
	end.

%% set_list/2
%% ====================================================================
%% Сохраняет список запущенных баффов в данном менеджере
set_buffs_list(Pid, List) ->
	reg:unbind({buff_list, Pid}),
	reg:set({buff_list, Pid}, List).

%% add_buff_to_list/2
%% ====================================================================
%% Добавляет бафф в список
add_buff_to_list(Pid, Id) ->
	List = get_buffs_list(Pid),
	case lists:member(Id, List) of
		true -> ok;
		_ -> set_buffs_list(Pid, List ++ [Id])
	end.

%% add_buff_to_list/2
%% ====================================================================
%% Удаляет бафф из списка
drop_buff_from_list(Pid, Id) ->
	List = get_buffs_list(Pid),
	set_buffs_list(Pid, lists:delete(Id, List)).


%% find_buff/2
%% ====================================================================
%% Поиск существующего баффа
find_buff(Pid, Ev, Id) when is_tuple(Id) ->
	List = get_buffs_list(Pid),
	case lists:member(Id, List) of
		true  -> get_buff_state(Ev, Id);
		false -> false
	end;

find_buff(Pid, Ev, Id) when is_atom(Id) ->
	List = get_buffs_list(Pid),
	case lists:keyfind(Id, 1, List) of
		false -> false;
		BuffId -> get_buff_state(Ev, BuffId)
	end.

get_buff_state(Ev, Id) ->
	case gen_event:call(Ev, {gen_buff, Id}, get_buff) of
		Buff when is_record(Buff, buff) -> Buff;
		_ -> false
	end.


%% do_unravel/1
%% ====================================================================
%% разгадать тактику
do_unravel(State) ->
	do_unravel_buff(gen_event:which_handlers(State#state.event_mgr), State).

do_unravel_buff([], _) ->
	ok;

do_unravel_buff([BuffId | TailBuffs], State) ->
	Buff = gen_event:call(State#state.event_mgr, BuffId, get_buff),
	case Buff#buff.type of
		trick ->
			gen_event:delete_handler(State#state.event_mgr, BuffId, remove_handler);
		_ -> ok
	end,
	do_unravel_buff(TailBuffs, State).


%% do_unravel/1
%% ====================================================================
%% воровство всех приемов
do_steal(State, Unit) ->
	UnitId = unit:get_id(Unit),
	do_steal_buff(gen_event:which_handlers(State#state.event_mgr), State, UnitId).

do_steal_buff([], _, _) ->
	ok;

do_steal_buff([BuffId | TailBuffs], State, UnitId) ->
	Buff = gen_event:call(State#state.event_mgr, BuffId, get_buff),
	case Buff#buff.type of
		trick ->
			gen_event:delete_handler(State#state.event_mgr, BuffId, remove_handler),
			buff_mgr:apply(UnitId, Buff);
		_ -> ok
	end,
	do_steal_buff(TailBuffs, State, UnitId).
