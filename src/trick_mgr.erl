%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% менеджер приемов
%%% ====================================================================


-module(trick_mgr).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CAST(Unit, Cmd), case reg:find({trick_mgr, Unit}) of
							 undefined   -> ?ERROR_NOT_IN_BATTLE;
							 TrickMgrPid -> gen_server:cast(TrickMgrPid, Cmd)
						 end).
-define(CALL(Unit, Cmd), case reg:find({trick_mgr, Unit}) of
							 undefined   -> ?ERROR_NOT_IN_BATTLE;
							 TrickMgrPid -> gen_server:call(TrickMgrPid, Cmd)
						 end).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_ev/1,
		 start_mgr/1,
		 apply/3,
		 apply/2,
		 list/1,
		 shock/1,
		 lock/2,
		 unlock/2]).


%% start_mgr/1
%% ====================================================================
%% регистрирует процесс event-manager
start_ev(Unit) ->
	{ok, Pid} = gen_event:start_link(),
	reg:set({trick_ev, Unit#b_unit.id}, Pid),
	reg:set({trick_ev, unit:get_pid(Unit#b_unit.id)}, Pid),
	{ok, Pid}.


%% start_mgr/1
%% ====================================================================
%% регистрирует процесс менеджера приемов
start_mgr(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% apply/3
%% ====================================================================
%% использование приема
apply(Unit, Trick, Recipient) when is_atom(Trick),
								   is_pid(Recipient) ->
	?CALL(Unit, {apply, Trick, Recipient});

apply(Unit, Trick, RecipientId) when is_atom(Trick),
									 is_integer(RecipientId) ->
	case unit:get_pid(RecipientId) of
		undefined    -> ?ERROR_NOT_IN_BATTLE;
		RecipientPid -> ?CALL(Unit, {apply, Trick, RecipientPid})
	end.


apply(Unit, Trick) when is_atom(Trick) ->
	?CALL(Unit, {apply, Trick}).


%% list/1
%% ====================================================================
%% возвращает список приемов
list(Unit) ->
	?CALL(Unit, list).


%% shock/1
%% ====================================================================
%% шок
shock(Unit) ->
	?CAST(Unit, shock).


%% lock/2
%% ====================================================================
%% блокировка приема
lock(Unit, TrickId) when is_atom(TrickId) ->
	?CAST(Unit, {lock, TrickId}).


%% unlock/2
%% ====================================================================
%% разблокировка приема
unlock(Unit, TrickId) when is_atom(TrickId) ->
	?CAST(Unit, {unlock, TrickId}).


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
	case unit:get_pid(Unit#b_unit.id) of
		undefined -> {stop, unit_not_found};
		UnitPid   ->
			case reg:get({trick_ev, Unit#b_unit.id}) of
				undefined -> {stop, trick_event_mgr_not_found};
				TrickEv   ->
					ok = reg:name([{trick_mgr, Unit#b_unit.id},
								   {trick_mgr, UnitPid}]),
					reg:bind({unit, Unit#b_unit.id}),

					State = #state{unit = UnitPid, event_mgr = TrickEv},

					%% запускаем одетые приемы
					init_tricks(TrickEv, UnitPid, user_state:get(Unit#b_unit.id, 'tricks')),
					do_notify(recalc, State),

					{ok, State}
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

%% выполнение приема
handle_call({apply, Trick, RecipientPid}, _From, State) when is_pid(RecipientPid) ->
	R = apply_trick(State#state.event_mgr, Trick, RecipientPid),
	{reply, R, State};

handle_call({apply, Trick}, _From, State) ->
	R = apply_trick(State#state.event_mgr, Trick),
	{reply, R, State};


%% возвращает список одетых приемов
handle_call(list, _From, State) ->
	List = lists:map(fun(Id) ->
						gen_event:call(State#state.event_mgr, Id, get_state)
					 end, gen_event:which_handlers(State#state.event_mgr)),
	{reply, List, State};


%% unknown request
handle_call(_Request, _From, State) ->
	{reply, ?ERROR_UNDEFINED, State}.


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

%% шок
handle_cast(shock, State) ->
	{noreply, State};


%% блокировка приема
handle_cast({lock, TrickId}, State) ->
	case gen_event:call(State#state.event_mgr, {gen_trick, TrickId}, lock) of
		ok -> do_notify(TrickId, recalc, State);
		_  -> error
	end,
	{noreply, State};


%% блокировка приема
handle_cast({unlock, TrickId}, State) ->
	case gen_event:call(State#state.event_mgr, {gen_trick, TrickId}, unlock) of
		ok -> do_notify(recalc, State);
		_  -> error
	end,
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
handle_info({unit, swap_done}, State) ->
	do_notify(swap_done, State),
	{noreply, State};

handle_info({unit, {change_state, {Change, _}}}, State) ->
	%% реагируем только на определенные изменения
	ChangeStr = atom_to_list(Change),
	Update = case ChangeStr of
				 "tactics"       -> true;
				 "tactics." ++ _ -> true;
				 "stats"         -> true;
				 "stats." ++ _   -> true;
				 "vitality.mana" -> true;
				 _ -> false
			 end,
	if
		Update -> do_notify(unit_changed, State);
		true   -> ok
	end,

	{noreply, State};


%% unknown message
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

init_tricks(_, _, []) ->
	ok;

init_tricks(Ev, UnitPid, [Trick | Tail]) ->
	case gen_trick:start_link(Ev, Trick, UnitPid) of
		ok  -> ?DBG("Started trick ~p~n", [Trick]);
		Err -> ?DBG("Error while start trick ~p: ~p~n", [Trick, Err])
	end,
	init_tricks(Ev, UnitPid, Tail).


do_notify(Msg, State) ->
	Unit = unit:get(State#state.unit),
	User = user_state:get(Unit#b_unit.id),
	gen_event:notify(State#state.event_mgr, {Msg, Unit#b_unit{user = User}}).

do_notify(TrickId, Msg, State) ->
	Unit = unit:get(State#state.unit),
	User = user_state:get(Unit#b_unit.id),
	gen_event:call(State#state.event_mgr, {gen_trick, TrickId}, {Msg, Unit#b_unit{user = User}}).


apply_trick(Ev, Trick) ->
	Msg = apply,
	do_apply_trick(Ev, Trick, Msg).

apply_trick(Ev, Trick, RecipientPid) ->
	Msg = {apply, RecipientPid},
	do_apply_trick(Ev, Trick, Msg).


do_apply_trick(Ev, Trick, Msg) ->
	Id = {gen_trick, Trick},
	Result = gen_event:call(Ev, Id, Msg),

	case Result of
		ok ->
			%% если прием выполнился, рассылаем уведомления другим приемам
			gen_event:notify(Ev, {applied, Trick}),
			ok;
		Error when is_record(Error, error) -> Error;
		{error, bad_module} -> ?ERROR_TRICK_NOT_APPLICABLE;
		_ -> ?ERROR_UNDEFINED
	end.
