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
-export([start_ev/1, start_mgr/1, notify/2]).


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


notify(Unit, Event) ->
	?CAST(Unit, {notify, Event}).

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
					lists:foreach(fun(Buff) -> apply_exists(BuffEv, UnitPid, Buff) end, ?user(Unit)#user.buffs),
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
	gen_event:notify(State#state.event_mgr, Msg),
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
%% запуск процесса существующего баффа
apply_exists(Ev, UnitPid, Buff) when is_pid(Ev),
									 is_record(Buff, u_buff) ->
	?DBG("Start buff ~p~n", [{Ev, UnitPid, Buff}]),
	gen_buff:start_link(Ev, Buff#u_buff.id, UnitPid, [{time, Buff#u_buff.time}, {value, Buff#u_buff.value}, exists]).
