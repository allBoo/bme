%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% АПИ для участника поединка
%%% ====================================================================


-module(unit).
-behaviour(gen_server).
-include_lib("bme.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 create_opponent_info/1,
		 set_opponents/2,
		 hit/3]).

%% start_link/1
%% ====================================================================
%% регистрирует процесс нового участника боя
start_link(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% create_opponent_info/1
%% ====================================================================
%% возвращает краткую информацию о юните
create_opponent_info(UserId) when (is_list(UserId) or is_integer(UserId)) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> create_opponent_info(UserPid)
	end;

create_opponent_info(UserPid) when is_pid(UserPid) ->
	gen_server:call(UserPid, create_opponent_info).


%% set_opponents/2
%% ====================================================================
%% устанавливает юниту список оппонентов
set_opponents(UserId, OpponentsList) when (is_list(UserId) or is_integer(UserId)) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> set_opponents(UserPid, OpponentsList)
	end;

set_opponents(UserPid, OpponentsList) when is_pid(UserPid) ->
	gen_server:cast(UserPid, {set_opponents, OpponentsList}).

%% hit/3
%% ====================================================================
%% выставление удара противнику
hit(UserId, Hits, Block) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> gen_server:call(UserPid, {hit, Hits, Block})
	end.



%% ====================================================================
%% Behavioural functions
%% ====================================================================

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
init(Unit) when is_record(Unit, b_unit) ->
	?DBG("Start unit API server ~p~n", [Unit#b_unit.id]),
	%% регистрируем имя сервера
	true = gproc:add_local_name({unit, Unit#b_unit.battle_id, Unit#b_unit.team_id, Unit#b_unit.id}),
	true = gproc:add_local_name({unit, Unit#b_unit.id}),
	true = gproc:add_local_name({unit, Unit#b_unit.name}),

	%% регистрируем теги для получения broadcast сообщений
	true = gproc:add_local_property({team_unit, Unit#b_unit.battle_id, Unit#b_unit.team_id}, Unit#b_unit.id),
	true = gproc:add_local_property({battle_unit, Unit#b_unit.battle_id}, Unit#b_unit.id),
	true = gproc:add_local_property({battle, Unit#b_unit.battle_id}, Unit#b_unit.id),

	{ok, Unit}.


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

%% возвращает краткую информацию об юните
handle_call(create_opponent_info, _, Unit) ->
	{reply, #b_opponent{pid = self(),
                        team = Unit#b_unit.team_pid,
                        id   = Unit#b_unit.id,
                        name = Unit#b_unit.name,
                        level = ((Unit#b_unit.user)#user.info)#u_info.level,
                        align = ((Unit#b_unit.user)#user.info)#u_info.align,
                        klan  = ((Unit#b_unit.user)#user.info)#u_info.klan,
                        cost  = ((Unit#b_unit.user)#user.dress)#u_dress.cost
						}, Unit};

%% выставление удара
handle_call({hit, Hits, Block}, _, State) ->
	case State#b_unit.opponent of
		undefined -> {reply, ?ERROR_TOO_FAST, State};
		OpponentPid -> {reply, ?ERROR_UNCOMPLETED, State}
	end;

%% unknown
handle_call(_, _, State) ->
	{reply, ?ERROR_WRONG_CALL, State}.


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

%% устанавливает юниту список оппонентов
handle_cast({set_opponents, OpponentsList}, Unit) ->
	%% сравниваем стоимось комлектов и определяем серых в бою
	CalculatedOpponentsList = lists:map(fun(Opponent) ->
												Delta = Opponent#b_opponent.cost / ((Unit#b_unit.user)#user.dress)#u_dress.cost,
												Opponent#b_opponent{gray = Delta < 0.6}
										end, OpponentsList),
	?DBG("Unit ~p set opponents ~p~n", [self(), CalculatedOpponentsList]),
	{noreply, Unit#b_unit{opponents = CalculatedOpponentsList}};

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

%% уведомление о запуске тимы
handle_info({team_start, TeamPid}, Unit) ->
	{noreply, Unit#b_unit{team_pid = TeamPid}};

%% уведомление о запуске боя
handle_info({battle_start, BattlePid}, Unit) ->
	{noreply, Unit#b_unit{battle_pid = BattlePid}};

%% unknown
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
