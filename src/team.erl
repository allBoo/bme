%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Управление командой в бою
%%% ====================================================================


-module(team).
-behaviour(gen_server).
-include_lib("bme.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, get_alive_units/2, get_alive_units/1]).

%% start_link/1
%% ====================================================================
%% запуск сервера
start_link(Team) when is_record(Team, b_team) ->
	gen_server:start_link(?MODULE, Team, []).


%% get_alive_units/2
%% ====================================================================
%% Возвращает список pid() живых юнитов
get_alive_units(BattleId, TeamId) ->
	case reg:find({team, BattleId, TeamId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		TeamPid   -> get_alive_units(TeamPid)
	end.

get_alive_units(TeamPid) ->
	gen_server:call(TeamPid, {get_alive_units}).

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
init(Team) when is_record(Team, b_team) ->
	?DBG("Start team server ~p~n", [Team#b_team.id]),
	process_flag(trap_exit, true),
	%% регистрируем имя сервера
	ok = reg:name({team, Team#b_team.battle_id, Team#b_team.id}),

	%% регистрируем теги с номером боя для получения broadcast сообщений
	ok = reg:bind([{battle, Team#b_team.battle_id},
				   {team, Team#b_team.battle_id}]),

	%% список пидов запущенных юнитов
	StartedUnits = reg:binded({team_unit, Team#b_team.battle_id, Team#b_team.id}),

	%% уведомляем их о пиде тимы
	reg:broadcast({team_unit, Team#b_team.battle_id, Team#b_team.id}, team, {start, self()}),

	{ok, Team#b_team{alive_units = StartedUnits, alive_count = length(StartedUnits), units = StartedUnits}}.


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

%% возвращает список пидов живых юнитов
handle_call({get_alive_units}, _, Team) ->
	{reply, Team#b_team.alive_units, Team};

%% error call
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

%% уведомление о запуске боя
handle_info({battle, {start, BattlePid}}, Team) ->
	{noreply, Team#b_team{battle_pid = BattlePid}};


%% уведомление о убитом юните
handle_info({unit, {killed, UnitPid}}, Team) when Team#b_team.alive_count > 0 ->
	case lists:member(UnitPid, Team#b_team.alive_units) of
		true  -> {noreply, unit_killed(UnitPid, Team)};
		false -> {noreply, Team}
	end;


%% уведомление о завершении поединка
handle_info({battle, {finish, Result}} = Msg, Team) ->
	?DBG("Team ~p got battle_finish message", [self()]),
	%% перенаправляем всем своим юнитам это сообщение
	[UnitPid ! Msg || UnitPid <- Team#b_team.units],
	{noreply, Team};


%% в любой непонятной ситтуации сохраняемся
handle_info({'EXIT', FromPid, Reason}, Team) ->
	?DBG("Team recieve exit signal ~p~n", [{FromPid, Reason}]),
	{noreply, Team};


%% unknown request
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
terminate(Reason, Team) ->
	?DBG("Team ~p terminates with reason ~p~n", [Team#b_team.id, Reason]),
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

%% unit_killed/2
%% ====================================================================
%% обработка уведомлений об убитых юнитах
unit_killed(UnitPid, Team) ->
	%% ?DBG("Team ~p check killed unit~n", [Team#b_team.id]),
	%% убираем его из списка живых юнитов
	AliveUnits = lists:delete(UnitPid, Team#b_team.alive_units),
	Team0 = Team#b_team{alive_units = AliveUnits,
						alive_count = length(AliveUnits)},

	%% если не осталось живых юнитов - останавливаем тиму
	case Team0#b_team.alive_count > 0 of
		true  -> Team0;
		false -> team_lost(Team0)
	end.


%% team_lost/1
%% ====================================================================
%% обработка проигрыша команды (все юниты убиты)
team_lost(Team) ->
	?DBG("Team ~p lost!~n", [Team#b_team.id]),
	battle:team_lost(Team#b_team.battle_pid, self()),
	Team.
