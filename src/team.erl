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
	case gproc:lookup_local_name({team, BattleId, TeamId}) of
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
	%% регистрируем имя сервера
	true = gproc:add_local_name({team, Team#b_team.battle_id, Team#b_team.id}),
	%% регистрируем тег с номером боя для получения broadcast сообщений
	true = gproc:add_local_property({team, Team#b_team.battle_id}, Team#b_team.id),
	%% список пидов запущенных юнитов
	StartedUnits = gproc:lookup_pids({p, l, {team_unit, Team#b_team.battle_id, Team#b_team.id}}),

	{ok, Team#b_team{alive_units = StartedUnits, alive_count = length(StartedUnits)}}.


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

%% авто-выбор противников в начале боя
handle_info(elect, Team) ->
	?DBG("Start autoelection process on team ~p~n", [Team#b_team.id]),
	%% получаем список команд-противников
	EnemyTeamsIds = battle:get_enemy_teams(Team#b_team.battle_id, Team#b_team.id),
	?DBG("Enemy teams ids ~p~n", [EnemyTeamsIds]),
	%% получаем список противников по всем тимам [[pid()] ... [pid()]]
	EnemyTeams = lists:map(fun(TeamId) -> team:get_alive_units(Team#b_team.battle_id, TeamId) end, EnemyTeamsIds),
	EnemyTeamsCount = length(EnemyTeams),
	?DBG("Enemy teams ~p~n", [EnemyTeams]),
	%% пройдем по всем юнитам в тиме и рандомно расставим им противников
%% 	lists:foreach(fun(UnitPid) ->
%% 							%% выбираем рандомную тиму
%% 							EnemyTeam = if
%% 											EnemyTeamsCount == 1 ->
%% 												lists:nth(EnemyTeams, 1);
%% 											true ->
%% 												lists:nth(EnemyTeams, random:uniform(EnemyTeamsCount))
%% 										end,
%% 							%% выбираем рандомного юнита из выбранной тимы
%% 							lists:nth(EnemyTeam, random:uniform(length(EnemyTeam)))
%% 						end, Team#b_team.alive_units),
	{noreply, Team};

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
