%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Управление боем
%%% ====================================================================


-module(battle).
-behaviour(gen_server).
-include_lib("bme.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 get_enemy_teams/2,
		 get_timeout/1]).

%% start_link/1
%% ====================================================================
%% запуск сервера
start_link(Battle) when is_record(Battle, battle) ->
	gen_server:start_link(?MODULE, Battle, []).


%% get_enemy_teams/2
%% ====================================================================
%% возвращает список вражеских команд
-spec get_enemy_teams(BattleId :: non_neg_integer(), MyTeamId :: non_neg_integer()) -> [non_neg_integer()].
get_enemy_teams(BattleId, MyTeamId) ->
	case gproc:lookup_local_name({battle, BattleId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		BattlePid -> gen_server:call(BattlePid, {get_enemy_teams, MyTeamId})
	end.


%% get_timeout/1
%% ====================================================================
%% возвращает текущий таймаут боя
get_timeout(BattleId) when is_integer(BattleId) ->
	case gproc:lookup_local_name({battle, BattleId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		BattlePid -> get_timeout(BattlePid)
	end;

get_timeout(BattlePid) when is_pid(BattlePid) ->
	gen_server:call(BattlePid, get_timeout).


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
init(Battle) when is_record(Battle, battle) ->
	?DBG("Start battle server ~p~n", [Battle#battle.id]),
	%% регистрируем имя сервера
	true = gproc:add_local_name({battle, Battle#battle.id}),

	%% список пидов запущенных тим
	StartedTeams = gproc:lookup_pids({p, l, {team, Battle#battle.id}}),

	%% формируем списки противников и раздаем их юнитам
	create_opponents_list(StartedTeams),

	%% отправляем всем сообщение о начале боя
	gproc:send({p, l, {battle, Battle#battle.id}}, {battle_start, self()}),

	{ok, Battle#battle{alive_teams = StartedTeams}}.


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

%% возвращает список вражеских команд в бою
handle_call({get_enemy_teams, MyTeamId}, _, Battle) ->
	EnemyTeamsIds = lists:filtermap(fun(Team) ->
											case Team#b_team.id /= MyTeamId of
												true -> {true, Team#b_team.id};
												false -> false
											end
										end, Battle#battle.teams),
	{reply, EnemyTeamsIds, Battle};


%% возвращает текущий таймаут поединка
handle_call(get_timeout, _, Battle) ->
	{reply, Battle#battle.timeout, Battle};


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

create_opponents_list(StartedTeams) ->
	%% для каждой тимы противниками будут все остальные тимы
	lists:foreach(fun(TeamPid) ->
						OpponentTeams = lists:delete(TeamPid, StartedTeams),
						OpponentUnits = lists:foldl(fun(EnemyTeamPid, Acc) ->
															Acc ++ team:get_alive_units(EnemyTeamPid)
													end, [], OpponentTeams),
						%% получаем краткую инфу о юнитах
						%% крайне не оптимально при кол-ве команд > 2
						Opponents = lists:map(fun unit:create_opponent_info/1, OpponentUnits),
						%?DBG("For team ~p found opposition units ~p from teams ~p", [TeamPid, Opponents, OpponentTeams]),
						%% отправляем всем юнитам в тиме сообщение со списком оппонентов
						lists:foreach(fun(UnitPid) ->
											  unit:set_opponents(UnitPid, Opponents)
									  end, team:get_alive_units(TeamPid))
				  end, StartedTeams),
	ok.
