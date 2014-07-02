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
		 get_timeout/1,
		 team_lost/2]).

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


%% team_lost/2
%% ====================================================================
%% уведомление о проигравшей команде
team_lost(BattleId, TeamPid) when is_integer(BattleId),
								  is_pid(TeamPid) ->
	case gproc:lookup_local_name({battle, BattleId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		BattlePid -> team_lost(BattlePid, TeamPid)
	end;

team_lost(BattlePid, TeamPid) when is_pid(BattlePid),
								   is_pid(TeamPid) ->
	gen_server:cast(BattlePid, {team_lost, TeamPid}).

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
	process_flag(trap_exit, true),
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


%% unknown request
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

%% обработка уведомления о проигравшей команде
handle_cast({team_lost, TeamPid}, Battle) ->
	%% удаляем тиму из списка запущенных
	AliveTeams = lists:delete(TeamPid, Battle#battle.alive_teams),
	Battle0    = Battle#battle{alive_teams = AliveTeams},

	case length(AliveTeams) of
		%% если осталась только одна тима, вешаем таймаут на финальную
		%% проверку статуса и завершение поединка
		%% таймаут нужен, чтобы отработать возможность ничьей
		1 -> {noreply, Battle0, 500};
		%% если не осталось ни кого - значит ничья
		0 -> finish(Battle0, standoff);
		%% иначе продолжаем
		_ -> {noreply, Battle0}
	end;


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

%% вызывается при завершении поединка
handle_info(timeout, Battle) when length(Battle#battle.alive_teams) == 1 ->
	finish(Battle, lists:nth(1, Battle#battle.alive_teams));


%% в любой непонятной ситтуации сохраняемся
handle_info({'EXIT', FromPid, Reason}, Battle) ->
	?DBG("Battle recieve exit signal ~p~n", [{FromPid, Reason}]),
	{noreply, Battle};


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
terminate(Reason, Battle) ->
	?DBG("Battle ~p terminates with reason ~p~n", [Battle#battle.id, Reason]),
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


%% finish/2
%% ====================================================================
%% завершение поединка
%% ничья
finish(Battle, standoff) when is_record(Battle, battle) ->
	?DBG("Battle ~p finished with standoff result!!!", [Battle#battle.id]),
	Result = #b_result{winner = standoff},
	finish(Battle, Result);

%% выигрыш
finish(Battle, WinnerTeam) when is_record(Battle, battle),
								is_pid(WinnerTeam) ->
	?DBG("Battle ~p finished! Team ~p is winner!!!", [Battle#battle.id, WinnerTeam]),
	Result = #b_result{winner = WinnerTeam},
	finish(Battle, Result);

%% завершение
finish(Battle, Result) when is_record(Battle, battle),
							is_record(Result, b_result) ->
	%% расчет коэффициента экспы
	%% @todo переделать
	ExpCoef = 1.0
			  + (Battle#battle.status + 0.25)					%% +25% за каждый уровень поединка
			  + (1 * math:bool_to_int(Battle#battle.blood))		%% +100 за кровавый
			  + calc_battle_exp_coef(Battle),					%% + надбавка за тип боя
	Result0 = Result#b_result{exp_coef = ExpCoef},

	%% сохраняем статистику поединка
	%% отправляем всем тимам сообщение о завершении поединка
	gproc:send({p, l, {team, Battle#battle.id}}, {battle_finish, Result0}),

	%% завершаем все процессы поединка
	bme:finish_battle(Battle),
	{noreply, Battle}.


calc_battle_exp_coef(Battle) ->
	case Battle#battle.type of
		battle  -> 0.0;
		haot    -> 1.0;
		dungeon -> -0.8;
		tower   -> -1.0;
		klan    -> 1.0;
		_ -> 0
	end.
