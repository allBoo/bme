%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Main include file
%%% ====================================================================
-include_lib("log.hrl").
-include_lib("error.hrl").
-include_lib("user.hrl").
-include_lib("battle.hrl").

%%% ====================================================================
%%% Helper macro for declaring children of supervisor
%%% ====================================================================
-define(BATTLE(Battle), {Battle#battle.id, {battle, start_link, [Battle]}, transient, 5000, worker, [battle]}).
-define(HITS_SUP(Battle), {hits_sup, {hits_sup, start_link, [Battle]}, transient, infinity, supervisor, [hits_sup]}).
-define(TEAM_SUP(Battle, Team), {Team#b_team.id, {team_sup, start_link, [Team#b_team{battle_id=Battle#battle.id}]}, transient, infinity, supervisor, [team_sup]}).
-define(TEAM(Team), {Team#b_team.id, {team, start_link, [Team]}, transient, 1000, worker, [team]}).
-define(UNIT_SUP(Team, Unit), {Unit#b_unit.id, {unit_sup, start_link, [Unit#b_unit{battle_id=Team#b_team.battle_id}]}, transient, infinity, supervisor, [unit_sup]}).
-define(UNIT(Unit), {Unit#b_unit.id, {unit, start_link, [Unit]}, transient, 1000, worker, [unit]}).

%%% ====================================================================
%%% Списки комнат
%%% ====================================================================

%% комнаты, в которых разрешены хаотические, групповые и прочие договорные поединки
-define(BATTLE_PLACES, [1, 2, 3, 4]).

%% комнаты, в которых разрешены нападения
-define(ATTACK_PLACES, [1, 2, 3, 4, 5, 6, 7]).
