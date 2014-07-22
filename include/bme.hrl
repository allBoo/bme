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
-include_lib("hit.hrl").
-include_lib("buff.hrl").
-include_lib("trick.hrl").
-include_lib("battle_log.hrl").

%%% ====================================================================
%%% Helper macro for declaring children of supervisor
%%% ====================================================================
-define(GSI(String, Integer), String ++ integer_to_list(Integer)).

-define(BATTLE(Battle), {?GSI("battle_", Battle#battle.id), {battle, start_link, [Battle]}, transient, 5000, worker, [battle]}).
-define(BATTLE_LOG(Battle), {?GSI("battle_log_", Battle#battle.id), {battle_log, start_link, [Battle#battle.id]}, transient, 5000, worker, [battle_log]}).
-define(HITS_SUP(Battle), {hits_sup, {hits_sup, start_link, [Battle]}, transient, infinity, supervisor, [hits_sup]}).
-define(TEAM_SUP(Battle, Team), {?GSI("teamsup_", Team#b_team.id), {team_sup, start_link, [Team#b_team{battle_id=Battle#battle.id}]}, transient, infinity, supervisor, [team_sup]}).
-define(TEAM(Team), {?GSI("team_", Team#b_team.id), {team, start_link, [Team]}, transient, 5000, worker, [team]}).
-define(UNIT_SUP(Team, Unit), {?GSI("unitsup_", Unit#b_unit.id), {unit_sup, start_link, [Unit#b_unit{battle_id=Team#b_team.battle_id}]}, transient, infinity, supervisor, [unit_sup]}).
-define(USER(User), {?GSI("user_", User#user.id), {user_state, start_link, [User]}, transient, 1000, worker, [user_state]}).
-define(UNIT(Unit), {?GSI("unit_", Unit#b_unit.id), {unit, start_link, [Unit]}, transient, 5000, worker, [unit]}).
-define(BUFF_SUP(Unit), {?GSI("buffsup_", Unit#b_unit.id), {buff_sup, start_link, [Unit]}, transient, infinity, supervisor, [buff_sup]}).
-define(TRICK_SUP(Unit), {?GSI("tricksup_", Unit#b_unit.id), {trick_sup, start_link, [Unit]}, transient, infinity, supervisor, [trick_sup]}).
-define(AI0(Unit), {?GSI("ai0_", Unit#b_unit.id), {ai0, start_link, [Unit]}, transient, 1000, worker, [ai0]}).

%%% ====================================================================
%%% Списки комнат
%%% ====================================================================

%% комнаты, в которых разрешены хаотические, групповые и прочие договорные поединки
-define(BATTLE_PLACES, [1, 2, 3, 4]).

%% комнаты, в которых разрешены нападения
-define(ATTACK_PLACES, [1, 2, 3, 4, 5, 6, 7]).
