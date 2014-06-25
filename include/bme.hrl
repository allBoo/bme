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
-define(TEAM_SUP(Team), {Team#b_team.id, {team_sup, start_link, [Team]}, transient, infinity, supervisor, [team_sup]}).


%%% ====================================================================
%%% Списки комнат
%%% ====================================================================

%% комнаты, в которых разрешены хаотические, групповые и прочие договорные поединки
-define(BATTLE_PLACES, [1, 2, 3, 4]).

%% комнаты, в которых разрешены нападения
-define(ATTACK_PLACES, [1, 2, 3, 4, 5, 6, 7]).
