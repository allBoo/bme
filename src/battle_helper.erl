%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Управление данными поединков
%%% ====================================================================

-module(battle_helper).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_teams/1, create_unit/2]).


create_teams(Teams) ->
	{BattleTeams, _} = lists:mapfoldl(fun create_team/2, 1, Teams),
	BattleTeams.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_team(Team, Index) ->
	%% считаем самого жирного по обвесу
	MaxCost = lists:foldl(fun(User, Max) ->
								case (User#user.dress)#u_dress.cost > Max of
									true ->
										(User#user.dress)#u_dress.cost;
									false ->
										Max
								end
						   end, 0, Team),
	%% создаем записи участников поединка
	Members = lists:map(fun(User) ->
								create_unit(User, Index)
						end, Team),
	{#b_team{id = Index, max_cost = MaxCost, units = Members, units_count = length(Members)}, Index + 1}.


create_unit(User, TeamId) when is_record(User, user)->
	#b_unit{id = User#user.id,
			name = User#user.name,
			ai = User#user.ai,
			team_id = TeamId,
			tactics = #b_tactics{changes = 3},
			user = User}.
