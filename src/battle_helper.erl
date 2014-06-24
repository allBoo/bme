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
-export([create_teams/1]).


create_teams(Teams) ->
	{BattleTeams, _} = lists:mapfoldl(fun create_team/2, 1, Teams),
	BattleTeams.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_team(Team, Index) ->
	MaxCost = lists:foldl(fun(User, Max) ->
								case (User#user.clother)#u_clother.cost > Max of
									true ->
										(User#user.clother)#u_clother.cost;
									false ->
										Max
								end
						   end, 0, Team),
	{#b_team{id = Index, max_cost = MaxCost, members = Team}, Index + 1}.
