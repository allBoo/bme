%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Управление данными пользователей
%%% ====================================================================

-module(user_helper).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get/1, split_teams/3]).

get(UsersIds) when is_list(UsersIds) ->
	[example:get(Id) || Id <- UsersIds];

get(_) ->
	error.

split_teams(Users, TeamsCount, default) ->
	%% маппим список юзеров на стоимость обмундирования, сортируем и в цикле раскидываем по командам
	Sorted = [X||{_,X} <- lists:sort([ {(N#user.dress)#u_dress.cost, N} || N <- Users])],
	create_pick_teams(Sorted, TeamsCount);

split_teams(Users, TeamsCount, random) ->
	%% перемешиваем список юзеров и раскидываем по командам
	Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Users])],
	create_list_teams(Shuffled, TeamsCount).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_list_teams(Users, 1) ->
	[Users];

create_list_teams(Users, TeamsCount) ->
	TeamLength = length(Users) div TeamsCount,
	{Team, Tail} = lists:split(TeamLength, Users),
	[Team | create_list_teams(Tail, TeamsCount - 1)].


create_pick_teams(Users, 1) ->
	[Users];

create_pick_teams(Users, TeamsCount) ->
	{Map, _} = lists:mapfoldl(fun(X, Index) -> {{Index, X}, Index + 1} end, 0, Users),
	{Team, Tail} = lists:partition(fun({A, _X}) -> A rem TeamsCount == 0 end, Map),
	[lists:map(fun({_Index, X}) -> X end, Team) | create_pick_teams(lists:map(fun({_Index, X}) -> X end, Tail), TeamsCount - 1)].
