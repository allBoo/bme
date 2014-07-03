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
-export([get/1,
		 split_teams/3,
		 get_weapons/1,
		 get_weapon_type/1,
		 is_magic_type/1,
		 is_natural_type/1,
		 get_dpower_index/1,
		 get_wpower_index/1]).

%% bool_to_int/1
%% ====================================================================
%% boolean to integer

%% get/1
%% ====================================================================
%%
get(UsersIds) when is_list(UsersIds) ->
	[example:get(Id) || Id <- UsersIds];

get(_) ->
	error.

%% split_teams/3
%% ====================================================================
%% разбиение юзеров по командам
split_teams(Users, TeamsCount, default) ->
	%% маппим список юзеров на стоимость обмундирования, сортируем и в цикле раскидываем по командам
	Sorted = [X||{_,X} <- lists:sort([ {(N#user.dress)#u_dress.cost, N} || N <- Users])],
	create_pick_teams(Sorted, TeamsCount);

split_teams(Users, TeamsCount, random) ->
	%% перемешиваем список юзеров и раскидываем по командам
	Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Users])],
	create_list_teams(Shuffled, TeamsCount).


%% get_weapons/1
%% ====================================================================
%% возвращает список оружия у юзера
get_weapons(User) ->
	[(User#user.damage)#u_damage.left] ++
		case (User#user.damage)#u_damage.right of
			Right when is_record(Right, u_weapon_damage) ->
				(User#user.damage)#u_damage.right;
			undefined -> []
		end.


%% get_weapon_type/1
%% ====================================================================
%% возвращает случайно выбранный тип удара оружием
get_weapon_type(Weapon) ->
	AvailableTypes = get_active_weapon_types(Weapon),
	Rnd = random:uniform(),
	{SelectedType, _, _} = list_helper:find_first(AvailableTypes,
												  fun({_Type, Min, Max}) ->
														  (Rnd >= Min) and (Rnd =< Max) end),
	SelectedType.

get_active_weapon_types(Types) ->
	AllAvailableTypes = [prick, chop, crush, cut, air, fire, water, earth, light, dark, gray],
	get_active_weapon_types(Types, AllAvailableTypes, length(AllAvailableTypes) + 1, 0.0).

get_active_weapon_types(_Types, _AvailableTypes, 1, _) ->
	[];

get_active_weapon_types(Types, AvailableTypes, Index, Acc) ->
	Value = element(Index, Types),
	R = case Value > 0 of
		true  ->
			Acc0 = Acc + Value,
			[{lists:nth(Index - 1, AvailableTypes), Acc, Value + Acc}];
		false -> Acc0 = Acc, []
	end,
	R ++ get_active_weapon_types(Types, AvailableTypes, Index - 1, Acc0).


%% is_magic_type/1
%% ====================================================================
%% проверка типа урона - стихийный или физический
is_magic_type(Type) ->
	lists:member(Type, [air, fire, water, earth, light, dark, gray]).

%% is_natural_type/1
%% ====================================================================
%% проверка типа урона - стихийный или физический
is_natural_type(Type) ->
	lists:member(Type, [prick, chop, crush, cut]).


%% get_dpower_index/1
%% ====================================================================
get_dpower_index(general) -> #u_dpower.general;
get_dpower_index(prick)   -> #u_dpower.prick;
get_dpower_index(chop)    -> #u_dpower.chop;
get_dpower_index(crush)   -> #u_dpower.crush;
get_dpower_index(cut)     -> #u_dpower.cut.

%% get_wpower_index/1
%% ====================================================================
get_wpower_index(general) -> #u_wpower.general;
get_wpower_index(air)     -> #u_wpower.air;
get_wpower_index(fire)    -> #u_wpower.fire;
get_wpower_index(water)   -> #u_wpower.water;
get_wpower_index(earth)   -> #u_wpower.earth;
get_wpower_index(light)   -> #u_wpower.light;
get_wpower_index(dark)    -> #u_wpower.dark;
get_wpower_index(gray)    -> #u_wpower.gray.


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
