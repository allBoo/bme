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
		 get_dpower/2,
		 get_wpower/2,
		 get_skill/2,
		 get_zone_armor/2,
		 get_zone_protection/3,
		 get_wprotection/2]).

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
			Right when is_record(Right, u_weapon) ->
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


%% get_dpower/2
%% ====================================================================
%% возвращает мф мощности заданного типа урона
get_dpower(Type, User) ->
	element(get_dpower_index(Type), User#user.dpower).

%% get_dpower/2
%% ====================================================================
%% возвращает мф мощности заданного типа магии
get_wpower(Type, User) ->
	element(get_wpower_index(Type), User#user.wpower).


%% get_skill/2
%% ====================================================================
%% возвращает умение владения заданным типом оружия
get_skill(SkillType, User) ->
	element(get_skill_index(SkillType), User#user.skills).


%% get_zone_armor/2
%% ====================================================================
%% возвращает значение брони в заданной зоне удара
get_zone_armor(head, User)   -> (User#user.armor)#u_armor.head;
get_zone_armor(torso, User)  -> (User#user.armor)#u_armor.torso;
get_zone_armor(paunch, User) -> (User#user.armor)#u_armor.torso;
get_zone_armor(belt, User)   -> (User#user.armor)#u_armor.belt;
get_zone_armor(legs, User)   -> (User#user.armor)#u_armor.legs.


%% get_zone_protection/3
%% ====================================================================
%% возвращает значение защиты от заданного урона в заданной зоне удара
get_zone_protection(head, DamageType, User) ->
	element(get_dprotection_index(DamageType), (User#user.dprotection)#u_dprotection.head);
get_zone_protection(torso, DamageType, User) ->
	element(get_dprotection_index(DamageType), (User#user.dprotection)#u_dprotection.torso);
get_zone_protection(paunch, DamageType, User) ->
	element(get_dprotection_index(DamageType), (User#user.dprotection)#u_dprotection.torso);
get_zone_protection(belt, DamageType, User) ->
	element(get_dprotection_index(DamageType), (User#user.dprotection)#u_dprotection.belt);
get_zone_protection(legs, DamageType, User) ->
	element(get_dprotection_index(DamageType), (User#user.dprotection)#u_dprotection.legs).


%% get_wprotection/2
%% ====================================================================
%% возвращает значение защиты от магии заданного типа
get_wprotection(air, User)   -> (User#user.wprotection)#u_wprotection.air;
get_wprotection(fire, User)  -> (User#user.wprotection)#u_wprotection.fire;
get_wprotection(water, User) -> (User#user.wprotection)#u_wprotection.water;
get_wprotection(earth, User) -> (User#user.wprotection)#u_wprotection.earth;
get_wprotection(light, User) -> (User#user.wprotection)#u_wprotection.light;
get_wprotection(dark, User)  -> (User#user.wprotection)#u_wprotection.dark;
get_wprotection(gray, User)  -> (User#user.wprotection)#u_wprotection.gray;
get_wprotection(_, User)     -> (User#user.wprotection)#u_wprotection.general.


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

%% get_skill_index/1
%% ====================================================================
get_skill_index(knife)  -> #u_skills.knife;
get_skill_index(axe)    -> #u_skills.axe;
get_skill_index(hammer) -> #u_skills.hammer;
get_skill_index(sword)  -> #u_skills.sword;
get_skill_index(staff)  -> #u_skills.staff;
get_skill_index(fire)   -> #u_skills.fire;
get_skill_index(air)    -> #u_skills.air;
get_skill_index(water)  -> #u_skills.water;
get_skill_index(earth)  -> #u_skills.earth;
get_skill_index(light)  -> #u_skills.light;
get_skill_index(dark)   -> #u_skills.dark;
get_skill_index(gray)   -> #u_skills.gray.

%% get_dprotection_index/1
%% ====================================================================
get_dprotection_index(general) -> #u_dprotection_zone.general;
get_dprotection_index(prick)   -> #u_dprotection_zone.prick;
get_dprotection_index(chop)    -> #u_dprotection_zone.chop;
get_dprotection_index(crush)   -> #u_dprotection_zone.crush;
get_dprotection_index(cut)     -> #u_dprotection_zone.cut.
