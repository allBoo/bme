%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Призрачная защита
%%% До вашего следующего размена ударами, вы получаете лишь четверть
%%% повреждений от оружия или магии
%%% ====================================================================


-module(wis_water_shield).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_got_damage/2]).


new(Buff) ->
	?DBG("Start wis_water_shield module~n", []),
	{ok, Buff#buff{
			id   = wis_water_shield,
			name = <<"Иней: обморожение"/utf8>>,
			type = effect,
			time = infinity,
			charges = 4,
			value = []
		}}.

%% реакция на урон оружием
on_unit_got_damage(HitResult, #buff{level = 7} = Buff) when is_record(HitResult, b_hit_result) ->
	%% сокращение урона на 25%
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * 0.25),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(HitResult, #buff{level = 8} = Buff) when is_record(HitResult, b_hit_result) ->
	%% сокращение урона на 25%
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * 0.25),
	%% @todo атакующий оружием получает уязвимость к магии воды
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(HitResult, #buff{level = 9} = Buff) when is_record(HitResult, b_hit_result) ->
	%% сокращение урона на 15%
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * 0.15),
	%% @todo атакующий оружием получает уязвимость к магии воды
	%% возвращает 15% урона противнику
	Riposte = #b_magic_attack{
		damage = round(Damage * 0.15),
		damage_type = water,
		buff = Buff
	},
	unit:magic_damage(HitResult#b_hit_result.attacker, Buff#buff.unit, Riposte, HitResult#b_hit_result.transaction),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(HitResult, #buff{level = 10} = Buff) when is_record(HitResult, b_hit_result) ->
	%% сокращение урона на 20%
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * 0.20),
	%% @todo атакующий оружием получает уязвимость к магии воды
	%% возвращает 20% урона противнику
	Riposte = #b_magic_attack{
		damage = round(Damage * 0.20),
		damage_type = water,
		buff = Buff
	},
	unit:magic_damage(HitResult#b_hit_result.attacker, Buff#buff.unit, Riposte, HitResult#b_hit_result.transaction),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(HitResult, #buff{level = 11} = Buff) when is_record(HitResult, b_hit_result) ->
	%% сокращение урона на 25%
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * 0.25),
	%% @todo атакующий оружием получает уязвимость к магии воды
	%% возвращает 25% урона противнику
	Riposte = #b_magic_attack{
		damage = round(Damage * 0.25),
		damage_type = water,
		buff = Buff
	},
	unit:magic_damage(HitResult#b_hit_result.attacker, Buff#buff.unit, Riposte, HitResult#b_hit_result.transaction),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};


%% реакция на урон магией
on_unit_got_damage(HitResult, Buff) when is_record(HitResult, b_magic_attack) ->
	%% сокращение урона на 25%
	Damage = round(HitResult#b_magic_attack.damage - HitResult#b_magic_attack.damage * 0.25),
	{ok, HitResult#b_magic_attack{damage = Damage}, Buff};


%% на все остальное не реагируем
on_unit_got_damage(HitResult, Buff) ->
	{ok, HitResult, Buff}.

%% ====================================================================
%% Internal functions
%% ====================================================================
