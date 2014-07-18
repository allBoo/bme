%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Дикая удача
%%% Следующий критический удар наносит максимальные повреждения.
%%% ====================================================================


-module(krit_wildluck).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_before_calc_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = krit_blindluck,
			name = <<"Дикая удача"/utf8>>,
			type = effect,
			time = infinity,
			charges = undefined,
			value = []
		}}.


%% перед критическим ударом увеличиваем базовый минимальный урон и снимаем бафф
on_unit_before_calc_damage({HitResult, Attacker, AttackerWeapon, Defendant}, _Buff) when is_record(HitResult, b_hit_result),
																						 HitResult#b_hit_result.crit == true ->
	%% базовый урон юзера
	D = Attacker#user.damage,
	BaseDamage = set_max_damage(D#u_damage.base),
	%% урон оружием
	WeaponDamage = set_max_damage(AttackerWeapon#u_weapon.damage),
	{remove_handler, {HitResult,
					  Attacker#user{damage = D#u_damage{base = BaseDamage}},
					  AttackerWeapon#u_weapon{damage = WeaponDamage},
					  Defendant}};

on_unit_before_calc_damage(HitData, Buff) ->
	{ok, HitData, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================

set_max_damage(DValue) ->
	#d_value{n = DValue#d_value.n + DValue#d_value.k, k = 0}.
