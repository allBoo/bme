%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Агрессивная Защита
%%% Следующий удар или заклятие противника нанесет не более 1 повреждения
%%% противник получает 3 * уровень ед. урона.
%%% защищает от приемов "Разгадать тактику" и "Ставка на Опережение"
%%% ====================================================================


-module(multi_agressiveshield).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1,
		 is_unravel_protected/1,
		 is_steal_protected/1,
		 on_unit_before_damage/2,
		 on_unit_after_damage/2]).


new(Buff) ->
	?DBG("Start multi_agressiveshield module~n", []),
	{ok, Buff#buff{
			id   = multi_agressiveshield,
			name = <<"Агрессивная Защита"/utf8>>,
			type = trick,
			time = infinity,
			charges = undefined,
			value = []
		}}.

%% защита от разгадайки
is_unravel_protected(Buff) ->
	%% если даже не получим урона, то прием снимаем
	{ok, true, Buff#buff{charges = 1}}.

%% защита от ставки
is_steal_protected(Buff) ->
	{ok, true, Buff#buff{charges = 1}}.

%% реакция на урон оружием
on_unit_before_damage(HitResult, Buff) when is_record(HitResult, b_hit_result) ->
	Damage = min(HitResult#b_hit_result.damage, 1),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};


%% реакция на урон магией
on_unit_before_damage(MagicAttack, Buff) when is_record(MagicAttack, b_magic_attack) ->
	Damage = min(MagicAttack#b_magic_attack.damage, 1),
	{ok, MagicAttack#b_magic_attack{damage = Damage}, Buff};


%% на все остальное не реагируем
on_unit_before_damage(HitResult, Buff) ->
	{ok, HitResult, Buff}.


%% реакция после полчения урона
on_unit_after_damage(HitResult, Buff) when is_record(HitResult, b_hit_result) ->
	Value = 3 * ?plevel(HitResult#b_hit_result.attacker),
	Riposte = #b_magic_attack{
		damage      = Value,
		damage_type = general,
		buff        = Buff,
		%% меняем местами получателя и атакующего
		attacker    = HitResult#b_hit_result.defendant,
		defendant   = HitResult#b_hit_result.attacker,
		transaction = HitResult#b_hit_result.transaction
	},
	unit:magic_damage(HitResult#b_hit_result.attacker, Riposte, HitResult#b_hit_result.transaction),
	{remove_handler, HitResult};


%% реакция после полчения урона
on_unit_after_damage(MagicAttack, Buff) when is_record(MagicAttack, b_magic_attack) ->
	Value = 3 * ?plevel(MagicAttack#b_magic_attack.attacker),
	Riposte = #b_magic_attack{
		damage      = Value,
		damage_type = general,
		buff        = Buff,
		%% меняем местами получателя и атакующего
		attacker    = MagicAttack#b_magic_attack.defendant,
		defendant   = MagicAttack#b_magic_attack.attacker,
		transaction = MagicAttack#b_magic_attack.transaction
	},
	unit:magic_damage(MagicAttack#b_magic_attack.attacker, Riposte, MagicAttack#b_magic_attack.transaction),
	{remove_handler, MagicAttack}.


%% ====================================================================
%% Internal functions
%% ====================================================================


