%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Хлебнуть Крови
%%% Ваш следующий критический удар и два удара за ним лечат вас частью нанесенного урона.
%%% Но не более чем ?? HP с противника ?? уровня. Выпитая кровь придает вам силы.
%%% ====================================================================


-module(krit_blooddrink).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_hit_damage/2]).

-record(state, {started = false, hits = 0}).

new(Buff) ->
	?DBG("Start krit_blooddrink module~n", []),
	{ok, Buff#buff{
			id   = krit_blooddrink,
			name = <<"Хлебнуть Крови"/utf8>>,
			type = effect,
			time = infinity,
			charges = undefined,
			value = [],
			state = #state{}
		}}.

%% реакция на нанесенный урон оружием

%% ждем первый критический удар
on_unit_hit_damage(HitResult, Buff) when is_record(HitResult, b_hit_result),
										 (Buff#buff.state)#state.started == false,
										 HitResult#b_hit_result.crit == true ->
	%% увеличиваем кол-во силы
	Value = [{'user.stats.str', get_str(HitResult#b_hit_result.damage, ?plevel(HitResult#b_hit_result.defendant))}],
	unit:increase_state(Buff#buff.unit, Value),
	%% хиляем юнита на половину урона, но не больше лимита
	do_heal(HitResult, Buff),

	%% расчет кол-ва ударов для которых действует эффект = 2 * кол-во точек удара
	Hits = 2 * user_state:get(?puserpid(HitResult#b_hit_result.attacker), 'battle_spec.hit_points'),
	{ok, HitResult, Buff#buff{value = Value, state = #state{started = true, hits = Hits}}};

%% следующие после критического
on_unit_hit_damage(HitResult, Buff) when is_record(HitResult, b_hit_result),
										 (Buff#buff.state)#state.started == true ->
	%% хиляем юнита на половину урона, но не больше лимита
	do_heal(HitResult, Buff),
	State = Buff#buff.state,
	Hits = State#state.hits - 1,
	if
		Hits > 0 ->
			{ok, HitResult, Buff#buff{state = State#state{hits = Hits}}};
		true ->
			{remove_handler, HitResult}
	end;

%% на все остальное не реагируем
on_unit_hit_damage(HitResult, Buff) ->
	{ok, HitResult, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_str(Damage, Level) ->
	D = math:limit((Damage / 2) / get_max_hp(Level), 1.0),
	round(get_max_str(Level) * D).

do_heal(HitResult, Buff) ->
	Damage = HitResult#b_hit_result.damage,
	MaxHP  = get_max_hp(?plevel(HitResult#b_hit_result.defendant)),
	HealValue = round(math:limit(Damage / 2, MaxHP)),
	Heal = #b_heal{
		value = HealValue,
		buff  = Buff,
		use_spirit = false,
		sender = HitResult#b_hit_result.attacker,
		recipient = HitResult#b_hit_result.attacker,
		transaction = HitResult#b_hit_result.transaction
	},
	unit:got_heal(Buff#buff.unit, Heal, HitResult#b_hit_result.transaction).


get_max_hp(11) -> 184;
get_max_hp(10) -> 154;
get_max_hp(9)  -> 128;
get_max_hp(8)  -> 107;
get_max_hp(7)  -> 77;
get_max_hp(6)  -> 55;
get_max_hp(_)  -> 0.

get_max_str(11) -> 16;
get_max_str(10) -> 15;
get_max_str(9)  -> 14;
get_max_str(8)  -> 13;
get_max_str(7)  -> 10;
get_max_str(6)  -> 8;
get_max_str(_)  -> 0.
