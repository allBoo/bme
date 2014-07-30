%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Стойкость
%%% Увеличивает вашу защиту от урона и магии на 5% до конца боя.
%%% Можно применять трижды
%%% ====================================================================


-module(hp_defence).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, renew/2, on_unit_before_damage/2]).

-record(state, {uses = 0, reduce = 0}).

new(Buff) ->
	{ok, Buff#buff{
			id   = hp_defence,
			name = <<"Стойкость"/utf8>>,
			type = trick,
			time = infinity,
			charges = undefined,
			value = [],
			state = #state{uses = 1, reduce = 0.05}
		}}.

renew(_, #buff{state = State} = Buff) when State#state.uses == 1 ->
	{ok, Buff#buff{
			state = State#state{uses = 2, reduce = 0.1}
		}};

renew(_, #buff{state = State} = Buff) when State#state.uses == 2 ->
	{ok, Buff#buff{
			lock = [hp_defence],
			state = State#state{uses = 3, reduce = 0.15}
		}};

renew(_, Buff) -> {ok, Buff}.

%% вообще это не совсем правильно, занижаем урон на 5-15%, реально нада увеличивать защиту
on_unit_before_damage(#b_hit_result{damage_type = air} = HitResult, #buff{state = State} = Buff) ->
	Damage = round(HitResult#b_hit_result.damage - HitResult#b_hit_result.damage * State#state.reduce),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_before_damage(#b_magic_attack{damage_type = air} = HitResult, #buff{state = State} = Buff) ->
	Damage = round(HitResult#b_magic_attack.damage - HitResult#b_magic_attack.damage * State#state.reduce),
	{ok, HitResult#b_magic_attack{damage = Damage}, Buff};

on_unit_before_damage(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================

