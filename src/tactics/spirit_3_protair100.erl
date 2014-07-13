%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Призрачная защита
%%% Уменьшает урон от Магии Воздуха в два раза на 3 хода
%%% ====================================================================


-module(spirit_3_protair100).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_got_damage/2]).


new(Buff) ->
	?DBG("Start spirit_3_protair100 module~n", []),
	{ok, Buff#buff{
			id   = spirit_3_protair100,
			name = <<"Призрачный воздух"/utf8>>,
			type = effect,
			time = infinity,
			charges = 3,
			value = []
		}}.


on_unit_got_damage(#b_hit_result{damage_type = air} = HitResult, Buff) ->
	Damage = round(HitResult#b_hit_result.damage / 2),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(_, Buff) ->
	{ok, Buff}.

%% ====================================================================
%% Internal functions
%% ====================================================================
