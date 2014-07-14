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


-module(spirit_block25).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_got_damage/2]).


new(Buff) ->
	?DBG("Start spirit_block25 module~n", []),
	{ok, Buff#buff{
			id   = spirit_block25,
			name = <<"Призрачная защита"/utf8>>,
			type = effect,
			time = infinity,
			charges = 1,
			value = []
		}}.


on_unit_got_damage(HitResult, Buff) when is_record(HitResult, b_hit_result) ->
	Damage = round(HitResult#b_hit_result.damage / 4),
	{ok, HitResult#b_hit_result{damage = Damage}, Buff};

on_unit_got_damage(HitResult, Buff) when is_record(HitResult, b_magic_attack) ->
	Damage = round(HitResult#b_magic_attack.damage / 4),
	{ok, HitResult#b_magic_attack{damage = Damage}, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
