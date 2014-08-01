%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Абсолютная защита
%%% До Вашего следующего размена ударами, все повреждения от оружия или магии по Вам сводятся к 1.
%%% ====================================================================


-module(block_absolute).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_before_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = block_absolute,
			name = <<"Абсолютная защита"/utf8>>,
			type = trick,
			time = infinity,
			charges = 1,
			value = []
		}}.


on_unit_before_damage(HitResult, Buff) when is_record(HitResult, b_hit_result) ->
	Damage = min(HitResult#b_hit_result.damage, 1),
	{ok, Buff, HitResult#b_hit_result{damage = Damage}};

on_unit_before_damage(HitResult, Buff) when is_record(HitResult, b_magic_attack) ->
	Damage = min(HitResult#b_magic_attack.damage, 1),
	{ok, Buff, HitResult#b_magic_attack{damage = Damage}};

on_unit_before_damage(HitResult, Buff) ->
	{ok, Buff, HitResult}.

%% ====================================================================
%% Internal functions
%% ====================================================================
