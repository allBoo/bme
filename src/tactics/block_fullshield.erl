%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Полная защита
%%% Следующий удар или заклятие нанесет вам не более 1 повреждения
%%% ====================================================================


-module(block_fullshield).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_got_damage/2]).


new(Buff) ->
	?DBG("Start block_fullshield module~n", []),
	{ok, Buff#buff{
			id   = block_fullshield,
			name = <<"Полная защита"/utf8>>,
			type = effect,
			time = infinity,
			charges = undefined,
			value = []
		}}.


on_unit_got_damage(HitResult, _Buff) ->
	Damage = min(HitResult#b_hit_result.damage, 1),
	{remove_handler, HitResult#b_hit_result{damage = Damage}}.


%% ====================================================================
%% Internal functions
%% ====================================================================
