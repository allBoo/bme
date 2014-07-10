%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех баффов
%%% ====================================================================


-module(spell_powerHPup5).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_start/1, on_end/1]).


new(Buff) ->
	{ok, Buff#buff{
			name = <<"Жажда жизни +5"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time),
			value = case Buff#buff.value of undefined -> get_value(Buff); Val -> Val end
		}}.


on_start(Buff) ->
	unit:increase_state(Buff#buff.unit, [{'user.vitality.maxhp', Buff#buff.value}]).


on_end(Buff) ->
	unit:reduce_state(Buff#buff.unit, [{'user.vitality.maxhp', Buff#buff.value}]).


%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Buff) ->
	Unit = unit:get_state(Buff#buff.unit),
	?stats(?user(Unit))#u_stats.dex * 5.

