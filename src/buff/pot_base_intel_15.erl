%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех баффов
%%% ====================================================================


-module(pot_base_intel_15).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_start/1, on_hit_done/1, on_end/1]).


new(Buff) ->
	?DBG("Start pot_base_intel_15 module~n", []),
	{ok, Buff#buff{
			name = <<"Снадобье разума"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time)
		}}.


on_hit_done(Buff) -> {ok, Buff}.


on_start(Buff) ->
	unit:increase_state(Buff#buff.unit, [{'user.stats.intel', Buff#buff.value}]).


on_end(Buff) ->
	unit:reduce_state(Buff#buff.unit, [{'user.stats.intel', Buff#buff.value}]).


%% ====================================================================
%% Internal functions
%% ====================================================================
