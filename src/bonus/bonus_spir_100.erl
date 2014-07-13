%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 100 духовности
%%% ====================================================================


-module(bonus_spir_100).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_spir_100 module~n", []),
	{ok, Buff#buff{
			id   = bonus_spir_100,
			name = <<"Путь Духа"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = []
		}}.


on_unit_state_change({'user.stats.spir', _}, Buff) ->
	Unit = unit:get_state(Buff#buff.unit),
	Spir = ?stats(?user(Unit))#u_stats.spir,
	case Spir of
		_100 when _100 >= 100 -> {ok, Buff};
		_75  when _75  >= 75  -> {swap, bonus_spir_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_spir_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_spir_25, Buff};
		_ -> {swap, bonus_spir_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
