%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 25 ловкости
%%% ====================================================================


-module(bonus_agil_25).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_agil_25 module~n", []),
	{ok, Buff#buff{
			id   = bonus_agil_25,
			name = <<"Скорость Молнии"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'user.mfs.udodge', 50}]
		}}.


on_unit_state_change({'user.stats.agil', _}, Buff) ->
	Unit = unit:get_state(Buff#buff.unit),
	Agil = ?stats(?user(Unit))#u_stats.agil,
	case Agil of
		_125 when _125 >= 125 -> {swap, bonus_agil_125, Buff};
		_100 when _100 >= 100 -> {swap, bonus_agil_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_agil_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_agil_50, Buff};
		_25  when _25  >= 25  -> {ok, Buff};
		_ -> {swap, bonus_agil_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
