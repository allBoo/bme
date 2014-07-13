%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 50 силы
%%% ====================================================================


-module(bonus_str_50).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_str_50 module~n", []),
	{ok, Buff#buff{
			id   = bonus_str_50,
			name = <<"Чудовищная Сила"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'user.dpower.general', 10}]
		}}.


on_unit_state_change({'user.stats.str', _}, Buff) ->
	Unit = unit:get_state(Buff#buff.unit),
	Str = ?stats(?user(Unit))#u_stats.str,
	case Str of
		_125 when _125 >= 125 -> {swap, bonus_str_125, Buff};
		_100 when _100 >= 100 -> {swap, bonus_str_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_str_75, Buff};
		_50  when _50  >= 50  -> {ok, Buff};
		_25  when _25  >= 25  -> {swap, bonus_str_25, Buff};
		_ -> {swap, bonus_str_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
