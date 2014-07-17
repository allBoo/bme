%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 125 интуиции
%%% ====================================================================


-module(bonus_int_125).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_int_125 module~n", []),
	{ok, Buff#buff{
			id   = bonus_int_125,
			name = <<"Предчувствие"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'dpower.crit', 25},
					 {'mfs.ucrit', 200},
					 {'mfs.acrit', 5}]
		}}.


on_unit_state_change({'stats.int', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Int = user_state:get(UserPid, 'stats.int'),
	case Int of
		_125 when _125 >= 125 -> {ok, Buff};
		_100 when _100 >= 100 -> {swap, bonus_int_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_int_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_int_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_int_25, Buff};
		_ -> {swap, bonus_int_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
