%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 125 силы
%%% ====================================================================


-module(bonus_str_125).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_str_125 module~n", []),
	{ok, Buff#buff{
			id   = bonus_str_125,
			name = <<"Чудовищная Сила"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'damage.base.n', 10},
					 {'dpower.general', 25},
					 {'vitality.maxhp', 50},
					 {'mfs.ucrit', 75},
					 {'mfs.udodge', 75}]
		}}.


on_unit_state_change({'stats.str', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Str = user_state:get(UserPid, 'stats.str'),
	case Str of
		_125 when _125 >= 125 -> {ok, Buff};
		_100 when _100 >= 100 -> {swap, bonus_str_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_str_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_str_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_str_25, Buff};
		_ -> {swap, bonus_str_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.



%% ====================================================================
%% Internal functions
%% ====================================================================
