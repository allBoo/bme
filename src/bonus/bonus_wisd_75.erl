%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 75 мудрости
%%% ====================================================================


-module(bonus_wisd_75).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_wisd_75 module~n", []),
	{ok, Buff#buff{
			id   = bonus_wisd_75,
			name = <<"Сила Мудрости"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'vitality.maxmana', 200}]
		}}.


on_unit_state_change({'stats.wisd', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Wisd = user_state:get(UserPid, 'stats.wisd'),
	case Wisd of
		_125 when _125 >= 125 -> {swap, bonus_wisd_125, Buff};
		_100 when _100 >= 100 -> {swap, bonus_wisd_100, Buff};
		_75  when _75  >= 75  -> {ok, Buff};
		_50  when _50  >= 50  -> {swap, bonus_wisd_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_wisd_25, Buff};
		_ -> {swap, bonus_wisd_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
