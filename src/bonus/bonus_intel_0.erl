%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 0 интеллекта
%%% ====================================================================


-module(bonus_intel_0).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_intel_0 module~n", []),
	{ok, Buff#buff{
			id   = bonus_intel_0,
			name = <<"Разум"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = []
		}}.


on_unit_state_change({'stats.intel', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Intel = user_state:get(UserPid, 'stats.intel'),
	case Intel of
		_125 when _125 >= 125 -> {swap, bonus_intel_125, Buff};
		_100 when _100 >= 100 -> {swap, bonus_intel_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_intel_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_intel_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_intel_25, Buff};
		_ -> {ok, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
