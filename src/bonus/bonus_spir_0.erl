%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 0 духовности
%%% ====================================================================


-module(bonus_spir_0).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_spir_0 module~n", []),
	{ok, Buff#buff{
			id   = bonus_spir_0,
			name = <<"Духовность"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = []
		}}.


on_unit_state_change({'stats.spir', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Spir = user_state:get(UserPid, 'stats.spir'),
	case Spir of
		_100 when _100 >= 100 -> {swap, bonus_spir_100, Buff};
		_75  when _75  >= 75  -> {swap, bonus_spir_75, Buff};
		_50  when _50  >= 50  -> {swap, bonus_spir_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_spir_25, Buff};
		_ -> {ok, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
