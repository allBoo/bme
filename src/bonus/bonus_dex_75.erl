%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Бонус за 75 выносливости
%%% ====================================================================


-module(bonus_dex_75).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_state_change/2]).


new(Buff) ->
	?DBG("Start bonus_dex_75 module~n", []),
	{ok, Buff#buff{
			id   = bonus_dex_75,
			name = <<"Стальное Тело"/utf8>>,
			type = bonus,
			time = infinity,
			charges = undefined,
			value = [{'vitality.maxhp', 300}]
		}}.


on_unit_state_change({'stats.dex', _}, Buff) ->
	UserPid = unit:get_user_pid(Buff#buff.unit),
	Dex = user_state:get(UserPid, 'stats.dex'),
	case Dex of
		_125 when _125 >= 125 -> {swap, bonus_dex_125, Buff};
		_100 when _100 >= 100 -> {swap, bonus_dex_100, Buff};
		_75  when _75  >= 75  -> {ok, Buff};
		_50  when _50  >= 50  -> {swap, bonus_dex_50, Buff};
		_25  when _25  >= 25  -> {swap, bonus_dex_25, Buff};
		_ -> {swap, bonus_dex_0, Buff}
	end;


on_unit_state_change(_, Buff) ->
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
