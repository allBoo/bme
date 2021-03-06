%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех баффов
%%% ====================================================================


-module(spell_powerHPup5).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1]).


new(Buff) ->
	HPValue = case Buff#buff.value of undefined -> get_value(Buff); Val -> Val end,
	{ok, Buff#buff{
			type = effect,
			name = <<"Жажда жизни +5"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time),
			value = [{'vitality.maxhp', HPValue}]
		}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_value(Buff) ->
	user_state:get(unit:get_user_pid(Buff#buff.unit), 'stats.dex') * 5.
