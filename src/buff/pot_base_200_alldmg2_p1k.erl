%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Элексир Снадобье каменной стойкости
%%% ====================================================================


-module(pot_base_200_alldmg2_p1k).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_start/1, on_end/1]).


new(Buff) ->
	?DBG("Start pot_base_200_alldmg2_p1k module~n", []),
	{ok, Buff#buff{
			type = pot,
			name = <<"Снадобье каменной стойкости"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time)
		}}.


on_start(Buff) ->
	unit:increase_state(Buff#buff.unit, [{'user.dprotection', Buff#buff.value}]),
	{ok, Buff}.


on_end(Buff) ->
	unit:reduce_state(Buff#buff.unit, [{'user.dprotection', Buff#buff.value}]),
	{ok, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
