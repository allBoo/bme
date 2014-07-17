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
-export([new/1]).


new(Buff) ->
	?DBG("Start pot_base_200_alldmg2_p1k module~n", []),
	{ok, Buff#buff{
			type = pot,
			name = <<"Снадобье каменной стойкости"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time),
			value = [{'dprotection', Buff#buff.value}]
		}}.


%% ====================================================================
%% Internal functions
%% ====================================================================
