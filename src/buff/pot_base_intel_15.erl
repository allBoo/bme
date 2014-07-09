%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% базовый модуль всех баффов
%%% ====================================================================


-module(pot_base_intel_15).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1]).


new(Buff) ->
	?DBG("Start pot_base_intel_15 module~n", []),
	{ok, Buff#buff{
			name = <<"Снадобье разума"/utf8>>,
			charges = gen_buff:calc_charges(Buff#buff.time)
		}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
