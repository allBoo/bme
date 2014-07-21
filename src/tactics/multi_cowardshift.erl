%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Коварный уход
%%% Следующий удар противника наносится по нему, вместо вас.
%%% ====================================================================


-module(multi_cowardshift).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_before_defend/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = multi_cowardshift,
			name = <<"Коварный уход"/utf8>>,
			type = effect,
			time = infinity,
			charges = 1,
			value = []
		}}.


on_unit_before_defend(HitQueue, _Buff) ->
	{remove_handler, HitQueue#b_hit_queue{defendant_pid = HitQueue#b_hit_queue.attacker_pid}}.


%% ====================================================================
%% Internal functions
%% ====================================================================
