%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Разгадать тактику
%%% Отменяет все приемы на противнике при размене ударами.
%%% ====================================================================


-module(multi_resolvetactic).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_before_hit/2, on_unit_before_defend/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = multi_resolvetactic,
			name = <<"Разгадать тактику"/utf8>>,
			type = trick,
			time = infinity,
			charges = 1,
			value = []
		}}.


on_unit_before_hit(HitQueue, _Buff) ->
	do(HitQueue#b_hit_queue.defendant_pid, HitQueue#b_hit_queue.attacker_pid),
	{remove_handler, HitQueue}.

on_unit_before_defend(HitQueue, _Buff) ->
	do(HitQueue#b_hit_queue.attacker_pid, HitQueue#b_hit_queue.defendant_pid),
	{remove_handler, HitQueue}.


%% ====================================================================
%% Internal functions
%% ====================================================================

do(Whom, From) ->
	buff_mgr:unravel(unit:get_id(Whom), From).
