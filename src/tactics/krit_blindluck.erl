%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Слепая удача
%%% Следующий удар будет критическим
%%% Вы не получите тактики с этого крита.
%%% ====================================================================


-module(krit_blindluck).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_hit_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = krit_blindluck,
			name = <<"Слепая удача"/utf8>>,
			type = trick,
			time = infinity,
			charges = undefined,
			value = [{'mfs.acrit', 100}]
		}}.


%% после удара снимаем бафф
on_unit_hit_damage(HitResult, _Buff) when is_record(HitResult, b_hit_result) ->
	%% убираем тактику крита и удара, если она была
	{remove_handler, HitResult#b_hit_result{counter = false, hited = false}};

on_unit_hit_damage(HitResult, Buff) ->
	{ok, HitResult, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
