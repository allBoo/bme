%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Танец ветра
%%% Вы увернетесь от следующего направленного в вас удара
%%% Вы не получите тактики за контрудар с этого уворота.
%%% ====================================================================


-module(counter_winddance).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_avoid_damage/2, on_unit_before_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = counter_winddance,
			name = <<"Танец ветра"/utf8>>,
			type = trick,
			time = infinity,
			charges = 1,
			value = [{'mfs.adodge', 100}]
		}}.


%% после уворота снимаем бафф
on_unit_avoid_damage(HitResult, _Buff) when is_record(HitResult, b_hit_result) ->
	%% убираем тактику контры, если она была
	{remove_handler, HitResult#b_hit_result{counter = false}};

on_unit_avoid_damage(HitResult, Buff) ->
	{ok, HitResult, Buff}.

%% если все-таки прошел удар (обреченка), то тоже снимаем бафф
on_unit_before_damage(HitResult, _Buff) when is_record(HitResult, b_hit_result) ->
	{remove_handler, HitResult};

%% реакция на весь прочий урон
on_unit_before_damage(OtherAttack, Buff) ->
	{ok, OtherAttack, Buff}.


%% ====================================================================
%% Internal functions
%% ====================================================================
