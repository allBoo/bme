%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Вломить
%%% Следующий удар наносит на 4 ед. больше урона
%%% ====================================================================


-module(novice_hit).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_calc_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = novice_hit,
			name = <<"Вломить"/utf8>>,
			type = effect,
			time = infinity,
			charges = undefined,
			value = []
		}}.


on_unit_calc_damage(Damage, _Buff) when is_number(Damage) ->
	{remove_handler, Damage + 4};

on_unit_calc_damage(Damage, _Buff) ->
	{remove_handler, Damage}.

%% ====================================================================
%% Internal functions
%% ====================================================================
