%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Описания приемов
%%% ====================================================================


-module(tricks).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).


novice_hit() ->
	#trick{
		id             = novice_hit,
		name           = <<"Вломить"/utf8>>,
		decription     = <<"Следующий удар наносит на 4 ед. больше урона"/utf8>>,
		class          = novice,
		type           = trick,
		delay          = 4,
		class_delay    = true,
		self_buff      = novice_hit
	}.

novice_def() ->
	#trick{
		id             = novice_def,
		name           = <<"Прикрыться"/utf8>>,
		decription     = <<"Следующий удар по вам нанесет на 3 ед. меньше урона"/utf8>>,
		class          = novice,
		type           = trick,
		delay          = 3,
		class_delay    = true,
		self_buff      = novice_def
	}.

novice_hp() ->
	#trick{
		id             = novice_hp,
		name           = <<"Собрать Зубы"/utf8>>,
		decription     = <<"Вы восстанавливаете от 2-5 HP"/utf8>>,
		class          = novice,
		type           = trick,
		tactics        = #b_tactics{spirit = 1},
		delay          = 4,
		class_delay    = true,
		action         = fun(UnitPid) ->
								 trick_helper:heal(novice_hp, UnitPid, #d_value{n = 1, k = 4}, false)
						 end
	}.

