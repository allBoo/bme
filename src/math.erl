%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Математические функции
%%% ====================================================================

-module(math).

%% ====================================================================
%% API functions
%% ====================================================================
-export([precision/2,
		 limit/2,
		 limit/3,
		 bool_to_int/1]).

%% precision/2
%% ====================================================================
%% ограничение точности floating-point
precision(Float, 0) ->
	trunc(Float);

precision(Float, 1) ->
	(trunc(Float * 10)) / 10;

precision(Float, 2) ->
	(trunc(Float * 100)) / 100.


%% limit/2
%% ====================================================================
%% ограничение максимального и минимального значения
limit(Value, Max) ->
	limit(Value, 0, Max).

limit(Value, Min, Max) ->
	case Value of
		Smaller when Smaller < Min -> Min;
		Bigger  when Bigger > Max  -> Max;
		Normal -> Normal
	end.


%% bool_to_int/1
%% ====================================================================
%% boolean to integer
bool_to_int(Bool) when Bool == true ->
	1;

bool_to_int(Bool) when Bool == false ->
	0.

%% ====================================================================
%% Internal functions
%% ====================================================================


