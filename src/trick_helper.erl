%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Основные действия приемов
%%% ====================================================================


-module(trick_helper).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([heal/4]).


heal(Trick, Unit, DValue, Spirit) when is_record(DValue, d_value) ->
	Luck  = user_state:get(?puserpid(Unit), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	heal(Trick, Unit, Value, Spirit);

heal(Trick, Unit, Value, Spirit) when is_number(Value),
									  is_boolean(Spirit) ->
	unit:got_heal(Unit, #b_heal{value = Value,
								trick = Trick,
								sender = Unit,
								recipient = Unit,
								use_spirit = Spirit}).
