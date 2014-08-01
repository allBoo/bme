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
-export([heal/4, magic_hit/5, magic_hit_mass/6, magic_heal/5, magic_heal_mass/6]).


heal(Trick, UnitPid, DValue, Spirit) when is_record(DValue, d_value) ->
	Luck  = user_state:get(?puserpid(UnitPid), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	heal(Trick, UnitPid, Value, Spirit);

heal(Trick, UnitPid, Value, Spirit) when is_number(Value),
									  is_boolean(Spirit) ->
	unit:got_heal(UnitPid, #b_heal{value = Value,
								   trick = Trick,
								   sender = UnitPid,
								   recipient = UnitPid,
								   use_spirit = Spirit}).


magic_hit(Trick, UnitPid, MagicType, Enemy, DValue) when is_record(DValue, d_value),
														 is_pid(UnitPid),
														 is_pid(Enemy) ->
	Luck  = user_state:get(?puserpid(UnitPid), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	magic_hit(Trick, UnitPid, MagicType, Enemy, Value);
magic_hit(Trick, UnitPid, MagicType, Enemy, Value) when is_number(Value),
														is_pid(UnitPid),
														is_pid(Enemy) ->
	unit:got_heal(UnitPid, #b_heal{value = Value,
								   trick = Trick,
								   sender = UnitPid,
								   recipient = UnitPid}).

magic_hit_mass(Trick, UnitPid, MagicType, Enemy, Cnt, DValue) when is_record(DValue, d_value),
																   is_pid(UnitPid),
																   is_pid(Enemy) ->
	Luck  = user_state:get(?puserpid(UnitPid), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	magic_hit_mass(Trick, UnitPid, MagicType, Enemy, Cnt, Value);
magic_hit_mass(Trick, UnitPid, MagicType, Enemy, Cnt, Value) when is_number(Value),
																  is_pid(UnitPid),
																  is_pid(Enemy) ->
	unit:got_heal(UnitPid, #b_heal{value = Value,
								   trick = Trick,
								   sender = UnitPid,
								   recipient = UnitPid}).


magic_heal(Trick, UnitPid, MagicType, Enemy, DValue) when is_record(DValue, d_value),
														  is_pid(UnitPid),
														  is_pid(Enemy) ->
	Luck  = user_state:get(?puserpid(UnitPid), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	magic_heal(Trick, UnitPid, MagicType, Enemy, Value);
magic_heal(Trick, UnitPid, MagicType, Enemy, Value) when is_number(Value),
														 is_pid(UnitPid),
														 is_pid(Enemy) ->
	unit:got_heal(UnitPid, #b_heal{value = Value,
								   trick = Trick,
								   sender = UnitPid,
								   recipient = UnitPid}).

magic_heal_mass(Trick, UnitPid, MagicType, Enemy, Cnt, DValue) when is_record(DValue, d_value),
																	is_pid(UnitPid),
																	is_pid(Enemy) ->
	Luck  = user_state:get(?puserpid(UnitPid), 'mfs.luck'),
	Value = formula:get_from_interval(DValue, Luck),
	magic_heal_mass(Trick, UnitPid, MagicType, Enemy, Cnt, Value);
magic_heal_mass(Trick, UnitPid, MagicType, Enemy, Cnt, Value) when is_number(Value),
																   is_pid(UnitPid),
																   is_pid(Enemy) ->
	unit:got_heal(UnitPid, #b_heal{value = Value,
								   trick = Trick,
								   sender = UnitPid,
								   recipient = UnitPid}).


