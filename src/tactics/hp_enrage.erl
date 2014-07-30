%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Ярость
%%% Увеличивает ваш урона на 5% до конца боя.
%%% Можно применять трижды
%%% ====================================================================


-module(hp_enrage).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, renew/2, on_unit_calc_damage/2]).

-record(state, {uses = 0, increase = 0}).

new(Buff) ->
	{ok, Buff#buff{
			id   = hp_enrage,
			name = <<"Ярость"/utf8>>,
			type = trick,
			time = infinity,
			charges = undefined,
			value = [],
			state = #state{uses = 1, increase = 0.05}
		}}.

renew(_, #buff{state = State} = Buff) when State#state.uses == 1 ->
	{ok, Buff#buff{
			state = State#state{uses = 2, increase = 0.1}
		}};

renew(_, #buff{state = State} = Buff) when State#state.uses == 2 ->
	{ok, Buff#buff{
			lock = [hp_enrage],
			state = State#state{uses = 3, increase = 0.15}
		}};

renew(_, Buff) -> {ok, Buff}.

%% вообще это не совсем правильно, увеличиваем весь урон на 5-15%,
%% реально нада увеличивать МФ мощности
on_unit_calc_damage(Damage0, #buff{state = State} = Buff) ->
	Damage = round(Damage0 + Damage0 * State#state.increase),
	{ok, Damage, Buff}.

%% ====================================================================
%% Internal functions
%% ====================================================================

