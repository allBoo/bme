%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Точный удар
%%% Следующий ваш удар игнорирует броню противника и снижает сопротивление
%%% урону на 250ед., но не ниже 0.
%%% ====================================================================


-module(multi_skiparmor).
-behaviour(gen_buff).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/1, on_unit_before_calc_damage/2]).


new(Buff) ->
	{ok, Buff#buff{
			id   = multi_skiparmor,
			name = <<"Точный удар"/utf8>>,
			type = effect,
			time = infinity,
			charges = undefined,
			value = []
		}}.


%% перед ударом умешьшаем броню и защиту противника и снимаем бафф
on_unit_before_calc_damage({HitResult, Attacker, AttackerWeapon, Defendant}, _Buff) ->
	%% защита
	DP = reduce_dp(Defendant#user.dprotection),
	WP = reduce_wp(Defendant#user.wprotection),
	{remove_handler, {HitResult,
					  Attacker,
					  AttackerWeapon,
					  Defendant#user{armor = #u_armor{}, dprotection = DP, wprotection = WP}
	}}.


%% ====================================================================
%% Internal functions
%% ====================================================================

reduce_dp(DP) ->
	DP#u_dprotection{
		head   = reduce_dpz(DP#u_dprotection.head),
		torso  = reduce_dpz(DP#u_dprotection.torso),
		belt   = reduce_dpz(DP#u_dprotection.belt),
		legs   = reduce_dpz(DP#u_dprotection.legs)
	}.

reduce_dpz(DPZ) ->
	DPZ#u_dprotection_zone{
		general = reduce_val(DPZ#u_dprotection_zone.general),
		prick   = reduce_val(DPZ#u_dprotection_zone.prick),
		chop    = reduce_val(DPZ#u_dprotection_zone.chop),
		crush   = reduce_val(DPZ#u_dprotection_zone.crush),
		cut     = reduce_val(DPZ#u_dprotection_zone.cut)
	}.

reduce_wp(WP) ->
	WP#u_wprotection{
		general = reduce_val(WP#u_wprotection.general),
		air     = reduce_val(WP#u_wprotection.air),
		fire    = reduce_val(WP#u_wprotection.fire),
		water   = reduce_val(WP#u_wprotection.water),
		earth   = reduce_val(WP#u_wprotection.earth),
		light   = reduce_val(WP#u_wprotection.light),
		dark    = reduce_val(WP#u_wprotection.dark),
		gray    = reduce_val(WP#u_wprotection.gray)
	}.

reduce_val(Val) ->
	max(Val - 250, 0).
