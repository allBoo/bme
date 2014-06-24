%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Battle Manager Application module
%%% ====================================================================

-module(bme_app).

-behaviour(application).

-include_lib("bme.hrl").

%% Application callbacks
-export([start/2, stop/1]).
-export([start_haot/3, character_attack/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bme_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Application Api
%% ===================================================================

%% start_haot/3
%% ====================================================================
%% Запуск хаотического поединка
-spec start_haot(UsersIds ::[integer()], City ::integer(), Options ::[term()]) -> Result when
		  Result :: {ok, Id :: integer(), UsersIds :: [integer()]} | {error, Reason :: string()}.
start_haot(UsersIds, City, Options) ->
	Properties = proplists:unfold(Options),
	%% получаем инфу по всем пользователям и проверяем возможность их участия в поединке
	Users = lists:filter(fun(User) ->
					UserInfo = User#user.info,
					Vitality = User#user.vitality,
					Clother  = User#user.clother,
					BattleLevel = proplists:get_value(level, Properties, #b_level{min=0, max=21}),
					%% проверяем что пользователи находятся в походящих локациях (город, зал)
					(User#user.city == City) and lists:member(User#user.room, ?BATTLE_PLACES) and
					%% проверяем что пользователи подходят под условия поединка
						%% уровень персонажа
					((UserInfo#u_info.level >= BattleLevel#b_level.min) and (UserInfo#u_info.level =< BattleLevel#b_level.max)) and
						%% уровень ХП
					((Vitality#u_vitality.hp / Vitality#u_vitality.maxhp) > 0.3) and
						%% стоимость обвеса
					((Clother#u_clother.cost =< proplists:get_value(max_cost, Properties, 0)) or (proplists:get_value(max_cost, Properties, 0) == 0))
			end, user_helper:get(UsersIds)),
	%?DBG("Users ~p~n", [Users]),

	case length(Users) >= 2 of
		true ->
			%% делим пользователей на команды в соотв. с выбранной стратегией (рандом / обвес)
			Teams = user_helper:split_teams(Users, 2, proplists:get_value(distribution, Properties, default)),
			%?DBG("Teams ~p~n", [Teams]),
			FirstUser = lists:nth(1, Users),
			%% говорим супервайзеру запустить новый бой
			bme_sup:start_battle(#battle{id = 0,
										 type = haot,
										 blood = proplists:get_bool(blood, Properties),
										 city = City,
										 room = FirstUser#user.city,
										 timeout = proplists:get_value(timeout, Properties, 5),
										 teams = Teams
										});
		false ->
			?ERROR_LOW_TEAM
	end.

%% character_attack/2
%% ====================================================================
%% Нападение на персонажа
character_attack(_Attacker, _Victim) ->
	ok.

