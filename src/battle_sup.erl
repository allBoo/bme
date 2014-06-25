%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор поединка
%%% Основная механика поединка, управление командами и предоставление
%%% моста для взаимодействия между игроками
%%% ====================================================================

-module(battle_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").

-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

start_link(Battle) when is_record(Battle, battle) ->
	supervisor:start_link(?MODULE, [Battle]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([Battle]) ->
	?DBG("Start battle supervisor for ID ~p~n", [Battle#battle.id]),
	%% регистрируем имя супервайзера
	true = gproc:add_local_name({battle_sup, Battle#battle.id}),

	%% запускаем ген-сервер боя и супервайзеры команд
	Children = [?BATTLE(Battle)] ++ lists:map(fun(Team) -> ?TEAM_SUP(Battle, Team) end, Battle#battle.teams),
	Strategy = one_for_one,
	MaxR = 0, MaxT = 1,
	{ok, {{Strategy, MaxR, MaxT}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


