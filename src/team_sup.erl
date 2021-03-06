%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор команды в бою
%%% Управление списком участников боя с одной стороны
%%% ====================================================================


-module(team_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

start_link(Team) when is_record(Team, b_team) ->
	supervisor:start_link(?MODULE, [Team]).

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
init([Team]) ->
	?DBG("Start team supervisor ~p~n", [{Team#b_team.battle_id, Team#b_team.id}]),
	true = gproc:add_local_name({team_sup, Team#b_team.battle_id, Team#b_team.id}),

	%% запускаем супервайзеры участников команд и ген-сервер тимы
	Children = lists:map(fun(Unit) -> ?UNIT_SUP(Team, Unit) end, Team#b_team.units) ++
				   [?TEAM(Team)],
	Strategy = one_for_one,
	MaxR = 0, MaxT = 1,
	{ok, {{Strategy, MaxR, MaxT}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


