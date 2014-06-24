%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор всех боев
%%% Запускает новые поединки, распределяет нагрузку по нодам
%%% управляет вмешательством в бои
%%% ====================================================================

-module(bme_sup).

-behaviour(supervisor).

-include_lib("bme.hrl").

%% API
-export([start_link/0, start_battle/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(BATTLE(Battle), {Battle#battle.id, {battle_sup, start_link, [Battle]}, permanent, 5000, supervisor, [battle_sup]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_battle(Battle) when (is_record(Battle, battle) and (Battle#battle.id == 0)) ->
	?DBG("Start new battle ~p~n", [Battle#battle.id]),
	%% @todo create battle record
	start_battle(Battle#battle{id = 1});

start_battle(Battle) when is_record(Battle, battle) ->
	?DBG("Restore exists battle ~p~n", [Battle#battle.id]),
	supervisor:start_child(?MODULE, [Battle]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart  = transient,
	Shutdown = infinity,
	Type     = supervisor,

	Children =
		[
			{battle, {battle_sup, start_link, []},
			Restart, Shutdown, Type, [battle_sup]}
		],

	Strategy = simple_one_for_one,
	MaxR = 10, MaxT = 10,
	{ok, {{Strategy, MaxR, MaxT}, Children}}.

