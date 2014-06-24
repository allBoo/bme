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
	ok;

start_battle(Battle) when is_record(Battle, battle) ->
	?DBG("Restore exists battle ~p~n", [Battle#battle.id]),
	?ERROR_UNCOMPLETED.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, []} }.

