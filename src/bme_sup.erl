%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор всего:)
%%% ====================================================================

-module(bme_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	BME =
		{bme, {bme, start_link, []},
		permanent, 1000, worker, [bme]},
	BattlesSup =
		{battles_sup, {battles_sup, start_link, []},
		permanent, infinity, supervisor, [battles_sup]},
	{ok,{{one_for_one, 15, 60}, [BME, BattlesSup]}}.

