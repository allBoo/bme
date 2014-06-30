%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор ударов
%%% каждый выставленный удар создает новый процесс в этом супервайзере
%%% ====================================================================


-module(hits_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% start_link/1
%% ====================================================================
%% запуск супервайзера
start_link(Battle) ->
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
init([Battle]) when is_record(Battle, battle) ->
	?DBG("Start hits queue ~p~n", [Battle#battle.id]),
	true = gproc:add_local_name({hits_queue, Battle#battle.id}),

	Restart  = transient,
	Shutdown = 1000,
	Type     = worker,

	Children =
		[
			{hit, {hit, start_link, []},
			Restart, Shutdown, Type, [hit]}
		],

	Strategy = simple_one_for_one,
	MaxR = 10, MaxT = 10,
	{ok, {{Strategy, MaxR, MaxT}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


