%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор всех боев
%%% Запускает новые поединки, распределяет нагрузку по нодам
%%% ====================================================================


-module(battles_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").

%% API
-export([start_link/0,
		 start_battle/1,
		 finish_battle/1]).

%% Supervisor callbacks
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_battle(Battle) when (is_record(Battle, battle) and (Battle#battle.id == 0)) ->
	?DBG("Start new battle ~p~n", [Battle#battle.id]),
	%% @todo create battle record
	start_battle(Battle#battle{id = 1});

start_battle(Battle) when is_record(Battle, battle) ->
	?DBG("Restore exists battle ~p~n", [Battle#battle.id]),
	supervisor:start_child(?MODULE, [Battle]).


finish_battle(Battle) when is_record(Battle, battle) ->
	case gproc:lookup_local_name({battle_sup, Battle#battle.id}) of
		undefined -> ?ERROR_NOT_APPLICABLE;
		BattlePid -> finish_battle(BattlePid)
	end;

finish_battle(BattlePid) when is_pid(BattlePid) ->
	?DBG("Terminate battle ~p~n", [BattlePid]),
	supervisor:terminate_child(?MODULE, BattlePid).


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

%% ====================================================================
%% Internal functions
%% ====================================================================


