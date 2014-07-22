%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Супервизор приемов
%%% ====================================================================


-module(trick_sup).
-behaviour(supervisor).
-include_lib("bme.hrl").
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

start_link(Unit) when is_record(Unit, b_unit) ->
	supervisor:start_link(?MODULE, [Unit]).

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
init([Unit]) ->
	?DBG("Start unit tricks supervisor ~p~n", [{Unit#b_unit.id}]),

	%% запускаем эвент-менеджер и менеджер приемов
	Children = [?TRICK_EV(Unit), ?TRICK_MGR(Unit)],

	Strategy = one_for_one,
	MaxR = 10, MaxT = 10,
	{ok, {{Strategy, MaxR, MaxT}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


