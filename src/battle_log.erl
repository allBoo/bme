%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Ведение лога боя
%%% ====================================================================


-module(battle_log).
-behaviour(gen_server).
-include_lib("bme.hrl").
-include_lib("battle_log.hrl").

%% standart behaviourals
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(CAST(BattleId, Cmd), case is_pid(BattleId) of
								 true -> gen_server:cast(BattleId, Cmd);
								 false ->
									 case gproc:lookup_local_name({battle_log, BattleId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 BattlePid -> gen_server:cast(BattlePid, Cmd) end end).
-define(CALL(BattleId, Cmd), case is_pid(BattleId) of
								 true -> gen_server:call(BattleId, Cmd);
								 false ->
									 case gproc:lookup_local_name({battle_log, BattleId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 BattlePid -> gen_server:call(BattlePid, Cmd) end end).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

-export([start_block/1,
		 commit/1,
		 rollback/1,
		 hit/2,
		 hit/3]).


%% start_link/1
%% ====================================================================
%% регистрирует процесс нового логгера
start_link(BattleId) when is_integer(BattleId) ->
	gen_server:start_link(?MODULE, BattleId, []).


%% start_block/1
%% ====================================================================
%% начало блока в логе (eg транзакция)
start_block(BattleId) ->
	?CALL(BattleId, start_block).


%% commit/1
%% ====================================================================
%% сохранение блока записей в лог
commit(BattleId) ->
	?CALL(BattleId, commit).


%% rollback/1
%% ====================================================================
%% удаление несохраненного блока записей
rollback(BattleId) ->
	?CALL(BattleId, rollback).


%% hit/2
%% ====================================================================
%% запись удара в лог
hit(BattleId, Hit) ->
	?CALL(BattleId, {hit, Hit}).


%% hit/3
%% ====================================================================
%% запись удара в лог
hit(BattleId, TransactionId, Hit) ->
	?CAST(BattleId, {hit, TransactionId, Hit}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {battle_id}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(BattleId) ->
	?DBG("Start new battle logger ~p~n", [BattleId]),

	%% регистрируем имя сервера
	true = gproc:add_local_name({battle_log, BattleId}),

	{ok, #state{battle_id = BattleId}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

%% начало транзакции
handle_call(start_block, {FromPid, _}, State) ->
	TransactionId = {log_transaction, FromPid},
	gproc:reg_or_locate({n, l, TransactionId}, []),
	gproc:set_value(TransactionId, []),
	{reply, {ok, TransactionId}, State};


%% коммит транзакции
handle_call(commit, {FromPid, _}, State) ->
	%commit_local_transaction(State),
	TransactionId = {log_transaction, FromPid},
	R = case gproc:lookup_local_name(TransactionId) of
			undefined -> ?ERROR_NOT_APPLICABLE;
			Logs ->
				gproc:unregister_name(TransactionId),
				write(Logs)
		end,
	{reply, R, State};


%% откат транзакции
handle_call(rollback, {FromPid, _}, State) ->
	TransactionId = {log_transaction, FromPid},
	R = case gproc:lookup_local_name(TransactionId) of
			undefined -> ?ERROR_NOT_APPLICABLE;
			_ -> gproc:unregister_name({n, l, TransactionId}), ok
		end,
	{reply, R, State};


%% запись строки удара в лог
handle_call({hit, Hit}, {FromPid, _}, State) ->
	TransactionId = {log_transaction, FromPid},
	R = case gproc:lookup_local_name(TransactionId) of
			%% если транзакция не запущена, пишем локально
			undefined -> write({hit, Hit});
			_ -> write({hit, Hit}, TransactionId)
		end,
	{reply, R, State};


%% unknown request
handle_call(_Request, _From, State) ->
	{reply, ok, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================


%% запись строки удара в лог
handle_cast({hit, TransactionId, Hit}, State) ->
	case gproc:lookup_local_name(TransactionId) of
		%% если транзакция не запущена, пишем локально
		undefined -> write({hit, Hit});
		_ -> write({hit, Hit}, TransactionId)
	end,
	{noreply, State};


%% unknown request
handle_cast(_Msg, State) ->
	{noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

write(LogsList) when is_list(LogsList) ->
	?DBG("~p", [LogsList]),
	ok;

write(Log) ->
	?DBG("~p", [Log]),
	ok.

write(Log, TransactionId) ->
	Logs = gproc:get_value({n, l, TransactionId}),
	gproc:set_value({n, l, TransactionId}, Logs ++ [Log]),
	ok.


