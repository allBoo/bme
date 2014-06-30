%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Размен ударами
%%% ====================================================================


-module(hit).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% api
-export([hit/2,
		 reply/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).


%% start_link/1
%% ====================================================================
%% регистрирует процесс нового размена
start_link(Hit) when is_record(Hit, b_hit) ->
	gen_server:start_link(?MODULE, Hit, []).


%% hit/2
%% ====================================================================
%% запуск процесса удара
hit(BattleId, Hit) when is_record(Hit, b_hit), is_integer(BattleId) ->
	%% если очередь ударов существует (бой запущен), выставляем новый удар
	case gproc:lookup_local_name({hits_queue, BattleId}) of
		undefined -> ?ERROR_NOT_APPLICABLE;
		QueuePid  ->
			%% пробуем стартануть размен
			case supervisor:start_child(QueuePid, [Hit]) of
				{ok, HitPid} ->
					{ok, HitPid};
				%% что-то пошло не так
				_ ->
					%% пробуем найти существующий размен
					%% выставленный оппонентом
					case gproc:lookup_local_name({hit, Hit#b_hit.recipient, Hit#b_hit.sender}) of
						ExistsHit when is_pid(ExistsHit) ->
							%% делаем ответ на размен
							reply(BattleId, ExistsHit, Hit);
						%% если такого нет, то вообще все плохо и непонятно
						_ ->
							?ERROR_UNDEFINED
					end
			end
	end.


%% reply/2
%% ====================================================================
%% ответ на размен
reply(BattleId, HitPid, Hit) when is_record(Hit, b_hit), is_pid(HitPid) ->
	gen_server:cast(HitPid, {reply, BattleId, Hit}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

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
init(Hit) when is_record(Hit, b_hit) ->
	?DBG("Start new hit ~p~n", [Hit]),
	%% регистрируем процесс чтобы избежать гонок
	gproc:add_local_name({hit, Hit#b_hit.sender, Hit#b_hit.recipient}),
	gproc:add_local_name({hit, Hit#b_hit.recipient, Hit#b_hit.sender}),

	%% уведомляем юнита, которому выставили размен
	unit:hited(Hit#b_hit.recipient, {Hit#b_hit.sender, self()}),

	%% если за отведенное время не будет ответа на размен, то удар по тайму
	{ok, Hit, get_timeout(Hit)}.


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

%% unknown
handle_call(_, _, State) ->
	{reply, ?ERROR_WRONG_CALL, State}.


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

%% ответ на размен
handle_cast({reply, BattleId, ReplyHit}, Hit) when ReplyHit#b_hit.sender == Hit#b_hit.recipient,
												   ReplyHit#b_hit.recipient == Hit#b_hit.sender ->
	?DBG("Hit reply omitted ~p~n", [{Hit, BattleId, ReplyHit}]),
	Attacker  = unit:get_state(Hit#b_hit.sender),
	Defendant = unit:get_state(Hit#b_hit.recipient),

	%% пока не заморачиваемся, просто возьмем рандомное значение урона
	AttackerHpDamage  = random:uniform(((Defendant#b_unit.user)#user.vitality)#u_vitality.hp),
	DefendantHpDamage = random:uniform(((Attacker#b_unit.user)#user.vitality)#u_vitality.hp),

	AttackerDamage = #b_damage{
		damage      = AttackerHpDamage,
		loss        = DefendantHpDamage,
		loss_mana   = 0,
		tactics     = #b_tactics{attack = 1, crit = 1},
		opponent_id = Defendant#b_unit.id,
		exp         = 100
	},
	DefendantDamage = #b_damage{
		damage      = DefendantHpDamage,
		loss        = AttackerHpDamage,
		loss_mana   = 0,
		tactics     = #b_tactics{attack = 2, crit = 1},
		opponent_id = Defendant#b_unit.id,
		exp         = 100
	},

	%% сообщаем юнитам о нанесенном им уроне
	unit:damage(Hit#b_hit.recipient, AttackerDamage),
	unit:damage(Hit#b_hit.sender, DefendantDamage),

	{stop, normal, Hit};


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

%% алерт о приближении таймаута
handle_info(timeout, #b_hit{timeout_alert = false} = Hit) ->
	?DBG("Hit timeout alert omitted ~p~n", [Hit]),
	%% уведомляем юнита, которому выставили размен,
	%% что через 10 сек будет пропуск хода
	unit:timeout_alarm(Hit#b_hit.recipient, Hit#b_hit.sender),
	{noreply, Hit#b_hit{timeout_alert = true}, get_alert_timeout()};


%% пропуск хода по таймауту
handle_info(timeout, #b_hit{timeout_alert = true} = Hit) ->
	?DBG("Hit timeout omitted ~p~n", [Hit]),
	{stop, normal, Hit};


%% unknown
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

%% расчитывает начальный таймаут хода до предупреждения о пропуске хода
%%
get_timeout(Hit) ->
	((Hit#b_hit.timeout * 60) * 1000) - get_alert_timeout().

get_alert_timeout() ->
	10000.

