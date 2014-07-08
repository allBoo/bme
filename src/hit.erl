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

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

-export([hit/2,
		 reply/3,
		 cancel/1,
		 test/0]).


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


%% cancel/1
%% ====================================================================
%% отмена размена
cancel(HitPid) when is_pid(HitPid) ->
	gen_server:cast(HitPid, cancel).

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
	random:seed(now()),

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

	%% расчет размена ударами
	%{AttackerDamage, DefendantDamage} =
	hit_process(BattleId, Hit#b_hit.sender, Hit, Hit#b_hit.recipient, ReplyHit),

	%% отправляем выжившим юнитам сообщение о завершении хода
	%% тип размена и с кем
	send_hit_done(Hit#b_hit.recipient, {obtained, Hit#b_hit.sender}),
	send_hit_done(Hit#b_hit.sender,    {sended,   Hit#b_hit.recipient}),

	%% завершаем процесс
	{stop, normal, Hit};


%% отмена размена
handle_cast(cancel, Hit) ->
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
	%% формируем пустой ответный ход
	ReplyHit = #b_hit{sender    = Hit#b_hit.recipient,
					  recipient = Hit#b_hit.sender,
					  hits      = [],
					  block     = [],
					  timeout   = Hit#b_hit.timeout,
					  timeout_pass = true},
	reply(0, self(), ReplyHit),
	{noreply, Hit};


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


%% send_hit_done/1
%% ====================================================================
%% отпраляем юнитам сообщение о завершении размена
%% сообщение отправляется только живому юниту
send_hit_done(UnitPid, Message) ->
	unit:hit_done(UnitPid, Message).


test() ->
	hit_process(0,
				1,
				#b_hit{sender = 0,
					   recipient = 0,
					   battle_id = 0,
					   hits = [head],
					   block = [head, torso, paunch],
					   timeout_pass = false,
					   magic_pass = false
				},
				2,
				#b_hit{sender = 0,
					   recipient = 0,
					   battle_id = 0,
					   hits = [head, legs],
					   block = [head, torso, paunch],
					   timeout_pass = false,
					   magic_pass = false
				}).

%% hit_process/5
%% ====================================================================
%% расчет размена ударами
hit_process(BattleId, AttackerPid, AttackerHit, DefendantPid, DefendantHit) ->
	%% сформируем общий список поочередных ударов
	HitsQueue = generate_hits_queue(AttackerPid, AttackerHit, DefendantPid, DefendantHit),

	%% отработка ударов по очереди, начиная с атакующего
	battle_log:start_block(BattleId),
	hits_queue(BattleId, HitsQueue),
	battle_log:commit(BattleId),

	%?DBG("TOTAL ~p~n", [{AttackerDamage, DefendantDamage}]),
	ok.


%% generate_hits_queue/4
%% ====================================================================
%% генерация очереди разменов ударами
generate_hits_queue(AttackerPid, AttackerHit, DefendantPid, DefendantHit) ->
	generate_hits_queue0(AttackerPid, AttackerHit#b_hit.hits, AttackerHit#b_hit.block,
						 DefendantPid, DefendantHit#b_hit.hits, DefendantHit#b_hit.block,
						 0, []).

generate_hits_queue0(_, [], _, _, [], _, _, Acc) ->
	Acc;

generate_hits_queue0(AttackerPid, AttackerHits, AttackerBlock, DefendantPid, DefendantHits, DefendantBlock, Index, Acc) ->
	{R1, T1} = generate_hits_queue1(AttackerPid, AttackerHits, AttackerBlock, DefendantPid, DefendantBlock, Index),
	{R2, T2} = generate_hits_queue1(DefendantPid, DefendantHits, DefendantBlock, AttackerPid, AttackerBlock, Index),
	generate_hits_queue0(AttackerPid, T1, AttackerBlock, DefendantPid, T2, DefendantBlock, Index + 1, Acc ++ R1 ++ R2).

generate_hits_queue1(_, [], _, _, _, _) ->
	{[], []};

generate_hits_queue1(AttackerPid, [Hit | AttackerHits], AttackerBlock, DefendantPid, DefendantBlock, Index) ->
	{[{AttackerPid, Hit, AttackerBlock, DefendantPid, DefendantBlock, Index}], AttackerHits}.


%% hits_queue/2
%% ====================================================================
%% размен ударами
hits_queue(_, []) ->
	%?DBG("Hits done~n", []),
	ok;


hits_queue(BattleId, [{AttackerPid, HitZone, AttackerBlock, DefendantPid, DefendantBlock, Index} | TailHits]) ->
	%?DBG("DO HIT, ~p~n", [{HitZone, Blocks, Attacker#user.id, Defendant#user.id}]),
	%% получаем данные юнитов при каждом ударе, чтобы не потерять ничего при конкурентных запросах
	AttackerUnit  = unit:get_state(AttackerPid),
	DefendantUnit = unit:get_state(DefendantPid),

	Attacker  = ?user(AttackerUnit),
	Defendant = ?user(DefendantUnit),

	%% получаем оружие атакующего
	AttackerWeapon = get_weapon(user_helper:get_weapons(Attacker), Index),

	%% в случае контрудара берем рандомную зону
	Hit = case HitZone == counter of
			  true  -> lists:nth(random:uniform(5), [head, torso, paunch, belt, legs]);
			  false -> HitZone
		  end,

	%% расчитываем уворот
	Dodge = formula:is_dodge(Attacker, Defendant),
	case Dodge of
		true ->
			%% котрудар
			%% нельзя произвести контр-удар в ответ на контр-удар
			Counter = (HitZone /= counter) and formula:is_counter(Attacker, Defendant),
			%% крит
			Crit = false,
			%% парир
			Parry = false,
			%% обычный блок
			Block = false,
			%% блок щитом
			Shield = false;
		false ->
			Counter = false,
			%% крит
			Crit = formula:is_crit(AttackerWeapon, Attacker, Defendant),
			%% парир
			Parry = formula:is_parry(Attacker, Defendant),
			case Parry of
				true ->
					%% обычный блок
					Block = false,
					%% блок щитом
					Shield = false;
				false ->
					%% попадание в блок
					Block = lists:member(Hit, DefendantBlock),
					%% блок щитом
					Shield = case Block of
								 true  -> false;
								 false -> formula:is_shield_block(Attacker, Defendant)
							 end
			end
	end,

	%% попадание = попал не в блок или крит
	Hited = not(Dodge) and (Crit or not(Block or Shield)),
	%% пробой защиты
	CritBreak = Hited and Crit and (Parry or Block or Shield),

	%% определяем тип урона оружием на основе ГСЧ
	DamageType = user_helper:get_weapon_type(AttackerWeapon#u_weapon.damage_type),

	%% собираем всю инфу по удару
	HitResult = #b_hit_result{
		hit         = Hit,
		blocks      = DefendantBlock,
		damage_type = DamageType,
		weapon_type = AttackerWeapon#u_weapon.type,
		weapon_twain = AttackerWeapon#u_weapon.twain == true,
		hited       = Hited,
		crit        = Crit,
		crit_break  = CritBreak,
		dodge       = Dodge,
		counter     = Counter,
		parry       = Parry,
		block       = Block,
		shield      = Shield
	},

	case Hited of
		%% если есть попадание, расчитываем исходный наносимый урон
		%% отправляем защищающемуся сообщение с кол-вом нанесенного ему урона
		%% он в ответ должен сказать сколько он реально получил урона (за счет защитных приемов и тд)
		%% при этом он может парралельно нанести ответный урон атакующему, но нам здесь это не важно
		true ->
			%% считаем урон для данного удара
			Damage = formula:get_damage(Hit, DamageType, Crit, CritBreak, Attacker, AttackerWeapon, Defendant),

			{ok, AttackerDamage} = unit:got_damage(DefendantPid, AttackerUnit, HitResult#b_hit_result{damage = Damage}, self()),

			%% полученный результат сообщаем атакующему, сколько он реально нанес и кому
			unit:hit_damage(AttackerPid, DefendantUnit, HitResult#b_hit_result{damage = AttackerDamage}, self());

		%% если удара не произошло, уведомляем об этом защищающегося
		false ->
			unit:avoid_damage(DefendantPid, AttackerUnit, HitResult, self())
	end,

	%% если сработала контра, добавляем удар от защищающегося
	AddHit = case Counter of
				 true -> [{DefendantPid, counter, DefendantBlock, AttackerPid, AttackerBlock, Index + 1}];	% @todo поправить index
				 false -> []
			 end,

	%% продолжаем размены
	hits_queue(BattleId, TailHits ++ AddHit).


%% get_weapon/2
%% ====================================================================
%% выбирает оружие для удара
get_weapon([Weapon | WeaponsList], _Index) when length(WeaponsList) == 0 ->
	Weapon;

get_weapon(WeaponsList, Index) ->
	lists:nth((Index rem length(WeaponsList)) + 1, WeaponsList).

