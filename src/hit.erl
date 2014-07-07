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

%% результат удара
-record(hit_damage, {damage = 0,
					 lost   = 0,
					 healed = 0,
					 lost_mana = 0,
					 exp = 0}).
-record(hit_result, {attacker_damage = #hit_damage{},
					 defendant_damage = #hit_damage{},
					 attacker_tactics = #b_tactics{},
					 defendant_tactics = #b_tactics{},
					 attacker = #b_unit{},
					 defendant = #b_unit{},
					 counter = false,
					 miss = false}).

-define(TACTIC(Cond), case Cond of true -> 1; false -> 0 end).
-define(TACTIC(Cond1, Cond2, Val), case Cond1 of true -> (case Cond2 of true -> Val; false -> 1 end); false -> 0 end).

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
	Attacker  = unit:get_state(Hit#b_hit.sender),
	Defendant = unit:get_state(Hit#b_hit.recipient),

	%% расчет размена ударами
	{AttackerDamage, DefendantDamage} = hit_process(BattleId, Attacker, Hit, Defendant, ReplyHit),

	%% сообщаем юнитам о нанесенном им уроне
	DefendantRes = unit:damage(Hit#b_hit.recipient, AttackerDamage),
	AttackerRes  = unit:damage(Hit#b_hit.sender, DefendantDamage),

	%% отправляем выжившим юнитам сообщение о завершении хода
	%% тип размена и с кем
	send_hit_done(DefendantRes, Hit#b_hit.recipient, {obtained, Hit#b_hit.sender}),
	send_hit_done(AttackerRes,  Hit#b_hit.sender,    {sended,   Hit#b_hit.recipient}),

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
send_hit_done({ok, alive}, UnitPid, Message) ->
	unit:hit_done(UnitPid, Message),
	ok;

send_hit_done(_, _UnitPid, _Message) ->
	ok.


test() ->
	hit_process(0,
				battle_helper:create_unit(example:get(1), 1),
				#b_hit{sender = 0,
					   recipient = 0,
					   battle_id = 0,
					   hits = [head],
					   block = [head, torso, paunch],
					   timeout_pass = false,
					   magic_pass = false
				},
				battle_helper:create_unit(example:get(2), 1),
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
hit_process(BattleId, Attacker, AttackerHit, Defendant, DefendantHit) ->
	AttackerUser  = Attacker#b_unit.user,
	DefendantUser = Defendant#b_unit.user,

	%% уравниваем кол-во ударов
	MaxHits = max(length(AttackerHit#b_hit.hits), length(DefendantHit#b_hit.hits)),
	AttackerHits  = AttackerHit#b_hit.hits ++ lists:duplicate(MaxHits - length(AttackerHit#b_hit.hits), none),
	DefendantHits = DefendantHit#b_hit.hits ++ lists:duplicate(MaxHits - length(DefendantHit#b_hit.hits), none),
	%?DBG("~p", [{AttackerHits, DefendantHits}]),

	%% получаем список оружия для каждого в виде списка
	AttackerWeapons  = user_helper:get_weapons(AttackerUser),
	DefendantWeapons = user_helper:get_weapons(DefendantUser),
	%?DBG("~p", [{AttackerWeapons, DefendantWeapons}]),

	%% отработка ударов по очереди, начиная с атакующего
	battle_log:start_block(BattleId),
	{AttackerDamage, DefendantDamage} =
		hits_queue(AttackerHits, AttackerHit#b_hit.block, AttackerWeapons, Attacker, #b_damage{},
				   DefendantHits, DefendantHit#b_hit.block, DefendantWeapons, Defendant, #b_damage{},
				   0, BattleId),
	battle_log:commit(BattleId),

	%?DBG("TOTAL ~p~n", [{AttackerDamage, DefendantDamage}]),
 	{AttackerDamage, DefendantDamage}.


%% hits_queue/4
%% ====================================================================
%% размен ударами
hits_queue([], _, _, Attacker, AttackerDamage,
		   [], _, _, Defendant, DefendantDamage,
		   _Index, _BattleId) ->
	%?DBG("Hits done~n", []),
	{AttackerDamage#b_damage{
			opponent_id = ?userid(Defendant)
		},
	 DefendantDamage#b_damage{
			opponent_id = ?userid(Attacker)
		}};


hits_queue([CurAttHit | AttackerHits],  AttackerBlock,  AttackerWeapons,  Attacker, AttackerDamage,
		   [CurDefHit | DefendantHits], DefendantBlock, DefendantWeapons, Defendant, DefendantDamage,
		   Index, BattleId) ->
	%% сначала бьет атакующий
	AttackerWeapon = get_weapon(AttackerWeapons, Index),
	ResAttacker = do_hit(CurAttHit, DefendantBlock, Attacker, AttackerWeapon, Defendant, BattleId),

	Attacker0  = ResAttacker#hit_result.attacker,
	Defendant0 = ResAttacker#hit_result.defendant,

	%% потом защищающийся
	DefendantWeapon = get_weapon(DefendantWeapons, Index),
	ResDefendant = do_hit(CurDefHit, AttackerBlock, Defendant0, DefendantWeapon, Attacker0, BattleId),

	Attacker1  = ResDefendant#hit_result.defendant,
	Defendant1 = ResDefendant#hit_result.attacker,

	%% если у кого-то сработала контра, добавляем еще один удар
	{AttAddHit, DefAddHit} = case ResAttacker#hit_result.counter or ResDefendant#hit_result.counter of
								 true  ->
									 {case ResAttacker#hit_result.counter of true -> [counter]; false -> [none] end,
									  case ResDefendant#hit_result.counter of true -> [counter]; false -> [none] end};
								 false -> {[], []}
							 end,

	%?DBG("RES ~p~n", [{ResAttacker, ResDefendant, AttAddHit, DefAddHit}]),

	%% суммирование урона и тактик за размен
	AttackerDamage0  = merge_damage(AttackerDamage, ResAttacker, ResDefendant),
	DefendantDamage0 = merge_damage(DefendantDamage, ResDefendant, ResAttacker),

	%% продолжаем размены
	hits_queue(AttackerHits ++ AttAddHit,  AttackerBlock,  AttackerWeapons,  Attacker1, AttackerDamage0,
			   DefendantHits ++ DefAddHit, DefendantBlock, DefendantWeapons, Defendant1, DefendantDamage0,
			   Index + 1, BattleId).


%% пустой удар - ничего не делаем
do_hit(none, _Block, AttackerUnit, _AttackerWeapon, DefendantUnit, _BattleId) ->
	%?DBG("EMPTY HIT, ~p~n", [{Attacker#user.id, Defendant#user.id}]),
	#hit_result{attacker = AttackerUnit,  defendant = DefendantUnit, miss = true};

%% удар
do_hit(HitZone, Blocks, AttackerUnit, AttackerWeapon, DefendantUnit, BattleId) ->
	%?DBG("DO HIT, ~p~n", [{HitZone, Blocks, Attacker#user.id, Defendant#user.id}]),
	Attacker  = ?user(AttackerUnit),
	Defendant = ?user(DefendantUnit),

	%% в случае контрудара берем рандомную зону
	Hit = case HitZone == counter of
			  true  -> lists:nth(random:uniform(5), [head, torso, paunch, belt, legs]);
			  false -> HitZone
		  end,
	%% расчитываем уворот
	Dodge = false,%formula:is_dodge(Attacker, Defendant),
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
					Block = lists:member(Hit, Blocks),
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
	%% расчет урона
	Damage = case Hited of
		%% если есть попадание, расчитываем нанесенный урон
		true ->
			%% считаем урон для данного удара
			formula:get_damage(Hit, DamageType, Crit, CritBreak, Attacker, AttackerWeapon, Defendant);

		false -> 0
	end,

	%% @todo обработка приемов влияющих на получение урона
	%% сначала защищающегося, потом атакующего
	DefendantDamage  = 0,
	DefendantLost    = Damage,
	DefendantHealed  = 0,
	DefendantManaLost   = 0,
	DefendantManaHealed = 0,

	AttackerDamage  = Damage,
	AttackerLost    = 0,
	AttackerHealed  = 0,
	AttackerManaLost   = 0,
	AttackerManaHealed = 0,

	%% пересчет отставшихся ХП и маны
	AttackerHp    = math:limit(?hp(Attacker) - AttackerLost + AttackerHealed, 0, ?maxhp(Attacker)),
	AttackerMana  = math:limit(?mana(Attacker) - AttackerManaLost + AttackerManaHealed, 0, ?maxmana(Attacker)),
	DefendantHp   = math:limit(?hp(Defendant) - DefendantLost + DefendantHealed, 0, ?maxhp(Defendant)),
	DefendantMana = math:limit(?mana(Defendant) - DefendantManaLost + DefendantManaHealed, 0, ?maxmana(Defendant)),
	AttackerUnit0  = AttackerUnit#b_unit{user = Attacker#user{vitality = (Attacker#user.vitality)#u_vitality{
					hp = AttackerHp, mana = AttackerMana
	}}},
	DefendantUnit0 = DefendantUnit#b_unit{user = Defendant#user{vitality = (Defendant#user.vitality)#u_vitality{
					hp = DefendantHp, mana = DefendantMana
	}}},

	%% считаем полученные тактики
	AttackerTactics = #b_tactics{
		attack  = ?TACTIC(Hited and not(Crit), AttackerWeapon#u_weapon.twain == true, 3),
		crit    = ?TACTIC(Crit, (AttackerWeapon#u_weapon.twain == true) or not(CritBreak), 2),
		hearts  = formula:get_hearts(Damage, Attacker, Defendant)
	},
	DefendantTactics = #b_tactics{
		counter = ?TACTIC(Counter),
		block   = ?TACTIC((Block or Shield) and not(Hited), AttackerWeapon#u_weapon.twain == true, 2),
		parry   = ?TACTIC(Parry and not(Hited))
	},

	%% пишем лог
	Log = case Hited of
			  true  ->
				  #log_hit{attacker = ?log_unit(AttackerUnit0), defendant = ?log_unit(DefendantUnit0),
						   hit = HitZone, blocks = Blocks, damage = AttackerDamage,
						   damage_type = DamageType, weapon_type = AttackerWeapon#u_weapon.type,
						   crit = Crit, crit_break = CritBreak, parry = Parry, block = Block, shield = Shield};
			  false ->
				  #log_miss{attacker = ?log_unit(AttackerUnit0), defendant = ?log_unit(DefendantUnit0),
						    hit = HitZone, blocks = Blocks, damage_type = DamageType,
						    weapon_type = AttackerWeapon#u_weapon.type, dodge = Dodge,
						    counter = Counter, parry = Parry, block = Block, shield = Shield}
		  end,
	battle_log:hit(BattleId, Log),

	%% собираем результат
	#hit_result{attacker_damage = #hit_damage{
					damage = AttackerDamage,
					lost   = AttackerLost - AttackerHealed,
					healed = AttackerHealed,
					lost_mana = AttackerManaLost - AttackerManaHealed,
					exp = formula:get_exp_by_damage(AttackerDamage, ?user(AttackerUnit0), ?user(DefendantUnit0))
				},
				defendant_damage = #hit_damage{
					damage = DefendantDamage,
					lost   = DefendantLost - DefendantHealed,
					healed = DefendantHealed,
					lost_mana = DefendantManaLost - DefendantManaHealed,
					exp = formula:get_exp_by_damage(DefendantDamage, ?user(DefendantUnit0), ?user(AttackerUnit0))
				},
				attacker_tactics = AttackerTactics,
				defendant_tactics = DefendantTactics,
				attacker = AttackerUnit0,
				defendant = DefendantUnit0,
				counter = Counter}.



%% get_weapon/2
%% ====================================================================
%% выбирает оружие для удара
get_weapon([Weapon | WeaponsList], _Index) when length(WeaponsList) == 0 ->
	Weapon;

get_weapon(WeaponsList, Index) ->
	lists:nth((Index rem length(WeaponsList)) + 1, WeaponsList).


%% merge_damage/3
%% ====================================================================
%% суммирование урона и тактик за размен
merge_damage(Damage, AsAttacker, AsDefendant) ->
	DT = Damage#b_damage.tactics,
	Tactics = #b_tactics{
		attack  = (AsAttacker#hit_result.attacker_tactics)#b_tactics.attack    + DT#b_tactics.attack,
		crit    = (AsAttacker#hit_result.attacker_tactics)#b_tactics.crit      + DT#b_tactics.crit,
		hearts  = math:precision((AsAttacker#hit_result.attacker_tactics)#b_tactics.hearts + DT#b_tactics.hearts, 2),
		counter = (AsDefendant#hit_result.defendant_tactics)#b_tactics.counter + DT#b_tactics.counter,
		block   = (AsDefendant#hit_result.defendant_tactics)#b_tactics.block   + DT#b_tactics.block,
		parry   = (AsDefendant#hit_result.defendant_tactics)#b_tactics.parry   + DT#b_tactics.parry
	},
	AsAttackerDamage  = AsAttacker#hit_result.attacker_damage,
	AsDefendantDamage = AsDefendant#hit_result.defendant_damage,
	Damage#b_damage{
		damaged   = Damage#b_damage.damaged + AsAttackerDamage#hit_damage.damage + AsDefendantDamage#hit_damage.damage,
		healed    = Damage#b_damage.healed + AsAttackerDamage#hit_damage.healed + AsDefendantDamage#hit_damage.healed,
		lost      = Damage#b_damage.lost + AsAttackerDamage#hit_damage.lost + AsDefendantDamage#hit_damage.lost,
		lost_mana = Damage#b_damage.lost_mana + AsAttackerDamage#hit_damage.lost_mana + AsDefendantDamage#hit_damage.lost_mana,
		exp       = Damage#b_damage.exp + AsAttackerDamage#hit_damage.exp + AsDefendantDamage#hit_damage.exp,
		tactics   = Tactics
	}.
