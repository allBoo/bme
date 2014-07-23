%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% АПИ для участника поединка
%%% ====================================================================


-module(unit).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CAST(UnitId, Cmd), case is_pid(UnitId) of
								 true -> gen_server:cast(UnitId, Cmd);
								 false ->
									 case reg:find({unit, UnitId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UnitPid -> gen_server:cast(UnitPid, Cmd) end end).
-define(CALL(UnitId, Cmd), case is_pid(UnitId) of
								 true -> gen_server:call(UnitId, Cmd);
								 false ->
									 case reg:find({unit, UnitId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UnitPid -> gen_server:call(UnitPid, Cmd) end end).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).

%% getters and setters
-export([get_id/1,
		 get_pid/1,
		 get_user_pid/1,
		 get_user/1,
		 get/1,
		 get/2,
		 increase/2,
		 reduce/2,
		 is_alive/1]).

%%
-export([notify/2,
		 create_opponent_info/1,
		 set_opponents/2,
		 hit/3,
		 hited/2,

		 swap_done/2,
		 hit_done/2,
		 kill/1,
		 timeout_alarm/2,
		 crash/1]).

%% damage and heal
-export([magic_damage/2,
		 magic_damage/3,
		 got_damage/3,
		 hit_damage/3,
		 avoid_damage/3,
		 got_heal/2,
		 got_heal/3]).


%% start_link/1
%% ====================================================================
%% регистрирует процесс нового участника боя
start_link(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% get_id/1
%% ====================================================================
%% возвращает id юнита по его PID
get_id(UnitPid) when is_pid(UnitPid) ->
	reg:get({unit_id, UnitPid}).


%% get_pid/1
%% ====================================================================
%% возвращает pid юнита по его ID
get_pid(UnitId) when is_pid(UnitId) ->
	UnitId;

get_pid(UnitId) when is_integer(UnitId) ->
	reg:find({unit, UnitId}).


%% get_user_pid/1
%% ====================================================================
%% возвращает pid юзерстейта по ID юнита
get_user_pid(UnitId) ->
	reg:get({unit_user, UnitId}).


%% get_user/1
%% ====================================================================
%% возвращает юзерстейт по ID юнита
get_user(UnitId) ->
	user_state:get(get_user_pid(UnitId)).


%% get/1
%% ====================================================================
%% возвращает текущий State (информацию о юните)
get(UnitId) ->
	?CALL(UnitId, get).


%% get/2
%% ====================================================================
%% возвращает выбранный параметр юзера
get(UnitId, Part) when is_atom(Part) ->
	?CALL(UnitId, {get, Part}).


%% increase/2
%% ====================================================================
%% увеличение параметров юнита
increase(UnitId, Params) ->
	?CAST(UnitId, {increase, Params}).


%% reduce/2
%% ====================================================================
%% уменьшение параметров юнита
reduce(UnitId, Params) ->
	?CAST(UnitId, {reduce, Params}).


%% is_alive/1
%% ====================================================================
%% возвращает текущий state юнита
is_alive(Unit) ->
	?CALL(Unit, is_alive).


%% notify/2
%% ====================================================================
%% уведомление через эвент-менеджер юнита
notify(Unit, Message) when is_record(Unit, b_unit) ->
	notify(Unit#b_unit.id, Message);

notify(UnitId, Message) when is_integer(UnitId) ->
	reg:broadcast({unit, UnitId}, unit, Message).


%% create_opponent_info/1
%% ====================================================================
%% возвращает краткую информацию о юните
create_opponent_info(UnitId) ->
	?CALL(UnitId, create_opponent_info).


%% set_opponents/2
%% ====================================================================
%% устанавливает юниту список оппонентов
set_opponents(UnitId, OpponentsList) ->
	?CAST(UnitId, {set_opponents, OpponentsList}).


%% hit/3
%% ====================================================================
%% выставление удара противнику
hit(UnitId, Hits, Block) ->
	?CALL(UnitId, {hit, Hits, Block}).


%% hited/2
%% ====================================================================
%% противник выставил размен
hited(UnitPid, {From, Hit}) when is_pid(UnitPid), is_pid(From), is_pid(Hit) ->
	gen_server:cast(UnitPid, {hited, {From, Hit}}).


%% magic_damage/2, magic_damage/3
%% ====================================================================
%% получение урона магией
magic_damage(UnitId, MagicAttack) ->
	magic_damage(UnitId, MagicAttack, undefined).

magic_damage(UnitId, MagicAttack, TransactionId) ->
	?CAST(UnitId, {magic_damage, MagicAttack, TransactionId}).


%% got_damage/3
%% ====================================================================
%% получение урона
got_damage(UnitId, HitResult, TransactionId) ->
	?CALL(UnitId, {got_damage, HitResult, TransactionId}).


%% hit_damage/3
%% ====================================================================
%% нанесение урона
hit_damage(UnitId, HitResult, TransactionId) ->
	?CALL(UnitId, {hit_damage, HitResult, TransactionId}).


%% avoid_damage/3
%% ====================================================================
%% избегание урона
avoid_damage(UnitId, HitResult, TransactionId) ->
	?CALL(UnitId, {avoid_damage, HitResult, TransactionId}).


%% got_heal/3, got_heal/3
%% ====================================================================
%% хилл
got_heal(UnitId, Heal) when is_record(Heal, b_heal) ->
	?CAST(UnitId, {got_heal, Heal, undefined}).

got_heal(UnitId, Heal, TransactionId) when is_record(Heal, b_heal) ->
	?CAST(UnitId, {got_heal, Heal, TransactionId}).

%% swap_done/2
%% ====================================================================
%% уведомление о завершении хода(размена)
swap_done(UnitId, HitFrom) ->
	?CAST(UnitId, {swap_done, HitFrom}).


%% hit_done/2
%% ====================================================================
%% уведомление о завершении удара
hit_done(UnitId, HitFrom) ->
	?CAST(UnitId, {hit_done, HitFrom}).

%% kill/1
%% ====================================================================
%% мгновенное убийство юнита
kill(UnitId) ->
	?CALL(UnitId, kill).


%% timeout_alarm/2
%% ====================================================================
%% предупреждение о приближении таймаута от противника
timeout_alarm(UnitPid, OpponentPid) ->
	gen_server:cast(UnitPid, {timeout_alarm, OpponentPid}).


%% crash/1
%% ====================================================================
%% тестирование падения юнита
crash(_UnitId) ->
	buff_mgr:apply(1, #u_buff{id=multi_speedup}),

	buff_mgr:apply(2, #u_buff{id=krit_blindluck}),
	buff_mgr:apply(2, #u_buff{id=multi_doom}),
	buff_mgr:apply(2, #u_buff{id=multi_cowardshift}),

	unit:hit(1, [head, legs], paunch),
	unit:hit(2, [head, torso], torso),
	ok.
	%?CALL(UserId, crash).


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
init(Unit) when is_record(Unit, b_unit) ->
	?DBG("Start unit API server ~p~n", [Unit#b_unit.id]),
	process_flag(trap_exit, true),
	random:seed(now()),

	%% регистрируем имя сервера
	ok = reg:name([{unit, Unit#b_unit.battle_id, Unit#b_unit.team_id, Unit#b_unit.id},
				   {unit, Unit#b_unit.id},
				   {unit, Unit#b_unit.name}]),

	%% регистрируем теги для получения broadcast сообщений
	ok = reg:bind([{team_unit, Unit#b_unit.battle_id, Unit#b_unit.team_id},
				   {battle_unit, Unit#b_unit.battle_id},
				   {battle, Unit#b_unit.battle_id}]),

	%% регистрируем PID юзерстейта
	UserPid = user_state:get_pid(Unit#b_unit.id),
	reg:set({unit_user, Unit#b_unit.id}, UserPid),
	reg:set({unit_user, self()}, UserPid),

	%% регистрируем ID юнита
	reg:set({unit_id, self()}, Unit#b_unit.id),

	{ok, Unit#b_unit{pid = self(), user = UserPid}}.


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

%% возвращает краткую информацию об юните
handle_call(create_opponent_info, _, Unit) ->
	UserInfo = user_state:get(?userpid(Unit), info),
	{reply, #b_opponent{pid = self(),
                        team = Unit#b_unit.team_pid,
                        id   = Unit#b_unit.id,
                        name = Unit#b_unit.name,
                        ai   = Unit#b_unit.ai,
                        level = UserInfo#u_info.level,
                        align = UserInfo#u_info.align,
                        klan  = UserInfo#u_info.klan,
                        cost  = user_state:get(?userpid(Unit), 'dress.cost')
                        }, Unit};


%% возвращает PID юзерстейта
handle_call(get_user_pid, _, Unit) ->
	{reply, ?userpid(Unit), Unit};


%% возвращает PID юзерстейт
handle_call(get_user, _, Unit) ->
	{reply, ?user(Unit), Unit};


%% возвращает state юнита
handle_call(get, _, Unit) ->
	{reply, Unit, Unit};

handle_call({get, Part}, _, User) ->
	{reply, unit_state:get_attr(Part, User), User};


%% возвращает state юнита
handle_call(is_alive, _, Unit) ->
	{reply, Unit#b_unit.alive, Unit};


%% блокируем все остальные вызовы к убитому юниту
handle_call(_, _, Unit) when is_record(Unit, b_unit),
							 Unit#b_unit.alive == false ->
	{reply, ?ERROR_UNIT_DEAD, Unit};


%% выставление удара
handle_call({hit, HitsList, Block}, _, Unit) ->
	Opponent = Unit#b_unit.opponent,
	R = case is_record(Opponent, b_opponent) of
		false -> ?ERROR_WAIT_OPPONENT;
		true  ->
			Hit = create_hit(HitsList, Block, Unit),

			%% валидация
			case validate_hit(Hit, Unit) of
				ok ->
					%% если удар был выставлен противником
					case lists:keyfind(Opponent#b_opponent.pid, 1, Unit#b_unit.obtained) of
						{_OpponentPid, ObtainedHitPid} ->
							%% ответ на удар
							hit:reply(Unit#b_unit.battle_id, ObtainedHitPid, Hit);
						false ->
							%% выставляем новый размен
							hit:hit(Unit#b_unit.battle_id, Hit)
					end;
				Error when is_record(Error, error) ->
					Error
			end
	end,

	case R of
		%% если выставлен размен
		{ok, HitPid} when is_pid(HitPid) ->
			%% добавляем его в список разменов
			Hits = Unit#b_unit.hits ++ [{Opponent#b_opponent.pid, HitPid}],
			%% меняем противника
			{reply, {hit, HitPid}, select_next_opponent(Unit#b_unit{hits = Hits})};

		%% совершен размен ударами
		ok ->
			{reply, {ok, hit_replied}, select_next_opponent(Unit)};

		%% что-то пошло не так
		Erorr when is_record(Erorr, error) ->
			{reply, Erorr, Unit}
	end;


%% получение урона
handle_call({got_damage, HitResult0, From}, _, Unit) ->
	?DBG("Got Damage ~p~n", [HitResult0#b_hit_result.damage]),
	%% считаем что получилось
	HitResult = buff_mgr:on_before_got_damage(?unitid(Unit), HitResult0),
	Damage = case HitResult of
		H when is_record(H, b_hit_result)   -> H#b_hit_result.damage;
		M when is_record(M, b_magic_attack) -> M#b_magic_attack.damage
	end,
	?DBG("Got real Damage ~p~n", [Damage]),

	user_state:reduce(?userpid(Unit), [{'vitality.hp', Damage}]),
	DamagedUnit = Unit#b_unit{total_lost = Unit#b_unit.total_lost + Damage},

	%% пишем в лог получение урона
	DamagedUser = user_state:get(DamagedUnit#b_unit.user),
	case HitResult of
		H0 when is_record(H0, b_hit_result) ->
			Log = #log_hit{attacker = HitResult#b_hit_result.attacker, defendant = DamagedUnit#b_unit{user = DamagedUser}, hit_result = H0},
			battle_log:hit(Unit#b_unit.battle_id, From, Log);
		M0 when is_record(M0, b_magic_attack) ->
			Log = #log_magic{attacker = HitResult#b_magic_attack.attacker, defendant = DamagedUnit#b_unit{user = DamagedUser}, attack_result = M0},
			battle_log:magic(Unit#b_unit.battle_id, From, Log)
	end,

	%% полученный результат сообщаем атакующему, сколько он реально нанес и кому
	Attacker = case is_record(HitResult, b_hit_result) of
					true  -> HitResult#b_hit_result.attacker;
					false -> HitResult#b_magic_attack.attacker
			   end,
	case Attacker =:= self() of
		true  ->
			timer:apply_after(1, unit, hit_damage, [Attacker, HitResult, From]);
		false -> unit:hit_damage(Attacker, HitResult, From)
	end,

	%% ответный урон
	buff_mgr:on_after_got_damage(?unitid(Unit), HitResult),

	{reply, {ok, Damage},  DamagedUnit};


%% нанесение урона
handle_call({hit_damage, HitResult0, _From}, _, Unit) ->
	?DBG("Hit Damage ~p~n", [HitResult0#b_hit_result.damage]),

	HitResult = buff_mgr:on_hit_damage(?unitid(Unit), HitResult0),
	Damage = HitResult#b_hit_result.damage,

	%% считаем полученные тактики
	Attacker  = ?user(Unit),	%% получаем полный State юзера, он нужен для formula
	Defendant = ?puser(HitResult#b_hit_result.defendant),
	Tactics = #b_tactics{
					attack  = ?TACTIC(HitResult#b_hit_result.hited and not(HitResult#b_hit_result.crit), HitResult#b_hit_result.weapon_twain, 3),
					crit    = ?TACTIC(HitResult#b_hit_result.crit, HitResult#b_hit_result.weapon_twain or not(HitResult#b_hit_result.crit_break), 2),
					hearts  = formula:get_hearts(Damage, Attacker, Defendant)
			  },

	%% кол-во экспы за удар
	Exp = formula:get_exp_by_damage(Damage, Attacker, Defendant),

	%% обновляем стейт
	DamagedUnit = unit_state:increase(Unit, [{'tactics', Tactics},
											 {'total_damaged', Damage},
											 {'exp', Exp}]),

	{reply, {ok, Damage},  DamagedUnit};


%% избегание урона
handle_call({avoid_damage, HitResult0, From}, _, Unit) ->
	?DBG("Avoid Damage ~p~n", [HitResult0#b_hit_result.damage]),

	HitResult = buff_mgr:on_avoid_damage(?unitid(Unit), HitResult0),

	%% считаем полученные тактики
	Tactics = #b_tactics{
					counter = ?TACTIC(HitResult#b_hit_result.counter),
					block   = ?TACTIC((HitResult#b_hit_result.block or HitResult#b_hit_result.shield)
							  and not(HitResult#b_hit_result.hited), HitResult#b_hit_result.weapon_twain, 2),
					parry   = ?TACTIC(HitResult#b_hit_result.parry and not(HitResult#b_hit_result.hited))
			  },

	DamagedUnit = unit_state:increase(Unit, [{'tactics', Tactics}]),
	DamagedUser = user_state:get(DamagedUnit#b_unit.user),

	%% пишем в лог избегание урона
	Log = #log_miss{attacker = HitResult#b_hit_result.attacker, defendant = DamagedUnit#b_unit{user = DamagedUser}, hit_result = HitResult},
	battle_log:hit(Unit#b_unit.battle_id, From, Log),

	{reply, ok,  DamagedUnit};



%% мгновенное убийство юнита
handle_call(kill, _, Unit) ->
	?DBG("Kill unit ~p", [self()]),

	Hp = user_state:get(?userpid(Unit), 'vitality.hp'),
	user_state:reduce(?userpid(Unit), [{'vitality.hp', Hp}]),
	KilledUnit = Unit#b_unit{total_lost = Unit#b_unit.total_lost + Hp},

	{reply, {ok, killed}, killed(KilledUnit), hibernate};


%% test crash
handle_call(crash, _, Unit) ->
	%a = b,
	{reply, ?ERROR_WRONG_CALL, Unit, hibernate};

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


%% блокируем все остальные вызовы к убитому юниту
handle_cast(_, Unit) when is_record(Unit, b_unit),
						  Unit#b_unit.alive == false ->
	{noreply, Unit};


%% увеличение параметров юнита
handle_cast({increase, Params}, Unit) ->
	{noreply, unit_state:increase(Unit, Params)};


%% уменьшение параметров юнита
handle_cast({reduce, Params}, Unit) ->
	{noreply, unit_state:reduce(Unit, Params)};


%% устанавливает юниту список оппонентов
handle_cast({set_opponents, OpponentsList}, Unit) ->
	%% сравниваем стоимось комлектов и определяем серых в бою
	MyCost = user_state:get(?userpid(Unit), 'dress.cost'),
	CalculatedOpponentsList = lists:map(fun(Opponent) ->
												Delta = Opponent#b_opponent.cost / MyCost,
												Opponent#b_opponent{gray = Delta < 0.6, timeout = false}
										end, OpponentsList),
	%%?DBG("Unit ~p set opponents ~p~n", [self(), CalculatedOpponentsList]),
	{noreply, Unit#b_unit{opponents = CalculatedOpponentsList}};


%% противник выставил размен
%% сохраняем его в списке и мониторим
handle_cast({hited, {From, Hit}}, Unit) ->
	?DBG("Unit ~p hited", [{From, Hit}]),
	%% добавляем в список выставленных нам разменов
	Obtained = Unit#b_unit.obtained ++ [{From, Hit}],
	{noreply, Unit#b_unit{obtained = Obtained}};


%% обработка конца хода
%% уведомление о прошедшем размене, который выставили мы
handle_cast({swap_done, {sended, OpponentPid}}, Unit) ->
	?DBG("Sended hit is done ~p", [{hit, OpponentPid}]),
	%% удаляем размен из списка выставленных разменов
	Hits = lists:keydelete(OpponentPid, 1, Unit#b_unit.hits),
	{noreply, do_swap_done(OpponentPid, Unit#b_unit{hits = Hits})};

%% уведомление о прошедшем размене, который выставили нам
handle_cast({swap_done, {obtained, OpponentPid}}, Unit) ->
	?DBG("Obtained hit is done ~p", [{hit, OpponentPid}]),
	%% удаляем размен из списка полученных разменов
	Obtained = lists:keydelete(OpponentPid, 1, Unit#b_unit.obtained),
	{noreply, do_swap_done(OpponentPid, Unit#b_unit{obtained = Obtained})};


%% приближение пропуска хода по тайму
handle_cast({timeout_alarm, OpponentPid}, Unit) ->
	%% ищем противника в списке оппонентов
	Opponents = list_helper:keymap(OpponentPid,
								   2,
								   Unit#b_unit.opponents,
								   fun(Opponent) ->
										   Opponent#b_opponent{timeout = true}
								   end),
	?DBG("Timeout alert from ~p~n", [OpponentPid]),
	{noreply, Unit#b_unit{opponents = Opponents}};


%% получение урона магией
handle_cast({magic_damage, MagicAttack0, From}, Unit) ->
	?DBG("Got Magic Damage ~p~n", [MagicAttack0#b_magic_attack.damage]),
	%% считаем что получилось
	MagicAttack = buff_mgr:on_before_got_damage(?unitid(Unit), MagicAttack0),
	?DBG("Got real Magic Damage ~p~n", [MagicAttack#b_magic_attack.damage]),
	Damage = MagicAttack#b_magic_attack.damage,

	user_state:reduce(?userpid(Unit), [{'vitality.hp', Damage}]),
	DamagedUnit = unit_state:increase(Unit, [{'total_lost', Damage}]),
	DamagedUser = user_state:get(DamagedUnit#b_unit.user),

	%% пишем в лог получение урона
	Log = #log_magic{attacker = MagicAttack#b_magic_attack.attacker, defendant = DamagedUnit#b_unit{user = DamagedUser}, attack_result = MagicAttack},
	battle_log:magic(Unit#b_unit.battle_id, From, Log),

	buff_mgr:on_after_got_damage(?unitid(Unit), MagicAttack),

	%% @todo отложенная проверка на убийство
	{noreply, DamagedUnit};


%% получение хилла
handle_cast({got_heal, Heal0, TransactionId}, Unit) when ?spirit(Unit) > 0 ->
	?DBG("Got Heal ~p~n", [Heal0#b_heal.value]),

	%% считаем что получилось
	Heal = buff_mgr:on_before_got_heal(?unitid(Unit), Heal0),
	?DBG("Got real Heal ~p~n", [Heal#b_heal.value]),

	InitialHp = user_state:get(?userpid(Unit), 'vitality.hp'),
	user_state:increase(?userpid(Unit), [{'vitality.hp', Heal#b_heal.value}]),
	NewHp  = user_state:get(?userpid(Unit), 'vitality.hp'),
	Healed = min(NewHp - InitialHp, Heal#b_heal.value),	%% реально отхиленное

	Spirit = case Heal0#b_heal.use_spirit of
				 true  -> math:precision((Healed / user_state:get(?userpid(Unit), 'vitality.maxhp')) * 10, 2);
				 false -> 0
			 end,

	HealedUnit = unit_state:increase(Unit, [{'total_healed', Healed},
											{'tactics.spirit', -Spirit}]),
	HealedUser = user_state:get(HealedUnit#b_unit.user),

	%% пишем в лог получение хилла
	Log = #log_heal{recipient = HealedUnit#b_unit{user = HealedUser}, sender = Heal#b_heal.sender,
					value = Heal#b_heal.value, buff = Heal#b_heal.buff, trick = Heal#b_heal.trick},
	battle_log:heal(Unit#b_unit.battle_id, TransactionId, Log),

	{noreply, HealedUnit};

%% если духа нет, но хилл от баффа, пишем в лог отхилл в 0
handle_cast({got_heal, Heal, TransactionId}, Unit) when is_record(Heal#b_heal.buff, buff),
														?spirit(Unit) =< 0 ->
	HealedUser = user_state:get(Unit#b_unit.user),
	Log = #log_heal{recipient = Unit#b_unit{user = HealedUser}, sender = Heal#b_heal.sender, value = 0, buff = Heal#b_heal.buff, empty_spirit = true},
	battle_log:heal(Unit#b_unit.battle_id, TransactionId, Log),

	{noreply, Unit};

%% если нет духа и хилл получается от заклятья, то сразу возвращаем ошибку
handle_cast({got_heal, Heal, _TransactionId}, Unit) when is_record(Heal#b_heal.buff, spell),
														 ?spirit(Unit) =< 0 ->
	{noreply, Unit};


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

%% уведомление о запуске тимы
handle_info({team, {start, TeamPid}}, Unit) ->
	{noreply, Unit#b_unit{team_pid = TeamPid}};


%% уведомление о запуске боя
handle_info({battle, {start, BattlePid}}, Unit) ->
	%% формируем начальное состояние юнита
	BattleUnit = set_initial_unit_data(Unit),
	%% поиск подходящего оппонента
	Opponent = select_random_opponent(Unit#b_unit.opponents),
	?DBG("Select opponent ~p~n", [Opponent]),
	notify(Unit, {new_opponent, Opponent}),
	{noreply, BattleUnit#b_unit{battle_pid = BattlePid, opponent = Opponent}};


%% уведомление о убитом юните
handle_info({unit, {killed, UnitPid}}, Unit) when is_pid(UnitPid), UnitPid /= self() ->
	{noreply, unit_killed(UnitPid, Unit)};


%% уведомление о завершении поединка
handle_info({battle, {finish, Result}}, Unit) ->
	?DBG("Unit ~p got battle_finish message", [self()]),
	%% считаем кол-во полученной экспы
	IsWinner = Result#b_result.winner == Unit#b_unit.team_pid,
	Exp = case IsWinner of
			  true -> Unit#b_unit.exp * Result#b_result.exp_coef;
			  false -> 0
		  end,
	?DBG("Unit ~p damaged ~pHP and got ~p exp", [self(), Unit#b_unit.total_damaged, Exp]),
	%% сохраняем
	{noreply, Unit};


%% в любой непонятной ситтуации сохраняемся
handle_info({'EXIT', FromPid, Reason}, Unit) ->
	?DBG("Unit recieve exit signal ~p~n", [{FromPid, Reason}]),
	{noreply, Unit};


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
terminate(Reason, _Unit) ->
	?DBG("Unit ~p terminates with reason ~p~n", [self(), Reason]),
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

%% select_random_opponent/1
%% ====================================================================
%% автовыбор противника
select_random_opponent(OpponentsList) ->
	lists:nth(random:uniform(length(OpponentsList)), OpponentsList).


%% select_next_opponent/1
%% ====================================================================
%% выбор следующего оппонента
select_next_opponent(Unit) ->
	%% разворачиваем список оппонентов так, что начало списка будет указывать
	%% на хвост начиная с текущего оппонента, потом добавляем список из начала
	%% [1, 2, (3), 4, 5] -> [3, 4, 5, 1, 2]
	{Head, Tail} = lists:splitwith(fun(Opponent) ->
										   Opponent#b_opponent.pid /= Unit#b_unit.opponent
								   end, Unit#b_unit.opponents),
	%% выбираем из списка оппонентов тех, кому не поставлен размен
	BusyOpponents = lists:map(fun({OpponentPid, _HitPid}) -> OpponentPid end, Unit#b_unit.hits) ++
						[(Unit#b_unit.opponent)#b_opponent.pid],
	Opponents = lists:filter(fun(Opponent) ->
									 lists:member(Opponent#b_opponent.pid, BusyOpponents) == false
							 end, Tail ++ Head),
	%% если есть свободные оппоненты, выбираем первого из списка, если нет - то уходим в ожидание
	Opponent = case length(Opponents) > 0 of
				   true -> lists:nth(1, Opponents);
				   false -> undefined
			   end,
	?DBG("Select new opponent ~p~n", [Opponent]),
	notify(Unit, {new_opponent, Opponent}),
	Unit#b_unit{opponent = Opponent}.


%% set_initial_unit_data/1
%% ====================================================================
%% расчет начальных параметров
set_initial_unit_data(Unit) ->
	Level    = user_state:get(?userpid(Unit), 'info.level'),
	Vitality = user_state:get(?userpid(Unit), 'vitality'),
	Stats    = user_state:get(?userpid(Unit), 'stats'),

	%% расчет силы духа
	MaxSpirit = case Level of
					_Small when Level < 7 -> 0;
					7 -> 10;
					8 -> 20;
					9 -> 30;
					_Hight when Level > 9 -> 40
				end + Stats#u_stats.spir,
	Spirit = math:precision((Vitality#u_vitality.hp / Vitality#u_vitality.maxhp) * MaxSpirit, 2),
	unit_state:increase(Unit, [{'tactics.spirit', Spirit}]).


%% create_hit/3
%% ====================================================================
%% возвращает запись нового удара
create_hit(HitsList, Block, Unit) ->
	Opponent = Unit#b_unit.opponent,
	Blocks   = create_blocks_list(Block, Unit),
	#b_hit{sender    = self(),
		   recipient = Opponent#b_opponent.pid,
		   battle_id = Unit#b_unit.battle_id,
		   hits      = HitsList,
		   block     = Blocks,
		   timeout   = battle:get_timeout(Unit#b_unit.battle_pid)}.

create_blocks_list(Block, Unit) ->
	%% кол-во зон блока
	BlockPoints = user_state:get(?userpid(Unit), 'battle_spec.block_points'),
	list_helper:rsublist([head, torso, paunch, belt, legs], Block, BlockPoints).


%% validate_hit/2
%% ====================================================================
%% вылидация выставленного удара
validate_hit(Hit, Unit) when is_record(Hit, b_hit) ->
	ValidateResults = [
		%% проверяем правильность передачи параметров удара и блока
		%% и соответствие кол-ва ударов указанному в параметрах юнита
		validate_hits_count(Hit, Unit),
		validate_block_count(Hit, Unit),
		%% проверяем что удар еще не выставлен данному противнику
		validate_unique_hit(Hit, Unit)
	],
	case ValidateResults of
		[ok, ok, ok] ->
			ok;
		_ ->
			list_helper:first_nequal(ValidateResults, ok)
	end.

validate_hits_count(Hit, Unit) ->
	ExpectedHitsResult = lists:duplicate(user_state:get(?userpid(Unit), 'battle_spec.hit_points'), true),
	case [is_hit_valid(H) || H <- Hit#b_hit.hits] of
		ExpectedHitsResult ->
			ok;
		_ ->
			?ERROR_NOT_APPLICABLE
	end.

validate_block_count(Hit, Unit) ->
	ExpectedHitsResult = lists:duplicate(user_state:get(?userpid(Unit), 'battle_spec.block_points'), true),
	case [is_hit_valid(H) || H <- Hit#b_hit.block] of
		ExpectedHitsResult ->
			ok;
		_ ->
			?ERROR_NOT_APPLICABLE
	end.

is_hit_valid(Hit) ->
	lists:member(Hit, [head, torso, paunch, belt, legs]).

validate_unique_hit(_Hit, Unit) ->
	Opponent = Unit#b_unit.opponent,
	%% проверяем что удар еще не выставлен данному противнику
	%% вообще такого быть не должно
	case lists:keyfind(Opponent#b_opponent.pid, 1, Unit#b_unit.hits) of
		true ->
			?LOG("Try hit already hited ~p~n", [Unit]),
			?ERROR_TOO_FAST;
		false ->
			ok
	end.


%% do_swap_done/2
%% ====================================================================
%% обработка конца размена
do_swap_done(OpponentPid, Unit) ->
	%buff_mgr:notify(?unitid(Unit), swap_done),
	notify(Unit, swap_done),

	%% если у противника стоит флаг таймаута - сбросим его
	Opponents = list_helper:keymap(OpponentPid,
								   2,
								   Unit#b_unit.opponents,
								   fun(Opponent) ->
										   Opponent#b_opponent{timeout = false}
								   end),

	%% если текущий противник не выбран - выбираем этого, с кем произведен размен
	NewOpponent = case Unit#b_unit.opponent of
					  undefined ->
						  case lists:keyfind(OpponentPid, 2, Opponents) of
							  false    -> undefined;
							  Opponent ->
								  ?DBG("Select opponent ~p~n", [Opponent]),
								  notify(Unit, {new_opponent, Opponent}),
								  Opponent
						  end;
					  _ -> Unit#b_unit.opponent
				  end,
	%% @todo сбросить лок с приемов, увеличить счетчик ходов, etc
	Unit0 = Unit#b_unit{opponent = NewOpponent, opponents = Opponents},

	%% если уровень ХП = 0, значит юнит помер
	case user_state:get(?userpid(Unit), 'vitality.hp') > 0 of
		true  -> Unit0;
		false -> killed(Unit0)
	end.


%% killed/1
%% ====================================================================
%% данный юнит убит
killed(Unit) ->
	%% @todo обработка магии спасения
	%% отключаем получение broadcast сообщений
	reg:unbind([{team_unit, Unit#b_unit.battle_id, Unit#b_unit.team_id},
				{battle_unit, Unit#b_unit.battle_id},
				{battle, Unit#b_unit.battle_id}]),

	%% уведомляем всех юнитов и подписчиков, что юнит убит
	Message = {killed, self()},
	broadcast(Unit, Message),
	notify(Unit, Message),
	%% удаляем все выставленные и назначенные размены
	[hit:cancel(HitPid) || {_, HitPid} <- Unit#b_unit.hits ++ Unit#b_unit.obtained],
	%% очищаем списки разменов, противников
	KilledUnit = Unit#b_unit{alive = false,
							 opponents = [],
							 opponent  = undefined,
							 obtained  = [],
							 hits      = []},
	KilledUser = user_state:get(KilledUnit#b_unit.user),
	%% @todo записываем убийство на счет оппонента
	?DBG("Unit ~p killed", [self()]),

	%% записываем в лог
	battle_log:killed(KilledUnit#b_unit.battle_id, battle_log:unit(KilledUnit#b_unit{user = KilledUser})),

	KilledUnit.


%% unit_killed/1
%% ====================================================================
%% обработка уведомлений об убитых юнитах
unit_killed(UnitPid, Unit) ->
	%%?DBG("Unit ~p check killed unit ~p~n", [self(), UnitPid]),
	%% убираем его из списка оппонентов, союзников и разменов
	Unit0 = Unit#b_unit{opponents = lists:keydelete(UnitPid, 2, Unit#b_unit.opponents),
						hits      = lists:keydelete(UnitPid, 1, Unit#b_unit.hits),
						obtained  = lists:keydelete(UnitPid, 1, Unit#b_unit.obtained)},

	%% если выбран оппонентом - меняем противника
	case Unit0#b_unit.opponent of
		Opponent when is_record(Opponent, b_opponent),
					  Opponent#b_opponent.pid == UnitPid ->
			select_next_opponent(Unit0);
		_ -> Unit0
	end.
%% 	case (Unit0#b_unit.opponent)#b_opponent.pid == UnitPid of
%% 		true  -> select_next_opponent(Unit0);
%% 		false -> Unit0
%% 	end.



%% broadcast/2
%% ====================================================================
%% уведомление всех участников боя
broadcast(Unit, Message) ->
	reg:broadcast({battle, Unit#b_unit.battle_id}, unit, Message).

