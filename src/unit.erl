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

-define(CAST(UserId, Cmd), case is_pid(UserId) of
								 true -> gen_server:cast(UserId, Cmd);
								 false ->
									 case reg:find({unit, UserId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UnitPid -> gen_server:cast(UnitPid, Cmd) end end).
-define(CALL(UserId, Cmd), case is_pid(UserId) of
								 true -> gen_server:call(UserId, Cmd);
								 false ->
									 case reg:find({unit, UserId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UnitPid -> gen_server:call(UnitPid, Cmd) end end).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 notify/2,
		 get_state/1,
		 create_opponent_info/1,
		 set_opponents/2,
		 hit/3,
		 hited/2,
		 got_damage/4,
		 hit_damage/4,
		 avoid_damage/4,
		 damage/2,
		 swap_done/2,
		 kill/1,
		 timeout_alarm/2,
		 increase_state/2,
		 reduce_state/2,
		 change_state/3,
		 crash/1]).

%% start_link/1
%% ====================================================================
%% регистрирует процесс нового участника боя
start_link(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% notify/2
%% ====================================================================
%% уведомление через эвент-менеджер юнита
notify(Unit, Message) when is_record(Unit, b_unit) ->
	notify(Unit#b_unit.id, Message);

notify(UnitId, Message) when is_integer(UnitId) ->
	reg:broadcast({unit, UnitId}, unit, Message).


%% get_state/1
%% ====================================================================
%% возвращает текущий State (информацию о юните)
get_state(UserId) ->
	?CALL(UserId, get_state).


%% create_opponent_info/1
%% ====================================================================
%% возвращает краткую информацию о юните
create_opponent_info(UserId) ->
	?CALL(UserId, create_opponent_info).


%% set_opponents/2
%% ====================================================================
%% устанавливает юниту список оппонентов
set_opponents(UserId, OpponentsList) ->
	?CAST(UserId, {set_opponents, OpponentsList}).


%% hit/3
%% ====================================================================
%% выставление удара противнику
hit(UserId, Hits, Block) ->
	?CALL(UserId, {hit, Hits, Block}).


%% hited/2
%% ====================================================================
%% противник выставил размен
hited(UnitPid, {From, Hit}) when is_pid(UnitPid), is_pid(From), is_pid(Hit) ->
	gen_server:cast(UnitPid, {hited, {From, Hit}}).


%% damage/2
%% ====================================================================
%% нанесение урона юниту
damage(UserId, Damage) when is_record(Damage, b_damage) ->
	?CALL(UserId, {damage, Damage}).


%% got_damage/4
%% ====================================================================
%% получение урона
got_damage(UserId, Attacker, HitResult, From) ->
	?CALL(UserId, {got_damage, Attacker, HitResult, From}).


%% hit_damage/4
%% ====================================================================
%% нанесение урона
hit_damage(UserId, Defendant, HitResult, From) ->
	?CALL(UserId, {hit_damage, Defendant, HitResult, From}).


%% avoid_damage/4
%% ====================================================================
%% избегание урона
avoid_damage(UserId, Attacker, HitResult, From) ->
	?CALL(UserId, {avoid_damage, Attacker, HitResult, From}).


%% swap_done/2
%% ====================================================================
%% уведомление о завершении размена
swap_done(UserId, HitFrom) ->
	?CAST(UserId, {swap_done, HitFrom}).


%% kill/1
%% ====================================================================
%% мгновенное убийство юнита
kill(UserId) ->
	?CALL(UserId, kill).


%% timeout_alarm/2
%% ====================================================================
%% предупреждение о приближении таймаута от противника
timeout_alarm(UnitPid, OpponentPid) ->
	gen_server:cast(UnitPid, {timeout_alarm, OpponentPid}).


%% increase_state/2
%% ====================================================================
%% увеличение параметров юнита
increase_state(UserId, Params) ->
	?CAST(UserId, {increase_state, Params}).


%% reduce_state/2
%% ====================================================================
%% уменьшение параметров юнита
reduce_state(UserId, Params) ->
	?CAST(UserId, {reduce_state, Params}).


%% crash/1
%% ====================================================================
%% тестирование падения юнита
crash(UserId) ->
	?CALL(UserId, crash).


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

	{ok, Unit}.


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
	{reply, #b_opponent{pid = self(),
                        team = Unit#b_unit.team_pid,
                        id   = Unit#b_unit.id,
                        name = Unit#b_unit.name,
                        ai   = Unit#b_unit.ai,
                        level = ((Unit#b_unit.user)#user.info)#u_info.level,
                        align = ((Unit#b_unit.user)#user.info)#u_info.align,
                        klan  = ((Unit#b_unit.user)#user.info)#u_info.klan,
                        cost  = ((Unit#b_unit.user)#user.dress)#u_dress.cost
                        }, Unit};


%% возвращает текущее состояние юнита
handle_call(get_state, _, Unit) ->
	{reply, Unit, Unit};


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
handle_call({got_damage, Attacker, HitResult, From}, _, Unit) ->
	?DBG("Got Damage ~p~n", [HitResult]),
	%% считаем что получилось
	%% @todo обработка приемов на снижение урона (призрачки, щиты и тд)
	Damage   = HitResult#b_hit_result.damage,

	User     = ?user(Unit),
	Vitality = ?vitality(User),
	Hp       = math:limit(?hp(User) - Damage, ?maxhp(User)),
	Mana     = math:limit(?mana(User) - 0, ?maxmana(User)),

	DamagedUnit = Unit#b_unit{user = User#user{vitality = Vitality#u_vitality{hp = Hp, mana = Mana}},
							  %total_damaged = Unit#b_unit.total_damaged + Damage#b_damage.damaged,
							  %total_healed  = Unit#b_unit.total_healed  + Damage#b_damage.healed,
							  total_lost    = Unit#b_unit.total_lost    + Damage},

	%% пишем в лог получение урона
	Log = #log_hit{attacker = ?log_unit(Attacker), defendant = ?log_unit(DamagedUnit),
				   hit_result = HitResult},
	battle_log:hit(Unit#b_unit.battle_id, From, Log),
	%battle_log:damage(DamagedUnit#b_unit.battle_id, ?log_unit(DamagedUnit)),

	{reply, {ok, Damage},  DamagedUnit};


%% нанесение урона
handle_call({hit_damage, Defendant, HitResult, _From}, _, Unit) ->
	?DBG("Hit Damage ~p~n", [HitResult]),

	Damage = HitResult#b_hit_result.damage,

	%% считаем полученные тактики
	Tactics = add_tactics(Unit, #b_tactics{
			attack  = ?TACTIC(HitResult#b_hit_result.hited and not(HitResult#b_hit_result.crit), HitResult#b_hit_result.weapon_twain, 3),
			crit    = ?TACTIC(HitResult#b_hit_result.crit, HitResult#b_hit_result.weapon_twain or not(HitResult#b_hit_result.crit_break), 2),
			hearts  = formula:get_hearts(Damage, ?user(Unit), ?user(Defendant))
		}),

	%% кол-во экспы за удар
	Exp = formula:get_exp_by_damage(Damage, ?user(Unit), ?user(Defendant)),

	DamagedUnit = Unit#b_unit{total_damaged = Unit#b_unit.total_damaged + Damage,
							  exp = Unit#b_unit.exp + Exp,
							  tactics = Tactics},

	{reply, {ok, Damage},  DamagedUnit};


%% избегание урона
handle_call({avoid_damage, Attacker, HitResult, From}, _, Unit) ->
	?DBG("Avoid Damage ~p~n", [HitResult]),

	%% считаем полученные тактики
	Tactics = add_tactics(Unit, #b_tactics{
			counter = ?TACTIC(HitResult#b_hit_result.counter),
			block   = ?TACTIC((HitResult#b_hit_result.block or HitResult#b_hit_result.shield)
					  and not(HitResult#b_hit_result.hited), HitResult#b_hit_result.weapon_twain, 2),
			parry   = ?TACTIC(HitResult#b_hit_result.parry and not(HitResult#b_hit_result.hited))
		}),

	DamagedUnit = Unit#b_unit{tactics = Tactics},

	%% пишем в лог избегание урона
	Log = #log_miss{attacker = ?log_unit(Attacker), defendant = ?log_unit(DamagedUnit),
				    hit_result = HitResult},
	battle_log:hit(Unit#b_unit.battle_id, From, Log),

	{reply, ok,  DamagedUnit};


%% получение урона
%% @deprecated
handle_call({damage, Damage}, _, Unit) when is_record(Damage, b_damage) ->
	?DBG("Damage omitted ~p~n", [Damage]),
	%% считаем что получилось
	User = Unit#b_unit.user,
	Vitality = User#user.vitality,
	Hp = math:limit(Vitality#u_vitality.hp - Damage#b_damage.lost, Vitality#u_vitality.maxhp),
	Mana = math:limit(Vitality#u_vitality.mana - Damage#b_damage.lost_mana, Vitality#u_vitality.maxmana),
	Tactics = add_tactics(Unit, Damage#b_damage.tactics),
	DamagedUnit = Unit#b_unit{user = User#user{vitality = Vitality#u_vitality{hp = Hp, mana = Mana}},
							  tactics = Tactics,
							  total_damaged = Unit#b_unit.total_damaged + Damage#b_damage.damaged,
							  total_healed  = Unit#b_unit.total_healed  + Damage#b_damage.healed,
							  total_lost    = Unit#b_unit.total_lost    + Damage#b_damage.lost,
							  exp = Unit#b_unit.exp + Damage#b_damage.exp},
	?DBG("Damage result ~p~n", [{Hp, Mana, Tactics}]),
	battle_log:damage(DamagedUnit#b_unit.battle_id, ?log_unit(DamagedUnit)),
	%% если уровень ХП = 0, значит юнит помер
	case Hp > 0 of
		true  -> {reply, {ok, alive},  DamagedUnit};
		false -> {reply, {ok, killed}, killed(DamagedUnit), hibernate}
	end;


%% мгновенное убийство юнита
handle_call(kill, _, Unit) ->
	?DBG("Kill unit ~p", [self()]),
	User = Unit#b_unit.user,
	Vitality = User#user.vitality,
	KilledUnit = Unit#b_unit{user = User#user{vitality = Vitality#u_vitality{hp = 0}},
							 total_lost = Unit#b_unit.total_lost + Vitality#u_vitality.hp},

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

%% увеличение параметров юнита
handle_cast({increase_state, Params}, Unit) ->
	{noreply, change_state(increase, Unit, Params)};


%% уменьшение параметров юнита
handle_cast({reduce_state, Params}, Unit) ->
	{noreply, change_state(reduce, Unit, Params)};


%% блокируем все остальные вызовы к убитому юниту
handle_cast(_, Unit) when is_record(Unit, b_unit),
						  Unit#b_unit.alive == false ->
	{noreply, Unit};


%% устанавливает юниту список оппонентов
handle_cast({set_opponents, OpponentsList}, Unit) ->
	%% сравниваем стоимось комлектов и определяем серых в бою
	CalculatedOpponentsList = lists:map(fun(Opponent) ->
												Delta = Opponent#b_opponent.cost / ((Unit#b_unit.user)#user.dress)#u_dress.cost,
												Opponent#b_opponent{gray = Delta < 0.6, timeout = false}
										end, OpponentsList),
	%%?DBG("Unit ~p set opponents ~p~n", [self(), CalculatedOpponentsList]),
	{noreply, Unit#b_unit{opponents = CalculatedOpponentsList}};


%% противник выставил размен
%% сохраняем его в списке и мониторим
handle_cast({hited, {From, Hit}}, Unit) ->
	?DBG("User ~p hited", [{From, Hit}]),
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
	User = Unit#b_unit.user,
	Level = (User#user.info)#u_info.level,
	Vitality = User#user.vitality,

	%% расчет силы духа
	MaxSpirit = case Level of
					_Small when Level < 7 -> 0;
					7 -> 10;
					8 -> 20;
					9 -> 30;
					_Hight when Level > 9 -> 40
				end + (User#user.stats)#u_stats.spir,
	Spirit = math:precision((Vitality#u_vitality.hp / Vitality#u_vitality.maxhp) * MaxSpirit, 2),

	Unit#b_unit{tactics  = #b_tactics{spirit = Spirit},
				alive    = true,
				opponent = undefined,
				obtained = [],
				hits     = []}.


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
	BlockPoints = ((Unit#b_unit.user)#user.battle_spec)#u_battle_spec.block_points,
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
	ExpectedHitsResult = lists:duplicate(((Unit#b_unit.user)#user.battle_spec)#u_battle_spec.hit_points, true),
	case [is_hit_valid(H) || H <- Hit#b_hit.hits] of
		ExpectedHitsResult ->
			ok;
		_ ->
			?ERROR_NOT_APPLICABLE
	end.

validate_block_count(Hit, Unit) ->
	ExpectedHitsResult = lists:duplicate(((Unit#b_unit.user)#user.battle_spec)#u_battle_spec.block_points, true),
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


%% add_tactics/2
%% ====================================================================
%% пересчет тактик
add_tactics(Unit, Delta) when is_record(Unit, b_unit),
							  is_record(Delta, b_tactics) ->
	Current = Unit#b_unit.tactics,
	Current#b_tactics{attack  = math:limit(Current#b_tactics.attack + Delta#b_tactics.attack, 25),
                      crit    = math:limit(Current#b_tactics.crit + Delta#b_tactics.crit, 25),
                      counter = math:limit(Current#b_tactics.counter + Delta#b_tactics.counter, 25),
                      block   = math:limit(Current#b_tactics.block + Delta#b_tactics.block, 25),
                      parry   = math:limit(Current#b_tactics.parry + Delta#b_tactics.parry, 25),
                      hearts  = math:limit(math:precision(Current#b_tactics.hearts + Delta#b_tactics.hearts, 2), 25),
                      spirit  = math:limit(math:precision(Current#b_tactics.spirit + Delta#b_tactics.spirit, 2), Current#b_tactics.spirit)}.


%% do_swap_done/2
%% ====================================================================
%% обработка конца размена
do_swap_done(OpponentPid, Unit) ->
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
	case ?hp(?user(Unit0)) > 0 of
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
	%% @todo записываем убийство на счет оппонента
	?DBG("Unit ~p killed", [self()]),

	%% записываем в лог
	battle_log:killed(KilledUnit#b_unit.battle_id, ?log_unit(KilledUnit)),

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


%% change_state/3
%% ====================================================================
%% изменение параметров юнита
change_state(_, Unit, []) ->
	Unit;

change_state(increase, Unit, [HParam | TParams]) ->
	Unit0 = change_state(Unit, HParam),
	change_state(increase, Unit0, TParams);

change_state(reduce, Unit, [{State, Delta} | TParams]) ->
	Unit0 = change_state(Unit, {State, -Delta}),
	change_state(reduce, Unit0, TParams).


change_state(Unit, {State, Delta}) when is_atom(State),
										is_record(Unit, b_unit) ->
	SState = erlang:atom_to_list(State),
	Unit0 = change_state(Unit, {SState, Delta}),
	notify(Unit0#b_unit.id, {change_state, {State, Delta}}),
	Unit0;

%% изменение внутреннего параметра User
change_state(Unit, {"user." ++ Part, Delta}) when is_record(Unit, b_unit) ->
	Unit#b_unit{user = change_state(Unit#b_unit.user, {Part, Delta})};

%% изменение мощности урона
change_state(User, {"dpower." ++ Part, Delta}) when is_record(User, user) ->
	User#user{dpower = change_state(User#user.dpower, {Part, Delta})};
change_state(DPower, {"general", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{general = change_state(DPower#u_dpower.general, Delta)};
change_state(DPower, {"prick", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{prick = change_state(DPower#u_dpower.prick, Delta)};
change_state(DPower, {"chop", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{chop = change_state(DPower#u_dpower.chop, Delta)};
change_state(DPower, {"crush", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{crush = change_state(DPower#u_dpower.crush, Delta)};
change_state(DPower, {"cut", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{cut = change_state(DPower#u_dpower.cut, Delta)};
change_state(DPower, {"crit", Delta}) when is_record(DPower, u_dpower) ->
	DPower#u_dpower{crit = change_state(DPower#u_dpower.crit, Delta)};

%% изменение базового урона
change_state(User, {"damage." ++ Part, Delta}) when is_record(User, user) ->
	User#user{damage = change_state(User#user.damage, {Part, Delta})};
change_state(Damage, {"base." ++ Part, Delta}) when is_record(Damage, u_damage) ->
	Damage#u_damage{base = change_state(Damage#u_damage.base, {Part, Delta})};


%% изменение защиты от оружия
change_state(User, {"dprotection", Delta}) when is_record(User, user) ->
	Zones = ["head", "torso", "belt", "legs"],
	Types = ["general", "prick", "chop", "crush", "cut"],

	lists:foldl(
		fun(Zone, UserAcc0) ->
			lists:foldl(
			  fun(Type, UserAcc1) ->
					  change_state(UserAcc1, {"dprotection." ++ Zone ++ "." ++ Type, Delta})
			  end,
			  UserAcc0, Types)
		end,
		User, Zones);

change_state(User, {"dprotection." ++ Part, Delta}) when is_record(User, user) ->
	User#user{dprotection = change_state(User#user.dprotection, {Part, Delta})};

change_state(DProtection, {"head." ++ Part, Delta}) when is_record(DProtection, u_dprotection) ->
	DProtection#u_dprotection{head = change_state(DProtection#u_dprotection.head, {Part, Delta})};
change_state(DProtection, {"torso." ++ Part, Delta}) when is_record(DProtection, u_dprotection) ->
	DProtection#u_dprotection{torso = change_state(DProtection#u_dprotection.torso, {Part, Delta})};
change_state(DProtection, {"belt." ++ Part, Delta}) when is_record(DProtection, u_dprotection) ->
	DProtection#u_dprotection{belt = change_state(DProtection#u_dprotection.belt, {Part, Delta})};
change_state(DProtection, {"legs." ++ Part, Delta}) when is_record(DProtection, u_dprotection) ->
	DProtection#u_dprotection{legs = change_state(DProtection#u_dprotection.legs, {Part, Delta})};

change_state(ZoneDProtection, {"general", Delta}) when is_record(ZoneDProtection, u_dprotection_zone) ->
	ZoneDProtection#u_dprotection_zone{general = change_state(ZoneDProtection#u_dprotection_zone.general, Delta)};
change_state(ZoneDProtection, {"prick", Delta}) when is_record(ZoneDProtection, u_dprotection_zone) ->
	ZoneDProtection#u_dprotection_zone{prick = change_state(ZoneDProtection#u_dprotection_zone.prick, Delta)};
change_state(ZoneDProtection, {"chop", Delta}) when is_record(ZoneDProtection, u_dprotection_zone) ->
	ZoneDProtection#u_dprotection_zone{chop = change_state(ZoneDProtection#u_dprotection_zone.chop, Delta)};
change_state(ZoneDProtection, {"crush", Delta}) when is_record(ZoneDProtection, u_dprotection_zone) ->
	ZoneDProtection#u_dprotection_zone{crush = change_state(ZoneDProtection#u_dprotection_zone.crush, Delta)};
change_state(ZoneDProtection, {"cut", Delta}) when is_record(ZoneDProtection, u_dprotection_zone) ->
	ZoneDProtection#u_dprotection_zone{cut = change_state(ZoneDProtection#u_dprotection_zone.cut, Delta)};

%% изменение защиты от магии
change_state(User, {"wprotection", Delta}) when is_record(User, user) ->
	Types = ["general", "air", "fire", "water", "earth", "light", "dark", "gray"],
	lists:foldl(
		fun(Type, UserAcc0) ->
			change_state(UserAcc0, {"wprotection." ++ Type, Delta})
		end,
		User, Types);

change_state(User, {"wprotection." ++ Part, Delta}) when is_record(User, user) ->
	User#user{wprotection = change_state(User#user.wprotection, {Part, Delta})};

change_state(TypeWProtection, {"general", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{general = change_state(TypeWProtection#u_wprotection.general, Delta)};
change_state(TypeWProtection, {"air", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{air = change_state(TypeWProtection#u_wprotection.air, Delta)};
change_state(TypeWProtection, {"fire", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{fire = change_state(TypeWProtection#u_wprotection.fire, Delta)};
change_state(TypeWProtection, {"water", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{water = change_state(TypeWProtection#u_wprotection.water, Delta)};
change_state(TypeWProtection, {"earth", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{earth = change_state(TypeWProtection#u_wprotection.earth, Delta)};
change_state(TypeWProtection, {"light", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{light = change_state(TypeWProtection#u_wprotection.light, Delta)};
change_state(TypeWProtection, {"dark", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{dark = change_state(TypeWProtection#u_wprotection.dark, Delta)};
change_state(TypeWProtection, {"gray", Delta}) when is_record(TypeWProtection, u_wprotection) ->
	TypeWProtection#u_wprotection{gray = change_state(TypeWProtection#u_wprotection.gray, Delta)};

%% изменение мощности магии
change_state(User, {"wpower", Delta}) when is_record(User, user) ->
	Types = ["general", "air", "fire", "water", "earth", "light", "dark", "gray"],
	lists:foldl(
		fun(Type, UserAcc0) ->
			change_state(UserAcc0, {"wpower." ++ Type, Delta})
		end,
		User, Types);

change_state(User, {"wpower." ++ Part, Delta}) when is_record(User, user) ->
	User#user{wpower = change_state(User#user.wpower, {Part, Delta})};

change_state(TypeWPower, {"general", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{general = change_state(TypeWPower#u_wpower.general, Delta)};
change_state(TypeWPower, {"air", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{air = change_state(TypeWPower#u_wpower.air, Delta)};
change_state(TypeWPower, {"fire", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{fire = change_state(TypeWPower#u_wpower.fire, Delta)};
change_state(TypeWPower, {"water", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{water = change_state(TypeWPower#u_wpower.water, Delta)};
change_state(TypeWPower, {"earth", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{earth = change_state(TypeWPower#u_wpower.earth, Delta)};
change_state(TypeWPower, {"light", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{light = change_state(TypeWPower#u_wpower.light, Delta)};
change_state(TypeWPower, {"dark", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{dark = change_state(TypeWPower#u_wpower.dark, Delta)};
change_state(TypeWPower, {"gray", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{gray = change_state(TypeWPower#u_wpower.gray, Delta)};

%% изменение живучести
change_state(User, {"vitality." ++ Part, Delta}) when is_record(User, user) ->
	User#user{vitality = change_state(User#user.vitality, {Part, Delta})};
change_state(Vitality, {"hp", Delta}) when is_record(Vitality, u_vitality) ->
	Vitality#u_vitality{hp = change_state(Vitality#u_vitality.hp, Delta)};
change_state(Vitality, {"maxhp", Delta}) when is_record(Vitality, u_vitality) ->
	MaxHP = max(change_state(Vitality#u_vitality.maxhp, Delta), 0),
	case Vitality#u_vitality.hp > MaxHP of
		true  ->  change_state(Vitality#u_vitality{maxhp = MaxHP}, {"hp", MaxHP - Vitality#u_vitality.hp});
		false -> Vitality#u_vitality{maxhp = MaxHP}
	end;
change_state(Vitality, {"mana", Delta}) when is_record(Vitality, u_vitality) ->
	Vitality#u_vitality{mana = change_state(Vitality#u_vitality.mana, Delta)};
change_state(Vitality, {"maxmana", Delta}) when is_record(Vitality, u_vitality) ->
	MaxMana = max(change_state(Vitality#u_vitality.maxmana, Delta), 0),
	case Vitality#u_vitality.mana > MaxMana of
		true  ->  change_state(Vitality#u_vitality{maxmana = MaxMana}, {"mana", MaxMana - Vitality#u_vitality.mana});
		false -> Vitality#u_vitality{maxmana = MaxMana}
	end;

%% изменение статов
change_state(User, {"stats." ++ Part, Delta}) when is_record(User, user) ->
	User0 = User#user{stats = change_state(User#user.stats, {Part, Delta})},
	on_stats_changed(User0, {Part, Delta});
change_state(Stats, {"str", Delta}) when is_record(Stats, u_stats) ->
	Stats#u_stats{str = change_state(Stats#u_stats.str, Delta)};
change_state(Stats, {"agil", Delta}) when is_record(Stats, u_stats) ->
	%% @todo дополнительное изменение мф уворота и антиуворота
	Stats#u_stats{agil = change_state(Stats#u_stats.agil, Delta)};
change_state(Stats, {"int", Delta}) when is_record(Stats, u_stats) ->
	%% @todo дополнительное изменение мф крита и антикрита
	Stats#u_stats{int = change_state(Stats#u_stats.int, Delta)};
change_state(Stats, {"dex", Delta}) when is_record(Stats, u_stats) ->
	%% @todo дополнительное изменение защиты от урона и магии
	Stats#u_stats{dex = change_state(Stats#u_stats.dex, Delta)};
change_state(Stats, {"intel", Delta}) when is_record(Stats, u_stats) ->
	%% @todo дополнительное изменение мф мощности магии
	Stats#u_stats{intel = change_state(Stats#u_stats.intel, Delta)};
change_state(Stats, {"wisd", Delta}) when is_record(Stats, u_stats) ->
	%% @todo изменить уровень маны
	Stats#u_stats{wisd = change_state(Stats#u_stats.wisd, Delta)};
change_state(Stats, {"spir", Delta}) when is_record(Stats, u_stats) ->
	%% @todo изменить уровень духа
	Stats#u_stats{spir = change_state(Stats#u_stats.spir, Delta)};

%% изменение МФ
change_state(User, {"mfs." ++ Part, Delta}) when is_record(User, user) ->
	User#user{mfs = change_state(User#user.mfs, {Part, Delta})};
change_state(Mfs, {"crit", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{crit = max(change_state(Mfs#u_mf.crit, Delta), 0)};
change_state(Mfs, {"ucrit", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{ucrit = max(change_state(Mfs#u_mf.ucrit, Delta), 0)};
change_state(Mfs, {"aucrit", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{aucrit = max(change_state(Mfs#u_mf.aucrit, Delta), 0)};
change_state(Mfs, {"acrit", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{acrit = max(change_state(Mfs#u_mf.acrit, Delta), 0)};
change_state(Mfs, {"dodge", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{dodge = max(change_state(Mfs#u_mf.dodge, Delta), 0)};
change_state(Mfs, {"udodge", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{udodge = max(change_state(Mfs#u_mf.udodge, Delta), 0)};
change_state(Mfs, {"audodge", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{audodge = max(change_state(Mfs#u_mf.audodge, Delta), 0)};
change_state(Mfs, {"adodge", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{adodge = max(change_state(Mfs#u_mf.adodge, Delta), 0)};
change_state(Mfs, {"counter", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{counter = max(change_state(Mfs#u_mf.counter, Delta), 0)};
change_state(Mfs, {"parry", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{parry = max(change_state(Mfs#u_mf.parry, Delta), 0)};
change_state(Mfs, {"block", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{block = max(change_state(Mfs#u_mf.block, Delta), 0)};
change_state(Mfs, {"luck", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{luck = max(change_state(Mfs#u_mf.luck, Delta), 0)};


%% изменение d-value
change_state(DValue, {"n", Delta}) when is_record(DValue, d_value) ->
	DValue#d_value{n = change_state(DValue#d_value.n, Delta)};
change_state(DValue, {"k", Delta}) when is_record(DValue, d_value) ->
	DValue#d_value{k = change_state(DValue#d_value.k, Delta)};

%% изменение числового параметра
change_state(Param, Delta) when is_number(Param),
								is_number(Delta) ->
	Param + Delta.

%% вызывается при изменении статов юзера
%% пересчитывает различные МФ, зависящие от статов

%% пересчет уворота и антиуворота от изменения ловкости
on_stats_changed(User, {"agil", Delta}) when is_record(User, user) ->
	Mfs = (User#user.mfs)#u_mf{
		dodge  = max(change_state((User#user.mfs)#u_mf.dodge, formula:get_dodge_by_agile(Delta)), 0),
		udodge = max(change_state((User#user.mfs)#u_mf.udodge, formula:get_udodge_by_agile(Delta)), 0)
	},
	User#user{mfs = Mfs};
%% пересчет крита и антикрита от изменения интуиции
on_stats_changed(User, {"int", Delta}) when is_record(User, user) ->
	Mfs = (User#user.mfs)#u_mf{
		crit  = max(change_state((User#user.mfs)#u_mf.crit, formula:get_crit_by_intuition(Delta)), 0),
		ucrit = max(change_state((User#user.mfs)#u_mf.ucrit, formula:get_ucrit_by_intuition(Delta)), 0)
	},
	User#user{mfs = Mfs};
%% пересчет ХП и защиты от изменения выноса
on_stats_changed(User, {"dex", Delta}) when is_record(User, user) ->
	Vitality = change_state(?vitality(User), {"maxhp", formula:get_hp_by_dex(Delta)}),
	Dprotection = change_state(User, {"dprotection", formula:get_dprotection_by_dex(Delta)}),
	Wprotection = change_state(User, {"wprotection", formula:get_wprotection_by_dex(Delta)}),
	User#user{vitality = Vitality,
			  dprotection = ?dprotection(Dprotection),
			  wprotection =?wprotection(Wprotection)};
%% пересчет мощности магии от интеллекта
on_stats_changed(User, {"intel", Delta}) when is_record(User, user) ->
	change_state(User, {"wpower", formula:get_wpower_by_intellect(Delta)});
%% пересчет кол-ва маны от мудрости
on_stats_changed(User, {"wisd", Delta}) when is_record(User, user) ->
	change_state(User, {"vitality.maxmana", formula:get_mana_by_wisd(Delta)});
%% пересчет кол-ва духа от духовности
%% @todo а нужно ли оно?
%on_stats_changed(User, {"spir", Delta}) ->
%	change_state(User, {"vitality.maxmana", formula:get_mana_by_wisd(Delta)});

on_stats_changed(User, {_, _}) ->
	User.

