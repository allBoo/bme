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
		 hit/3,
		 magic/2,
		 magic/3,
		 killed/2,
		 damage/2,
		 test/0]).


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
hit(BattleId, HitLog) ->
	?CALL(BattleId, {hit, HitLog}).


%% hit/3
%% ====================================================================
%% запись удара в лог
hit(BattleId, TransactionId, HitLog) ->
	?CAST(BattleId, {hit, TransactionId, HitLog}).


%% magic/2
%% ====================================================================
%% запись магического каста в лог
magic(BattleId, MagicLog) ->
	?CALL(BattleId, {magic, MagicLog}).


%% magic/3
%% ====================================================================
%% запись магического каста в лог
magic(BattleId, TransactionId, MagicLog) ->
	?CAST(BattleId, {magic, TransactionId, MagicLog}).


%% killed/2
%% ====================================================================
%% запись об убийстве юнита
killed(BattleId, Unit) ->
	?CALL(BattleId, {killed, Unit}).


damage(BattleId, Unit) ->
	?CALL(BattleId, {damage, Unit}).

test() ->
	get_hit_index([head, torso, legs]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {battle_id, file}).

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

	random:seed(now()),

	LogFile = io_lib:format("log/battle_~b.log.html", [BattleId]),
	file:copy("tmp/battle_log.tpl", LogFile),
	{ok, File} = file:open(LogFile, [append, {encoding, utf8}]),

	{ok, #state{battle_id = BattleId, file = File}}.


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
	gproc:set_value({n, l, TransactionId}, []),
	{reply, {ok, TransactionId}, State};


%% коммит транзакции
handle_call(commit, {FromPid, _}, State) ->
	%commit_local_transaction(State),
	TransactionId = {log_transaction, FromPid},
	R = case gproc:lookup_local_name(TransactionId) of
			undefined -> ?ERROR_NOT_APPLICABLE;
			_ ->
				Logs = gproc:get_value({n, l, TransactionId}),
				gproc:unregister_name({n, l, TransactionId}),
				write(Logs, State#state.file)
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
handle_call({hit, HitLog}, {FromPid, _}, State) ->
	TransactionId = {log_transaction, FromPid},
	R = case gproc:lookup_local_name(TransactionId) of
			%% если транзакция не запущена, пишем локально
			undefined -> write(HitLog, State#state.file);
			_ -> write(HitLog, TransactionId, State#state.file)
		end,
	{reply, R, State};


%% запись об убийстве юнита
handle_call({killed, Unit}, _From, State) ->
	write({killed, Unit}, State#state.file),
	{reply, ok, State};


%% запись полученного урона (дебаг)
handle_call({damage, Unit}, _From, State) ->
	write(Unit, State#state.file),
	{reply, ok, State};


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
handle_cast({hit, FromPid, HitLog}, State) ->
	TransactionId = {log_transaction, FromPid},
	case gproc:lookup_local_name(TransactionId) of
		%% если транзакция не запущена, пишем локально
		undefined -> write(HitLog, State#state.file);
		_ -> write(HitLog, TransactionId, State#state.file)
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
terminate(_Reason, State) ->
	file:close(State#state.file),
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

%% возвращает время в формате H:M
curtime() ->
	{_, {H, M, _}} = erlang:localtime(),
	io_lib:format("~2..0b:~2..0b", [H, M]).


%% выбор случайной фразы из списка
phrase(List) ->
	lists:nth(random:uniform(length(List)), List).


get_hit_p1(male) ->
	X = #log_hit_p1{},
	phrase(X#log_hit_p1.male);
get_hit_p1(female) ->
	X = #log_hit_p1{},
	phrase(X#log_hit_p1.female).

get_hit_p2() ->
	X = #log_hit_p2{},
	phrase(X#log_hit_p2.any).

get_miss_p2() ->
	X = #log_miss_p2{},
	phrase(X#log_miss_p2.any).

get_hit_p3(male) ->
	X = #log_hit_p3{},
	phrase(X#log_hit_p3.male);
get_hit_p3(female) ->
	X = #log_hit_p3{},
	phrase(X#log_hit_p3.female).

get_hit_p4() ->
	X = #log_hit_p4{},
	phrase(X#log_hit_p4.any).

get_hit_p51(DamageType, male) ->
	X = #log_hit_p51_male{},
	get_hit_p51(DamageType, X);
get_hit_p51(DamageType, female) ->
	X = #log_hit_p51_female{},
	get_hit_p51(DamageType, X);
get_hit_p51(prick, X) ->
	phrase(element(2, X));
get_hit_p51(chop, X) ->
	phrase(element(3, X));
get_hit_p51(crush, X) ->
	phrase(element(4, X));
get_hit_p51(cut, X) ->
	phrase(element(5, X)).

get_hit_p52(male) ->
	X = #log_hit_p52{},
	phrase(X#log_hit_p52.male);
get_hit_p52(female) ->
	X = #log_hit_p52{},
	phrase(X#log_hit_p52.female).

get_hit_p521() ->
	X = #log_hit_p521{},
	phrase(X#log_hit_p521.any).

get_hit_p522(prick) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.prick);
get_hit_p522(chop) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.chop);
get_hit_p522(crush) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.crush);
get_hit_p522(cut) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.cut);
get_hit_p522(air) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.air);
get_hit_p522(fire) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.fire);
get_hit_p522(water) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.water);
get_hit_p522(earth) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.earth);
get_hit_p522(light) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.light);
get_hit_p522(dark) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.dark);
get_hit_p522(gray) ->
	X = #log_hit_p522{},
	phrase(X#log_hit_p522.gray).

get_hit_p6(arm) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.arm);
get_hit_p6(knife) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.knife);
get_hit_p6(axe) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.axe);
get_hit_p6(hammer) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.hammer);
get_hit_p6(sword) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.sword);
get_hit_p6(staff) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.staff);
get_hit_p6(bucket) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.bucket);
get_hit_p6(nails) ->
	X = #log_hit_p6{},
	phrase(X#log_hit_p6.nails).

get_hit_p8() ->
	X = #log_hit_p8{},
	phrase(X#log_hit_p8.any).

get_hit_p7(head) ->
	X = #log_hit_p7{},
	phrase(X#log_hit_p7.head);
get_hit_p7(torso) ->
	X = #log_hit_p7{},
	phrase(X#log_hit_p7.torso);
get_hit_p7(paunch) ->
	X = #log_hit_p7{},
	phrase(X#log_hit_p7.paunch);
get_hit_p7(belt) ->
	X = #log_hit_p7{},
	phrase(X#log_hit_p7.belt);
get_hit_p7(legs) ->
	X = #log_hit_p7{},
	phrase(X#log_hit_p7.legs).

get_crit_p1(HitZone, false) ->
	X = #log_crit_p1{},
	get_crit_p1(HitZone, X);
get_crit_p1(HitZone, true) ->
	X = #log_critblock_p1{},
	get_crit_p1(HitZone, X);
get_crit_p1(head, X) ->
	element(2, X);
get_crit_p1(torso, X) ->
	element(3, X);
get_crit_p1(paunch, X) ->
	element(4, X);
get_crit_p1(belt, X) ->
	element(5, X);
get_crit_p1(legs, X) ->
	element(6, X).

get_crit_p2(HitZone, male) ->
	X = #log_crit_p2_male{},
	get_crit_p1(HitZone, X);
get_crit_p2(HitZone, female) ->
	X = #log_crit_p2_female{},
	get_crit_p1(HitZone, X).


get_miss_p5(dodge, Sex) ->
	X = #log_miss_p5_dodge{},
	get_miss_p5(X, Sex);
get_miss_p5(block, Sex) ->
	X = #log_miss_p5_block{},
	get_miss_p5(X, Sex);
get_miss_p5(shield, Sex) ->
	X = #log_miss_p5_shield{},
	get_miss_p5(X, Sex);
get_miss_p5(parry, Sex) ->
	X = #log_miss_p5_parry{},
	get_miss_p5(X, Sex);
get_miss_p5(X, male) ->
	phrase(element(2, X));
get_miss_p5(X, female) ->
	phrase(element(3, X)).

get_miss_p6(counter, male) ->
	X = #log_miss_p6_counter{},
	phrase(X#log_miss_p6_counter.male);
get_miss_p6(counter, female) ->
	X = #log_miss_p6_counter{},
	phrase(X#log_miss_p6_counter.female).

get_killed_p1(male) ->
	X = #log_killed_p1{},
	phrase(X#log_killed_p1.male);
get_killed_p1(female) ->
	X = #log_killed_p1{},
	phrase(X#log_killed_p1.female).

get_hit_index(Hits) when is_list(Hits) ->
	get_hits_index(Hits, 0);

get_hit_index(head)   -> 1;
get_hit_index(torso)  -> 2;
get_hit_index(paunch) -> 3;
get_hit_index(belt)   -> 4;
get_hit_index(legs)   -> 5;
get_hit_index(_)      -> 0.

get_hits_index([], Acc) ->
	Acc;

get_hits_index([Hit|Hits], Acc) ->
	Acc0 = Acc * 10 + get_hit_index(Hit),
	get_hits_index(Hits, Acc0).


%% запись логов
write(LogsList, File) when is_list(LogsList) ->
	io:fwrite(File, ?log_delimiter, []),
	lists:foreach(fun(El) -> write(El, File) end, LogsList),
	ok;

write(HitLog, File) when is_record(HitLog, log_hit) ->
	Log = HitLog#log_hit.hit_result,
	Attacker  = HitLog#log_hit.attacker,
	Defendant = HitLog#log_hit.defendant,
	%% общая для всех видов лога часть параметров
	HeadParams = [curtime(), get_hit_index(Log#b_hit_result.hit), get_hit_index(Log#b_hit_result.blocks), Defendant#log_unit.team, Attacker#log_unit.team,
				  Defendant#log_unit.team, Defendant#log_unit.name, get_hit_p1(Defendant#log_unit.sex),
				  get_hit_p2(), get_hit_p3(Attacker#log_unit.sex), Attacker#log_unit.team, Attacker#log_unit.name],
	TailParams = [Attacker#log_unit.name, Log#b_hit_result.damage, Defendant#log_unit.hp, Defendant#log_unit.maxhp],

	%% выбираем шаблоны
	case Log#b_hit_result.crit of
		true  ->
			Template = ?log_crit_tpl,
			Params = HeadParams ++
						 [get_crit_p1(Log#b_hit_result.hit, Log#b_hit_result.crit_break),
						  get_crit_p2(Log#b_hit_result.hit, Attacker#log_unit.sex)] ++
						 TailParams;
		false ->
			%% все магические атаки идут на 2-й шаблон
			%% атаки оружием рандомно на 1-й или 2-й
			case
				user_helper:is_natural_type(Log#b_hit_result.damage_type) and
					(random:uniform(2) == 1) of
				true ->
					Template = ?log_hit_tpl1,
					Params = HeadParams ++
						 [get_hit_p4(), get_hit_p51(Log#b_hit_result.damage_type, Attacker#log_unit.sex),
						  get_hit_p6(Log#b_hit_result.weapon_type), get_hit_p7(Log#b_hit_result.hit),
						  get_hit_p8()] ++
						 TailParams;
				false ->
					Template = ?log_hit_tpl2,
					Params = HeadParams ++
						 [get_hit_p4(), get_hit_p52(Attacker#log_unit.sex), get_hit_p521(),
						  get_hit_p522(Log#b_hit_result.damage_type), get_hit_p6(Log#b_hit_result.weapon_type),
						  get_hit_p7(Log#b_hit_result.hit), get_hit_p8()] ++
						 TailParams
			end
	end,

	io:fwrite(File, Template, Params),
	ok;

write(HitLog, File) when is_record(HitLog, log_miss) ->
	Log = HitLog#log_miss.hit_result,
	Attacker  = HitLog#log_miss.attacker,
	Defendant = HitLog#log_miss.defendant,
	%% общая для всех видов лога часть параметров
	HeadParams = [curtime(), get_hit_index(Log#b_hit_result.hit), get_hit_index(Log#b_hit_result.blocks), Defendant#log_unit.team, Attacker#log_unit.team,
				  Attacker#log_unit.team, Attacker#log_unit.name, get_hit_p1(Attacker#log_unit.sex),
				  get_miss_p2(), get_hit_p3(Defendant#log_unit.sex), Defendant#log_unit.team, Defendant#log_unit.name],

	Template = case Log#b_hit_result.counter of
				   true  -> ?log_counter_tpl;
				   false -> ?log_miss_tpl
			   end,

	Params = HeadParams ++ case Log#b_hit_result.dodge of
				 true  ->
					 [get_miss_p5(dodge, Defendant#log_unit.sex), get_hit_p6(Log#b_hit_result.weapon_type),
									get_hit_p7(Log#b_hit_result.hit)] ++
						 case Log#b_hit_result.counter of
							 true  -> [get_miss_p6(counter, Defendant#log_unit.sex)];
							 false -> []
						 end;
				 false ->
					 case Log#b_hit_result.parry of
						 true -> [get_miss_p5(parry, Defendant#log_unit.sex)];
						 false -> case Log#b_hit_result.shield of
									  true  -> [get_miss_p5(shield, Defendant#log_unit.sex)];
									  false -> [get_miss_p5(block, Defendant#log_unit.sex)]
								  end
					 end ++
						 [get_hit_p6(Log#b_hit_result.weapon_type), get_hit_p7(Log#b_hit_result.hit)]
			 end,

	io:fwrite(File, Template, Params),
	ok;


write({killed, Unit}, File) when is_record(Unit, log_unit) ->
	Template = ?log_killed_tpl,
	Params = [curtime(), Unit#log_unit.name, get_killed_p1(Unit#log_unit.sex)],
	io:fwrite(File, Template, Params),
	ok;


write(Unit, File) when is_record(Unit, log_unit) ->
	Template = ?log_start ++ ?log_unit ++ "[~b/~b]" ++ ?log_end,
	Params = [Unit#log_unit.team, Unit#log_unit.name, Unit#log_unit.hp, Unit#log_unit.maxhp],
	io:fwrite(File, Template, Params),
	ok;


write(Log, _File) ->
	?LOG("~p", [Log]),
	ok.

write(Log, TransactionId, _File) ->
	Logs = gproc:get_value({n, l, TransactionId}),
	gproc:set_value({n, l, TransactionId}, Logs ++ [Log]),
	ok.
