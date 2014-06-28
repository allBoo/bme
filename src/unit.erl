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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 create_opponent_info/1,
		 set_opponents/2,
		 hit/3,
		 hited/2]).

%% start_link/1
%% ====================================================================
%% регистрирует процесс нового участника боя
start_link(Unit) when is_record(Unit, b_unit) ->
	gen_server:start_link(?MODULE, Unit, []).


%% create_opponent_info/1
%% ====================================================================
%% возвращает краткую информацию о юните
create_opponent_info(UserId) when (is_list(UserId) or is_integer(UserId)) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> create_opponent_info(UserPid)
	end;

create_opponent_info(UserPid) when is_pid(UserPid) ->
	gen_server:call(UserPid, create_opponent_info).


%% set_opponents/2
%% ====================================================================
%% устанавливает юниту список оппонентов
set_opponents(UserId, OpponentsList) when (is_list(UserId) or is_integer(UserId)) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> set_opponents(UserPid, OpponentsList)
	end;

set_opponents(UserPid, OpponentsList) when is_pid(UserPid) ->
	gen_server:cast(UserPid, {set_opponents, OpponentsList}).


%% hit/3
%% ====================================================================
%% выставление удара противнику
hit(UserId, Hits, Block) when (is_list(UserId) or is_integer(UserId)) ->
	case gproc:lookup_local_name({unit, UserId}) of
		undefined -> ?ERROR_NOT_IN_BATTLE;
		UserPid   -> hit(UserPid, Hits, Block)
	end;

hit(UserPid, Hits, Block) when is_pid(UserPid) ->
	gen_server:call(UserPid, {hit, Hits, Block}).


%% hited/2
%% ====================================================================
%% противник выставил размен
hited(UnitPid, {From, Hit}) when is_pid(UnitPid), is_pid(From), is_pid(Hit) ->
	gen_server:cast(UnitPid, {hited, {From, Hit}}).


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
	%% регистрируем имя сервера
	true = gproc:add_local_name({unit, Unit#b_unit.battle_id, Unit#b_unit.team_id, Unit#b_unit.id}),
	true = gproc:add_local_name({unit, Unit#b_unit.id}),
	true = gproc:add_local_name({unit, Unit#b_unit.name}),

	%% регистрируем теги для получения broadcast сообщений
	true = gproc:add_local_property({team_unit, Unit#b_unit.battle_id, Unit#b_unit.team_id}, Unit#b_unit.id),
	true = gproc:add_local_property({battle_unit, Unit#b_unit.battle_id}, Unit#b_unit.id),
	true = gproc:add_local_property({battle, Unit#b_unit.battle_id}, Unit#b_unit.id),

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
                        level = ((Unit#b_unit.user)#user.info)#u_info.level,
                        align = ((Unit#b_unit.user)#user.info)#u_info.align,
                        klan  = ((Unit#b_unit.user)#user.info)#u_info.klan,
                        cost  = ((Unit#b_unit.user)#user.dress)#u_dress.cost
                        }, Unit};


%% выставление удара
handle_call({hit, HitsList, Block}, _, Unit) ->
	Opponent = Unit#b_unit.opponent,
	Hit = #b_hit{sender    = self(),
				 recipient = Opponent#b_opponent.pid,
				 hits      = HitsList,
				 block     = Block,
				 timeout   = battle:get_timeout(Unit#b_unit.battle_pid)},
	R = case is_record(Unit#b_unit.opponent, b_opponent) of
		false -> ?ERROR_WAIT_OPPONENT;
		true  ->
			%% проверяем что удар еще не выставлен данному противнику
			%% вообще такого быть не должно
			case lists:keyfind(Opponent#b_opponent.pid, 1, Unit#b_unit.hits) of
				true ->
					?LOG("Try hit already hited ~p~n", [Unit]),
					?ERROR_TOO_FAST;
				false ->
					%% если удар был выставлен противником
					case lists:keyfind(Opponent#b_opponent.pid, 1, Unit#b_unit.obtained) of
						{_OpponentPid, ObtainedHitPid} ->
							%% ответ на удар
							hit:reply(Unit#b_unit.battle_id, ObtainedHitPid, Hit);
						false ->
							%% выставляем новый размен
							hit:hit(Unit#b_unit.battle_id, Hit)
					end
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
			{reply, {hit_replied}, Unit};
		
		%% что-то пошло не так
		Erorr when is_record(Erorr, error) ->
			{reply, Erorr, Unit}
	end;


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

%% устанавливает юниту список оппонентов
handle_cast({set_opponents, OpponentsList}, Unit) ->
	%% сравниваем стоимось комлектов и определяем серых в бою
	CalculatedOpponentsList = lists:map(fun(Opponent) ->
												Delta = Opponent#b_opponent.cost / ((Unit#b_unit.user)#user.dress)#u_dress.cost,
												Opponent#b_opponent{gray = Delta < 0.6}
										end, OpponentsList),
	?DBG("Unit ~p set opponents ~p~n", [self(), CalculatedOpponentsList]),
	{noreply, Unit#b_unit{opponents = CalculatedOpponentsList}};


%% противник выставил размен
%% сохраняем его в списке
handle_cast({hited, {From, Hit}}, Unit) ->
	?DBG("User ~p hited", [{From, Hit}]),
	Obtained = Unit#b_unit.obtained ++ [{From, Hit}],
	{noreply, Unit#b_unit{obtained = Obtained}};


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
handle_info({team_start, TeamPid}, Unit) ->
	{noreply, Unit#b_unit{team_pid = TeamPid}};

%% уведомление о запуске боя
handle_info({battle_start, BattlePid}, Unit) ->
	%% формируем начальное состояние юнита
	BattleUnit = set_initial_unit_data(Unit),
	%% поиск подходящего оппонента
	Opponent = select_random_opponent(Unit#b_unit.opponents),
	{noreply, BattleUnit#b_unit{battle_pid = BattlePid, opponent = Opponent}};

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
