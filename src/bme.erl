%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Battle Manager API
%%% ====================================================================


-module(bme).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_haot/3,
		 start_battle/4,
		 character_attack/2,
		 finish_battle/1]).

%% start_link/0
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% start_haot/3
%% ====================================================================
%% Запуск хаотического поединка
-spec start_haot(UsersIds ::[integer()], City ::integer(), Options ::[term()]) -> Result when
		  Result :: {ok, Id :: integer(), UsersIds :: [integer()]} | {error, Reason :: string()}.
start_haot(UsersIds, City, Options) ->
	call({haot, UsersIds, City, Options}).


start_battle(LeftTeam, RightTeam, City, Options) ->
	call({battle, LeftTeam, RightTeam, City, Options}).

%% character_attack/2
%% ====================================================================
%% Нападение на персонажа
character_attack(_Attacker, _Victim) ->
	ok.


%% finish_battle/1
%% ====================================================================
%% завершение поединка
finish_battle(Battle) when is_record(Battle, battle) ->
	case gproc:lookup_local_name({battle_sup, Battle#battle.id}) of
		undefined -> ?ERROR_NOT_APPLICABLE;
		BattlePid -> finish_battle(BattlePid)
	end;

finish_battle(BattlePid) when is_pid(BattlePid) ->
	?DBG("Terminate battle ~p~n", [BattlePid]),
	cast({finish_battle, BattlePid}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

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
init([]) ->
	random:seed(now()),
	{ok, #state{}}.


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

handle_call({haot, UsersIds, City, Options}, _From, State) ->
	Properties = proplists:unfold(Options),
	%% получаем инфу по всем пользователям и проверяем возможность их участия в поединке
	Users = lists:filter(fun(User) ->
					UserInfo = User#user.info,
					Vitality = User#user.vitality,
					Dress    = User#user.dress,
					BattleLevel = proplists:get_value(level, Properties, #b_level{min=0, max=21}),
					%% проверяем что пользователи находятся в походящих локациях (город, зал)
					(User#user.city == City) and lists:member(User#user.room, ?BATTLE_PLACES) and
					%% проверяем что пользователи подходят под условия поединка
						%% уровень персонажа
					((UserInfo#u_info.level >= BattleLevel#b_level.min) and (UserInfo#u_info.level =< BattleLevel#b_level.max)) and
						%% уровень ХП
					((Vitality#u_vitality.hp / Vitality#u_vitality.maxhp) > 0.3) and
						%% стоимость обвеса
					((Dress#u_dress.cost =< proplists:get_value(max_cost, Properties, 0)) or (proplists:get_value(max_cost, Properties, 0) == 0))
			end, user_helper:get(UsersIds)),
	%?DBG("Users ~p~n", [Users]),

	Res = case length(Users) >= 2 of
		true ->
			%% делим пользователей на команды в соотв. с выбранной стратегией (рандом / обвес)
			Teams = battle_helper:create_teams(user_helper:split_teams(Users, 2, proplists:get_value(distribution, Properties, default))),
			%%?DBG("Teams ~p~n", [Teams]),
			FirstUser = lists:nth(1, Users),
			%% говорим супервайзеру запустить новый бой
			battles_sup:start_battle(#battle{id = 0,
											 type = haot,
											 blood = proplists:get_bool(blood, Properties),
											 city = City,
											 room = FirstUser#user.city,
											 timeout = proplists:get_value(timeout, Properties, 5),
											 teams = Teams
											});
		false ->
			?ERROR_LOW_TEAM
	end,
	{reply, Res, State};


%% запуск физического поединка
handle_call({battle, LeftTeamIds, RightTeamIds, City, Options}, _From, State) ->
	Properties = proplists:unfold(Options),
	LeftTeamUsers  = user_helper:get(LeftTeamIds),
	RightTeamUsers = user_helper:get(RightTeamIds),
	Teams = battle_helper:create_teams([LeftTeamUsers, RightTeamUsers]),
	FirstUser = lists:nth(1, LeftTeamUsers),
	%% говорим супервайзеру запустить новый бой
	Res = battles_sup:start_battle(#battle{id = 0,
									 type = battle,
									 blood = proplists:get_bool(blood, Properties),
									 city = City,
									 room = FirstUser#user.city,
									 timeout = proplists:get_value(timeout, Properties, 5),
									 teams = Teams
									}),
	{reply, Res, State};


%% unknown request
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

%% завершение поединка
handle_cast({finish_battle, BattlePid}, State) ->
	battles_sup:finish_battle(BattlePid),
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

call(Req) ->
	chk_reply(gen_server:call(?MODULE, Req)).

cast(Req) ->
	chk_reply(gen_server:cast(?MODULE, Req)).

chk_reply(Reply) ->
	case is_record(Reply, error) of
		true -> ?THROW_BME_ERROR(Reply);
		_  -> Reply
	end.
