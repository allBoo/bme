%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Предоставляет доступ к данным юзера (user.hrl)
%%% ====================================================================


-module(user_state).
-behaviour(gen_server).
-include_lib("bme.hrl").

%% standart behaviourals
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

-define(CAST(UserId, Cmd), case is_pid(UserId) of
								 true -> gen_server:cast(UserId, Cmd);
								 false ->
									 case reg:find({user, UserId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UserPid -> gen_server:cast(UserPid, Cmd) end end).
-define(CALL(UserId, Cmd), case is_pid(UserId) of
								 true -> gen_server:call(UserId, Cmd);
								 false ->
									 case reg:find({user, UserId}) of
										 undefined -> ?ERROR_NOT_IN_BATTLE;
										 UserPid -> gen_server:call(UserPid, Cmd) end end).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
		 get_pid/1,
		 get/1,
		 get/2,
		 increase/2,
		 reduce/2]).


%% start_link/1
%% ====================================================================
%% регистрирует процесс юзера
start_link(User) when is_record(User, user) ->
	gen_server:start_link(?MODULE, User, []).


%% get_pid/1
%% ====================================================================
%% возвращает pid юзера по его ID
get_pid(UserId) when is_pid(UserId) ->
	UserId;

get_pid(UserId) when is_integer(UserId) ->
	reg:find({user, UserId}).


%% get/1
%% ====================================================================
%% возвращает текущий State (информацию о юзере)
get(UserId) ->
	?CALL(UserId, get).


%% get/2
%% ====================================================================
%% возвращает выбранный параметр юзера
get(UserId, Part) when is_atom(Part) ->
	?CALL(UserId, {get, Part}).


%% increase/2
%% ====================================================================
%% увеличение параметров юнита
increase(UserId, Params) ->
	?CAST(UserId, {increase, Params}).


%% reduce/2
%% ====================================================================
%% уменьшение параметров юнита
reduce(UserId, Params) ->
	?CAST(UserId, {reduce, Params}).


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
init(User) when is_record(User, user) ->
	?DBG("Start user API server ~p~n", [User#user.id]),
	process_flag(trap_exit, true),

	%% регистрируем имя сервера
	ok = reg:name([{user, User#user.id}]),

	{ok, User}.


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

%% возвращает текущее состояние юзера
handle_call(get, _, User) ->
	{reply, User, User};

handle_call({get, Part}, _, User) ->
	{reply, get_attr(Part, User), User};


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

%% увеличение параметров юнита
handle_cast({increase, Params}, User) ->
	{noreply, change_state(increase, User, Params)};


%% уменьшение параметров юнита
handle_cast({reduce, Params}, User) ->
	{noreply, change_state(reduce, User, Params)};


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

%% change_state/3
%% ====================================================================
%% изменение параметров юнита
change_state(_, User, []) ->
	User;

change_state(increase, User, [HParam | TParams]) ->
	User0 = change_state(User, HParam),
	change_state(increase, User0, TParams);

change_state(reduce, User, [{State, Delta} | TParams]) ->
	User0 = change_state(User, {State, -Delta}),
	change_state(reduce, User0, TParams).


change_state(User0, {State, Delta}) when is_atom(State),
										is_record(User0, user) ->
	SState = erlang:atom_to_list(State),
	User = change_state(User0, {SState, Delta}),
	unit:notify(User#user.id, {change_state, {State, Delta}}),
	User;

%% изменение внутреннего параметра User
%change_state(Unit, {"user." ++ Part, Delta}) when is_record(Unit, b_unit) ->
%	Unit#b_unit{user = change_state(Unit#b_unit.user, {Part, Delta})};

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
change_state(TypeWPower, {"reduction", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{reduction = change_state(TypeWPower#u_wpower.reduction, Delta)};
change_state(TypeWPower, {"manaconsumption", Delta}) when is_record(TypeWPower, u_wpower) ->
	TypeWPower#u_wpower{manaconsumption = change_state(TypeWPower#u_wpower.manaconsumption, Delta)};

%% изменение живучести
change_state(User, {"vitality." ++ Part, Delta}) when is_record(User, user) ->
	User#user{vitality = change_state(User#user.vitality, {Part, Delta})};
change_state(Vitality, {"hp", Delta}) when is_record(Vitality, u_vitality) ->
	Vitality#u_vitality{hp = math:limit(change_state(Vitality#u_vitality.hp, Delta), 0, Vitality#u_vitality.maxhp)};
change_state(Vitality, {"maxhp", Delta}) when is_record(Vitality, u_vitality) ->
	MaxHP = max(change_state(Vitality#u_vitality.maxhp, Delta), 0),
	case Vitality#u_vitality.hp > MaxHP of
		true  ->  change_state(Vitality#u_vitality{maxhp = MaxHP}, {"hp", MaxHP - Vitality#u_vitality.hp});
		false -> Vitality#u_vitality{maxhp = MaxHP}
	end;
change_state(Vitality, {"mana", Delta}) when is_record(Vitality, u_vitality) ->
	Vitality#u_vitality{mana = math:limit(change_state(Vitality#u_vitality.mana, Delta), 0, Vitality#u_vitality.maxmana)};
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
change_state(Mfs, {"acounter", Delta}) when is_record(Mfs, u_mf) ->
	Mfs#u_mf{acounter = max(change_state(Mfs#u_mf.acounter, Delta), 0)};
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


%% get_attr/2
%% ====================================================================
%% возвращает заданный параметр юзера

%% корневые элементы
get_attr(id, User) -> User#user.id;
get_attr(name, User) -> User#user.name;
get_attr(ai, User) -> User#user.ai;
get_attr(ai_level, User) -> User#user.ai_level;
get_attr(city, User) -> User#user.city;
get_attr(room, User) -> User#user.room;
get_attr(info, User) -> User#user.info;
get_attr(dress, User) -> User#user.dress;
get_attr(vitality, User) -> User#user.vitality;
get_attr(stats, User) -> User#user.stats;
get_attr(mfs, User) -> User#user.mfs;
get_attr(damage, User) -> User#user.damage;
get_attr(dpower, User) -> User#user.dpower;
get_attr(wpower, User) -> User#user.wpower;
get_attr(skills, User) -> User#user.skills;
get_attr(armor, User) -> User#user.armor;
get_attr(dprotection, User) -> User#user.dprotection;
get_attr(wprotection, User) -> User#user.wprotection;
get_attr(battle_spec, User) -> User#user.battle_spec;
get_attr(buffs, User) -> User#user.buffs;
get_attr(tricks, User) -> User#user.tricks;

%% внутренние элементы
get_attr(Part0, User) when is_atom(Part0),
						   is_record(User, user) ->
	Part = atom_to_list(Part0),
	get_attr(Part, User);

%% u_info
get_attr("info." ++ Part, User) when is_record(User, user) -> get_attr(Part, User#user.info);
get_attr("level", Info) when is_record(Info, u_info) -> Info#u_info.level;
get_attr("align", Info) when is_record(Info, u_info) -> Info#u_info.align;
get_attr("klan", Info) when is_record(Info, u_info) -> Info#u_info.klan;
get_attr("pic", Info) when is_record(Info, u_info) -> Info#u_info.pic;
get_attr("sex", Info) when is_record(Info, u_info) -> Info#u_info.sex;

%% u_stats
get_attr("stats." ++ Part, User) when is_record(User, user) -> get_attr(Part, User#user.stats);
get_attr("str", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.str;
get_attr("agil", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.agil;
get_attr("int", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.int;
get_attr("dex", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.dex;
get_attr("intel", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.intel;
get_attr("wisd", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.wisd;
get_attr("spir", Stats) when is_record(Stats, u_stats) -> Stats#u_stats.spir;

%% u_dress
get_attr("dress." ++ Part, User) when is_record(User, user) -> get_attr(Part, User#user.dress);
get_attr("cost", Dress) when is_record(Dress, u_dress) -> Dress#u_dress.cost;

%% u_vitality
get_attr("vitality." ++ Part, User) when is_record(User, user) -> get_attr(Part, User#user.vitality);
get_attr("hp", Vitality) when is_record(Vitality, u_vitality) -> Vitality#u_vitality.hp;
get_attr("maxhp", Vitality) when is_record(Vitality, u_vitality) -> Vitality#u_vitality.maxhp;
get_attr("mana", Vitality) when is_record(Vitality, u_vitality) -> Vitality#u_vitality.mana;
get_attr("maxmana", Vitality) when is_record(Vitality, u_vitality) -> Vitality#u_vitality.maxmana;

%% u_battle_spec
get_attr("battle_spec." ++ Part, User) when is_record(User, user) -> get_attr(Part, User#user.battle_spec);
get_attr("hit_points", BattleSpec) when is_record(BattleSpec, u_battle_spec) -> BattleSpec#u_battle_spec.hit_points;
get_attr("magic_points", BattleSpec) when is_record(BattleSpec, u_battle_spec) -> BattleSpec#u_battle_spec.magic_points;
get_attr("block_points", BattleSpec) when is_record(BattleSpec, u_battle_spec) -> BattleSpec#u_battle_spec.block_points.

