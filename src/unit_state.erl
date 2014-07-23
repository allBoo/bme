%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Управление стейтом юнита (b_unit)
%%% ====================================================================

-module(unit_state).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_attr/2, change_state/3, increase/2, reduce/2]).


%% get_attr/2
%% ====================================================================
%% @doc возвращает заданный параметр юзера

%% корневые элементы
get_attr(id, Unit)            -> Unit#b_unit.id;
get_attr(pid, Unit)           -> Unit#b_unit.pid;
get_attr(name, Unit)          -> Unit#b_unit.name;
get_attr(ai, Unit)            -> Unit#b_unit.ai;
get_attr(battle_id, Unit)     -> Unit#b_unit.battle_id;
get_attr(battle_pid, Unit)    -> Unit#b_unit.battle_pid;
get_attr(team_id, Unit)       -> Unit#b_unit.team_id;
get_attr(team_pid, Unit)      -> Unit#b_unit.team_pid;
get_attr(user, Unit)          -> Unit#b_unit.user;
get_attr(alive, Unit)         -> Unit#b_unit.alive;
get_attr(leader, Unit)        -> Unit#b_unit.leader;
get_attr(opponents, Unit)     -> Unit#b_unit.opponents;
get_attr(opponent, Unit)      -> Unit#b_unit.opponent;
get_attr(obtained, Unit)      -> Unit#b_unit.obtained;
get_attr(hits, Unit)          -> Unit#b_unit.hits;
get_attr(tactics, Unit)       -> Unit#b_unit.tactics;
get_attr(total_damaged, Unit) -> Unit#b_unit.total_damaged;
get_attr(total_healed, Unit)  -> Unit#b_unit.total_healed;
get_attr(total_lost, Unit)    -> Unit#b_unit.total_lost;
get_attr(exp, Unit)           -> Unit#b_unit.exp;

%% внутренние элементы
get_attr(Part0, Unit) when is_atom(Part0),
						   is_record(Unit, b_unit) ->
	Part = atom_to_list(Part0),
	get_attr(Part, Unit);

%% b_opponent
get_attr("opponent." ++ Part, Unit) when is_record(Unit, b_unit) -> get_attr(Part, Unit#b_unit.opponent);
get_attr("pid", Opponent)     when is_record(Opponent, b_opponent) -> Opponent#b_opponent.pid;
get_attr("team", Opponent)    when is_record(Opponent, b_opponent) -> Opponent#b_opponent.team;
get_attr("id", Opponent)      when is_record(Opponent, b_opponent) -> Opponent#b_opponent.id;
get_attr("name", Opponent)    when is_record(Opponent, b_opponent) -> Opponent#b_opponent.name;
get_attr("ai", Opponent)      when is_record(Opponent, b_opponent) -> Opponent#b_opponent.ai;
get_attr("level", Opponent)   when is_record(Opponent, b_opponent) -> Opponent#b_opponent.level;
get_attr("align", Opponent)   when is_record(Opponent, b_opponent) -> Opponent#b_opponent.align;
get_attr("klan", Opponent)    when is_record(Opponent, b_opponent) -> Opponent#b_opponent.klan;
get_attr("cost", Opponent)    when is_record(Opponent, b_opponent) -> Opponent#b_opponent.cost;
get_attr("gray", Opponent)    when is_record(Opponent, b_opponent) -> Opponent#b_opponent.gray;
get_attr("timeout", Opponent) when is_record(Opponent, b_opponent) -> Opponent#b_opponent.timeout;
get_attr(_, Opponent) when Opponent =:= undefined -> undefined;

%% b_tactics
get_attr("tactics." ++ Part, Unit) when is_record(Unit, b_unit) -> get_attr(Part, Unit#b_unit.tactics);
get_attr("attack", Tactics)  when is_record(Tactics, b_tactics) -> Tactics#b_tactics.attack;
get_attr("crit", Tactics)    when is_record(Tactics, b_tactics) -> Tactics#b_tactics.crit;
get_attr("counter", Tactics) when is_record(Tactics, b_tactics) -> Tactics#b_tactics.counter;
get_attr("block", Tactics)   when is_record(Tactics, b_tactics) -> Tactics#b_tactics.block;
get_attr("parry", Tactics)   when is_record(Tactics, b_tactics) -> Tactics#b_tactics.parry;
get_attr("hearts", Tactics)  when is_record(Tactics, b_tactics) -> Tactics#b_tactics.hearts;
get_attr("spirit", Tactics)  when is_record(Tactics, b_tactics) -> Tactics#b_tactics.spirit;
get_attr("changes", Tactics) when is_record(Tactics, b_tactics) -> Tactics#b_tactics.changes.



%% increase/3
%% ====================================================================
%% изменение параметров юнита
increase(Unit, Delta) ->
	change_state(increase, Unit, Delta).


%% reduce/3
%% ====================================================================
%% изменение параметров юнита
reduce(Unit, Delta) ->
	change_state(reduce, Unit, Delta).


%% change_state/3
%% ====================================================================
%% изменение параметров юнита
change_state(_, Unit, []) when is_record(Unit, b_unit) ->
	Unit;

change_state(increase, Unit, [{State, Delta} | TParams]) when is_record(Unit, b_unit),
															  is_number(Delta) ->
	Unit0 = change_state(Unit, {State, Delta}),
	change_state(increase, Unit0, TParams);

change_state(reduce, Unit, [{State, Delta} | TParams]) when is_record(Unit, b_unit),
															is_number(Delta) ->
	Unit0 = change_state(Unit, {State, -Delta}),
	change_state(reduce, Unit0, TParams);

change_state(increase, Unit, [{tactics, Delta} | TParams]) when is_record(Unit, b_unit),
																is_record(Delta, b_tactics) ->
	Tactics0 = Unit#b_unit.tactics,
	Tactics  = #b_tactics{
		attack  = math:limit(Tactics0#b_tactics.attack  + Delta#b_tactics.attack, 0, 25),
		crit    = math:limit(Tactics0#b_tactics.crit    + Delta#b_tactics.crit, 0, 25),
		counter = math:limit(Tactics0#b_tactics.counter + Delta#b_tactics.counter, 0, 25),
		block   = math:limit(Tactics0#b_tactics.block   + Delta#b_tactics.block, 0, 25),
		parry   = math:limit(Tactics0#b_tactics.parry   + Delta#b_tactics.parry, 0, 25),
		hearts  = math:precision(math:limit(Tactics0#b_tactics.hearts  + Delta#b_tactics.hearts, 0, 25), 2),
		spirit  = math:precision(max(Tactics0#b_tactics.spirit         + Delta#b_tactics.spirit, 0), 2),
		changes = math:limit(Tactics0#b_tactics.changes + Delta#b_tactics.changes, 0, 25)
	},
	Unit0 = Unit#b_unit{tactics = Tactics},
	unit:notify(Unit#b_unit.id, {change_state, {tactics, Delta}}),
	change_state(increase, Unit0, TParams);

change_state(reduce, Unit, [{tactics, Delta} | TParams]) when is_record(Unit, b_unit),
															  is_record(Delta, b_tactics) ->
	Tactics0 = Unit#b_unit.tactics,
	Tactics  = #b_tactics{
		attack  = math:limit(Tactics0#b_tactics.attack  - Delta#b_tactics.attack, 0, 25),
		crit    = math:limit(Tactics0#b_tactics.crit    - Delta#b_tactics.crit, 0, 25),
		counter = math:limit(Tactics0#b_tactics.counter - Delta#b_tactics.counter, 0, 25),
		block   = math:limit(Tactics0#b_tactics.block   - Delta#b_tactics.block, 0, 25),
		parry   = math:limit(Tactics0#b_tactics.parry   - Delta#b_tactics.parry, 0, 25),
		hearts  = math:precision(math:limit(Tactics0#b_tactics.hearts  - Delta#b_tactics.hearts, 0, 25), 2),
		spirit  = math:precision(max(Tactics0#b_tactics.spirit         - Delta#b_tactics.spirit, 0), 2),
		changes = math:limit(Tactics0#b_tactics.changes - Delta#b_tactics.changes, 0, 25)
	},
	Unit0 = Unit#b_unit{tactics = Tactics},
	unit:notify(Unit#b_unit.id, {change_state, {tactics, Delta}}),
	change_state(reduce, Unit0, TParams).


change_state(Unit0, {State, Delta}) when is_atom(State),
										 is_record(Unit0, b_unit) ->
	SState = erlang:atom_to_list(State),
	Unit = change_state(Unit0, {SState, Delta}),
	unit:notify(Unit#b_unit.id, {change_state, {State, Delta}}),
	Unit;


%% изменение корневых свойств
change_state(Unit, {"total_damaged", Delta}) when is_record(Unit,b_unit) ->
	Unit#b_unit{total_damaged = change_state(Unit#b_unit.total_damaged, Delta)};
change_state(Unit, {"total_healed", Delta}) when is_record(Unit,b_unit) ->
	Unit#b_unit{total_healed = change_state(Unit#b_unit.total_healed, Delta)};
change_state(Unit, {"total_lost", Delta}) when is_record(Unit,b_unit) ->
	Unit#b_unit{total_lost = change_state(Unit#b_unit.total_lost, Delta)};
change_state(Unit, {"exp", Delta}) when is_record(Unit,b_unit) ->
	Unit#b_unit{exp = change_state(Unit#b_unit.exp, Delta)};


%% изменение тактик
change_state(Unit, {"tactics." ++ Part, Delta}) when is_record(Unit,b_unit) ->
	Unit#b_unit{tactics = change_state(Unit#b_unit.tactics, {Part, Delta})};
change_state(Tactics, {"attack", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{attack = math:limit(change_state(Tactics#b_tactics.attack, Delta), 0, 25)};
change_state(Tactics, {"crit", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{crit = math:limit(change_state(Tactics#b_tactics.crit, Delta), 0, 25)};
change_state(Tactics, {"counter", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{counter = math:limit(change_state(Tactics#b_tactics.counter, Delta), 0, 25)};
change_state(Tactics, {"block", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{block = math:limit(change_state(Tactics#b_tactics.block, Delta), 0, 25)};
change_state(Tactics, {"parry", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{parry = math:limit(change_state(Tactics#b_tactics.parry, Delta), 0, 25)};
change_state(Tactics, {"hearts", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{hearts = math:limit(change_state(Tactics#b_tactics.hearts, Delta), 0, 25)};
change_state(Tactics, {"spirit", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{spirit = max(change_state(Tactics#b_tactics.spirit, Delta), 0)};
change_state(Tactics, {"changes", Delta}) when is_record(Tactics, b_tactics) ->
	Tactics#b_tactics{changes = math:limit(change_state(Tactics#b_tactics.changes, Delta), 0, 25)};


%% изменение d-value
change_state(DValue, {"n", Delta}) when is_record(DValue, d_value) ->
	DValue#d_value{n = change_state(DValue#d_value.n, Delta)};
change_state(DValue, {"k", Delta}) when is_record(DValue, d_value) ->
	DValue#d_value{k = change_state(DValue#d_value.k, Delta)};

%% изменение числового параметра
change_state(Param, Delta) when is_number(Param),
								is_number(Delta) ->
	Param + Delta.

%% ====================================================================
%% Internal functions
%% ====================================================================


