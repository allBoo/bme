#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%%! -env ERL_LIBS deps +pc unicode
%% -*- erlang -*-

%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Генератор модуля описаний приемов из файла data/tricks.xml
%%% Перезаписывает файл src/tricks.erl
%%% ====================================================================
-include_lib("include/bme.hrl").


main(_) ->
	io:setopts([{encoding, unicode}]),
	TargetFile = "src/tricks.erl",
	file:copy(TargetFile, "tmp/tricks.erl.backup"),
	file:copy("data/templates/tricks.erl.tpl", TargetFile),
	{ok, File} = file:open(TargetFile, [append, {encoding, utf8}]),

	{ok, Xml} = file:read_file("data/tricks.xml"),
	{ok, Parsed, _} = erlsom:simple_form(Xml, [{output_encoding, utf8}]),

	start_tricks(Parsed, File),
	io:format("Recompile...~n", []),
	os:cmd("rebar compile").


start_tricks({"tricks", [], Tricks}, File) ->
	parse_tricks(Tricks, File).

parse_tricks([Trick | Tricks], File) ->
	parse_trick(Trick, File),
	parse_tricks(Tricks, File);
parse_tricks([], _File) -> ok.

parse_trick({"trick",[], Attrs}, File) ->
	Trick = parse_trick_attrs(#trick{}, Attrs),
	write_trick(Trick, File).

parse_trick_attrs(Trick, []) -> Trick;
parse_trick_attrs(Trick, [{Attr,[],[Value]} | Attrs]) when is_binary(Value) ->
	parse_trick_attrs(Trick, [{Attr,[],[binary:bin_to_list(Value)]} | Attrs]);
parse_trick_attrs(Trick, [{Attr,[],["true"]} | Attrs]) ->
	parse_trick_attrs(Trick, [{Attr,[],[true]} | Attrs]);
parse_trick_attrs(Trick, [{Attr,[],["false"]} | Attrs]) ->
	parse_trick_attrs(Trick, [{Attr,[],[false]} | Attrs]);

parse_trick_attrs(Trick, [{"id",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{id = list_to_atom(Value)}, Attrs);
parse_trick_attrs(Trick, [{"name",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{name = binary:list_to_bin(Value)}, Attrs);
parse_trick_attrs(Trick, [{"decription",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{decription = binary:list_to_bin(Value)}, Attrs);
parse_trick_attrs(Trick, [{"class",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{class = list_to_atom(Value)}, Attrs);
parse_trick_attrs(Trick, [{"type",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{type = list_to_atom(Value)}, Attrs);
parse_trick_attrs(Trick, [{"mana",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{mana = list_to_integer(Value)}, Attrs);
parse_trick_attrs(Trick, [{"level",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{level = list_to_integer(Value)}, Attrs);
parse_trick_attrs(Trick, [{"initial_delay",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{initial_delay = list_to_integer(Value)}, Attrs);
parse_trick_attrs(Trick, [{"delay",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{delay = list_to_integer(Value)}, Attrs);
parse_trick_attrs(Trick, [{"class_delay",[],[Value]} | Attrs]) when is_boolean(Value) ->
	parse_trick_attrs(Trick#trick{class_delay = Value}, Attrs);
parse_trick_attrs(Trick, [{"expend",[],[Value]} | Attrs]) when is_boolean(Value) ->
	parse_trick_attrs(Trick#trick{expend = Value}, Attrs);
parse_trick_attrs(Trick, [{"require_target",[],[Value]} | Attrs]) when is_boolean(Value) ->
	parse_trick_attrs(Trick#trick{require_target = Value}, Attrs);
parse_trick_attrs(Trick, [{"target_type",[],[Value]} | Attrs]) ->
	parse_trick_attrs(Trick#trick{target_type = list_to_atom(Value)}, Attrs);

parse_trick_attrs(Trick, [{"tactics",[],TacticsAttr} | Attrs]) ->
	Tactics = parse_trick_tactics(#b_tactics{}, TacticsAttr),
	parse_trick_attrs(Trick#trick{tactics = Tactics}, Attrs);

parse_trick_attrs(Trick, [{"stats",[],StatsAttr} | Attrs]) ->
	Stats = parse_trick_stats(#u_stats{}, StatsAttr),
	parse_trick_attrs(Trick#trick{stats = Stats}, Attrs);

parse_trick_attrs(Trick, [{"skills",[],SkillsAttr} | Attrs]) ->
	Skills = parse_trick_skills(#u_skills{}, SkillsAttr),
	parse_trick_attrs(Trick#trick{skills = Skills}, Attrs);

parse_trick_attrs(Trick, [{"self_buff",[],BuffAttr} | Attrs]) ->
	Buff = parse_trick_buff(#u_buff{}, BuffAttr),
	parse_trick_attrs(Trick#trick{self_buff = Buff}, Attrs);
parse_trick_attrs(Trick, [{"enemy_buff",[],BuffAttr} | Attrs]) ->
	Buff = parse_trick_buff(#u_buff{}, BuffAttr),
	parse_trick_attrs(Trick#trick{enemy_buff = Buff}, Attrs);

parse_trick_attrs(Trick, [{"action", [{"arity", Arity}], ActionAttrs} | Attrs]) ->
	Action = parse_trick_action(Arity, ActionAttrs, Trick),
	parse_trick_attrs(Trick#trick{action = Action}, Attrs);

%%                 action

parse_trick_attrs(Trick, [UnknownAttr | Attrs]) ->
	io:format("Unknown attr ~p~n", [UnknownAttr]),
	parse_trick_attrs(Trick, Attrs).


%% tactics
parse_trick_tactics(Tactics, []) -> Tactics;
parse_trick_tactics(Tactics, [{Attr,[],[Value]} | Attrs]) when is_binary(Value) ->
	parse_trick_tactics(Tactics, [{Attr,[],[binary:bin_to_list(Value)]} | Attrs]);

parse_trick_tactics(Tactics, [{"attack",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{attack = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"crit",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{crit = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"counter",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{counter = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"block",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{block = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"parry",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{parry = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"hearts",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{hearts = list_to_integer(Value)}, Attrs);
parse_trick_tactics(Tactics, [{"spirit",[],["any"]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{spirit = any}, Attrs);
parse_trick_tactics(Tactics, [{"spirit",[],[Value]} | Attrs]) ->
	parse_trick_tactics(Tactics#b_tactics{spirit = list_to_integer(Value)}, Attrs);

parse_trick_tactics(Tactics, [UnknownAttr | Attrs]) ->
	io:format("Unknown tactic attr ~p~n", [UnknownAttr]),
	parse_trick_tactics(Tactics, Attrs).


%% stats
parse_trick_stats(Stats, []) -> Stats;
parse_trick_stats(Stats, [{Attr,[],[Value]} | Attrs]) when is_binary(Value) ->
	parse_trick_stats(Stats, [{Attr,[],[binary:bin_to_list(Value)]} | Attrs]);

parse_trick_stats(Stats, [{"str",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{str = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"agil",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{agil = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"int",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{int = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"dex",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{dex = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"intel",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{intel = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"wisd",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{wisd = list_to_integer(Value)}, Attrs);
parse_trick_stats(Stats, [{"spir",[],[Value]} | Attrs]) ->
	parse_trick_stats(Stats#u_stats{spir = list_to_integer(Value)}, Attrs);

parse_trick_stats(Stats, [UnknownAttr | Attrs]) ->
	io:format("Unknown stat attr ~p~n", [UnknownAttr]),
	parse_trick_stats(Stats, Attrs).


%% skills
parse_trick_skills(Skills, []) -> Skills;
parse_trick_skills(Skills, [{Attr,[],[Value]} | Attrs]) when is_binary(Value) ->
	parse_trick_skills(Skills, [{Attr,[],[binary:bin_to_list(Value)]} | Attrs]);

parse_trick_skills(Skills, [{"knife",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{knife = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"axe",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{axe = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"hammer",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{hammer = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"sword",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{sword = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"staff",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{staff = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"fire",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{fire = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"air",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{air = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"water",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{water = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"earth",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{earth = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"light",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{light = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"dark",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{dark = list_to_integer(Value)}, Attrs);
parse_trick_skills(Skills, [{"gray",[],[Value]} | Attrs]) ->
	parse_trick_skills(Skills#u_skills{gray = list_to_integer(Value)}, Attrs);

parse_trick_skills(Skills, [UnknownAttr | Attrs]) ->
	io:format("Unknown skills attr ~p~n", [UnknownAttr]),
	parse_trick_stats(Skills, Attrs).


parse_trick_buff(Buff, []) -> Buff;
parse_trick_buff(Buff, [{Attr,[],[Value]} | Attrs]) when is_binary(Value) ->
	parse_trick_buff(Buff, [{Attr,[],[binary:bin_to_list(Value)]} | Attrs]);

parse_trick_buff(Buff, [{"id",[],[Value]} | Attrs]) ->
	parse_trick_buff(Buff#u_buff{id = list_to_atom(Value)}, Attrs);
parse_trick_buff(Buff, [{"time",[],[Value]} | Attrs]) ->
	parse_trick_buff(Buff#u_buff{time = list_to_integer(Value)}, Attrs);
parse_trick_buff(Buff, [{"level",[],[Value]} | Attrs]) ->
	parse_trick_buff(Buff#u_buff{level = list_to_integer(Value)}, Attrs);

parse_trick_buff(Buff, [Id]) -> Buff#u_buff{id = list_to_atom(Id)};

parse_trick_buff(Buff, [UnknownAttr | Attrs]) ->
	io:format("Unknown buff attr ~p~n", [UnknownAttr]),
	parse_trick_buff(Buff, Attrs).


parse_trick_action(Arity, ActionAttrs, Trick) when is_binary(Arity) ->
	parse_trick_action(binary:bin_to_list(Arity), ActionAttrs, Trick);
parse_trick_action(Arity, ActionAttrs, Trick) when is_list(Arity) ->
	parse_trick_action(list_to_integer(Arity), ActionAttrs, Trick);
parse_trick_action(0, ActionAttrs, Trick) ->
	"fun() -> " ++ parse_action(ActionAttrs, Trick) ++ " end";
parse_trick_action(1, ActionAttrs, Trick) ->
	"fun(UnitPid) -> " ++ parse_action(ActionAttrs, Trick) ++ " end";
parse_trick_action(2, ActionAttrs, Trick) ->
	"fun(UnitPid, RecipientPid) -> " ++ parse_action(ActionAttrs, Trick) ++ " end".

parse_action([ActionAttrs], _Trick) when is_binary(ActionAttrs) ->
	binary:bin_to_list(ActionAttrs);

parse_action([{"predefined", [{"name", ActionName}], ActionAttrs}], Trick) when is_binary(ActionName) ->
	parse_action([{"predefined", [{"name", binary:bin_to_list(ActionName)}], ActionAttrs}], Trick);
parse_action([{"predefined", [{"name", ActionName}], ActionAttrs}], Trick) ->
	predefined_action(ActionName, ActionAttrs, Trick).

predefined_action("heal/4", ActionAttrs, Trick) ->
	Value  = decode_value(find_action_attr("value", ActionAttrs)),
	Spirit = decode_value(find_action_attr("spirit", ActionAttrs, false)),

	"trick_helper:heal(" ++ atom_to_list(Trick#trick.id) ++ ", UnitPid, " ++ Value ++ ", " ++ Spirit ++ ")".


find_action_attr(Name, Attrs) ->
	case lists:keyfind(Name, 1, Attrs) of
		false ->
			io:format("Action ~p value not found in ~p~n", [Name, Attrs]),
			erlang:error(action_value_no_found, [Name, Attrs]);
		{_, _, [Value]} when is_binary(Value) ->
			binary:bin_to_list(Value);
		{_, _, [Value]} -> Value
	end.

find_action_attr(Name, Attrs, Default) ->
	case lists:keyfind(Name, 1, Attrs) of
		false ->
			Default;
		{_, _, [Value]} when is_binary(Value) ->
			binary:bin_to_list(Value);
		{_, _, [Value]} -> Value
	end.


decode_value({"dvalue", [], [{"n", [],[N]},{"k",[],[K]}]}) when is_binary(N) ->
	decode_value({"dvalue", [], [{"n", [],[binary:bin_to_list(N)]},{"k",[],[K]}]});
decode_value({"dvalue", [], [{"n", [],[N]},{"k",[],[K]}]}) when is_binary(K) ->
	decode_value({"dvalue", [], [{"n", [],[N]},{"k",[],[binary:bin_to_list(K)]}]});
decode_value({"dvalue", [], [{"n", [],[N]},{"k",[],[K]}]}) ->
	"#d_value{n = " ++ N ++ ", k = " ++ K ++ "}";
decode_value(Value) when is_binary(Value) ->
	decode_value(binary:bin_to_list(Value));
decode_value(Value) -> Value.



write_trick(Trick, File) ->
	io:format("WRITE TRICK ~p~n", [Trick#trick.id]),
	Str = s(Trick#trick.id) ++ "() -> #trick{" ++
		"id = " ++ s(Trick#trick.id) ++
		", name = <<\"" ++ s(Trick#trick.name) ++ "\"/utf8>>" ++
		", decription = <<\"" ++ s(Trick#trick.decription) ++ "\"/utf8>>" ++
		", class = " ++ s(Trick#trick.class) ++
		", type = " ++ s(Trick#trick.type) ++
		", tactics = " ++ s(Trick#trick.tactics) ++
		", mana = " ++ s(Trick#trick.mana) ++
		", level = " ++ s(Trick#trick.level) ++
		", stats = " ++ s(Trick#trick.stats) ++
		", skills = " ++ s(Trick#trick.skills) ++
		", initial_delay = " ++ s(Trick#trick.initial_delay) ++
		", delay = " ++ s(Trick#trick.delay) ++
		", class_delay = " ++ s(Trick#trick.class_delay) ++
		", expend = " ++ s(Trick#trick.expend) ++
		", require_target = " ++ s(Trick#trick.require_target) ++
		", target_type = " ++ s(Trick#trick.target_type) ++
		", self_buff = " ++ s(Trick#trick.self_buff) ++
		", enemy_buff = " ++ s(Trick#trick.enemy_buff) ++
		", action = " ++ s(Trick#trick.action) ++ "}.",
	io:fwrite(File, "~ts~n", [Str]).


s(A) when is_atom(A)-> atom_to_list(A);
s(A) when is_binary(A)-> io_lib:format("~ts", [A]);
s(A) when is_integer(A)-> integer_to_list(A);

s(A) when is_record(A, b_tactics) ->
	"#b_tactics{" ++
		"attack = " ++ s(A#b_tactics.attack) ++
		", crit = " ++ s(A#b_tactics.crit) ++
		", counter = " ++ s(A#b_tactics.counter) ++
		", block = " ++ s(A#b_tactics.block) ++
		", parry = " ++ s(A#b_tactics.parry) ++
		", hearts = " ++ s(A#b_tactics.hearts) ++
		", spirit = " ++ s(A#b_tactics.spirit) ++ "}";

s(A) when is_record(A, u_buff) ->
	"#u_buff{" ++
		"id = " ++ s(A#u_buff.id) ++
		", level = " ++ s(A#u_buff.level) ++
		", time = " ++ s(A#u_buff.time) ++ "}";

s(A) when is_record(A, u_stats) ->
	"#u_stats{" ++
		"str = " ++ s(A#u_stats.str) ++
		", agil = " ++ s(A#u_stats.agil) ++
		", int = " ++ s(A#u_stats.int) ++
		", dex = " ++ s(A#u_stats.dex) ++
		", intel = " ++ s(A#u_stats.intel) ++
		", wisd = " ++ s(A#u_stats.wisd) ++
		", spir = " ++ s(A#u_stats.spir) ++ "}";

s(A) when is_record(A, u_skills) ->
	"#u_skills{" ++
		"knife = " ++ s(A#u_skills.knife) ++
		", axe = " ++ s(A#u_skills.axe) ++
		", hammer = " ++ s(A#u_skills.hammer) ++
		", sword = " ++ s(A#u_skills.sword) ++
		", staff = " ++ s(A#u_skills.staff) ++
		", fire = " ++ s(A#u_skills.fire) ++
		", air = " ++ s(A#u_skills.air) ++
		", water = " ++ s(A#u_skills.water) ++
		", earth = " ++ s(A#u_skills.earth) ++
		", light = " ++ s(A#u_skills.light) ++
		", dark = " ++ s(A#u_skills.dark) ++
		", gray = " ++ s(A#u_skills.gray) ++ "}";

s(A)-> A.
