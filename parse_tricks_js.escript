#!/usr/bin/env escript
%% -*- coding: utf-8 -*-
%%! -env ERL_LIBS deps -pa ebin/ +pc unicode
%% -*- erlang -*-

%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Генератор описаний приемов из файла tmp/tricks.js в data/tricks.xml
%%% Перезаписывает файл data/tricks.xml
%%% ====================================================================
-include_lib("include/bme.hrl").


main(_) ->
	io:setopts([{encoding, unicode}]),
	TargetFile = "data/tricks.xml",
	file:copy(TargetFile, "tmp/tricks.xml.backup"),
	file:copy("data/templates/tricks.xml.tpl", TargetFile),
	{ok, File} = file:open(TargetFile, [append, {encoding, utf8}]),

	{ok, Json} = file:read_file("tmp/tricks.js"),
	{ok, Parsed, _} = rfc4627:decode(Json),

	start_tricks(Parsed, File).


start_tricks({obj, TricksList}, File) ->
	io:fwrite(File, "<tricks>\n", []),
	parse_tricks(TricksList, File).

parse_tricks([Trick | Tricks], File) ->
	parse_trick(Trick, File),
	parse_tricks(Tricks, File);
parse_tricks([], File) -> io:fwrite(File, "</tricks>", []).

parse_trick({Id, {obj, Attrs}}, File) ->
	Trick = parse_trick_attrs(#trick{id = list_to_atom(Id)}, Attrs),
	write_trick(Trick, File).

parse_trick_attrs(Trick, []) -> Trick;

parse_trick_attrs(Trick, [{"name", <<"wis", _Value>>} | Attrs]) ->
	parse_trick_attrs(Trick#trick{type = <<"spell">>}, Attrs);
parse_trick_attrs(Trick, [{"name", _Value} | Attrs]) ->
	parse_trick_attrs(Trick#trick{type = <<"trick">>}, Attrs);

parse_trick_attrs(Trick, [{"caption", Value} | Attrs]) when is_binary(Value) ->
	parse_trick_attrs(Trick#trick{name = Value}, Attrs);
parse_trick_attrs(Trick, [{"description", Value} | Attrs]) when is_binary(Value) ->
	parse_trick_attrs(Trick#trick{description = Value}, Attrs);
parse_trick_attrs(Trick, [{"school", Value} | Attrs]) when is_binary(Value) ->
	parse_trick_attrs(Trick#trick{class = Value}, Attrs);

parse_trick_attrs(Trick, [{"required", {obj, Value}} | Attrs]) ->
	parse_trick_attrs(parse_trick_attrs(Trick, Value), Attrs);

parse_trick_attrs(Trick, [{"resources", {obj, Value}} | Attrs]) ->
	parse_trick_attrs(parse_trick_attrs(Trick, Value), Attrs);

parse_trick_attrs(Trick, [{"consumes", {obj, Value}} | Attrs]) ->
	parse_trick_attrs(parse_trick_attrs(Trick, Value), Attrs);

parse_trick_attrs(Trick, [{"attack", {obj, _Value}} | Attrs]) ->
	parse_trick_attrs(Trick, Attrs);

parse_trick_attrs(Trick, [{"healing", {obj, _Value}} | Attrs]) ->
	parse_trick_attrs(Trick, Attrs);

parse_trick_attrs(Trick, [{"level", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{level = Value}, Attrs);

parse_trick_attrs(#trick{stats = Stats} = Trick, [{"strength", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{stats = Stats#u_stats{str = Value}}, Attrs);
parse_trick_attrs(#trick{stats = Stats} = Trick, [{"endurance", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{stats = Stats#u_stats{dex = Value}}, Attrs);
parse_trick_attrs(#trick{stats = Stats} = Trick, [{"intellect", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{stats = Stats#u_stats{intel = Value}}, Attrs);

parse_trick_attrs(#trick{skills = Skills} = Trick, [{"airmagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{air = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"earthmagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{earth = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"firemagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{fire = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"watermagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{water = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"greymagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{gray = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"lightmagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{light = Value}}, Attrs);
parse_trick_attrs(#trick{skills = Skills} = Trick, [{"darkmagicskill", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{skills = Skills#u_skills{dark = Value}}, Attrs);

parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"hit", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{attack = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"block", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{block = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"krit", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{crit = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"counter", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{counter = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"parry", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{parry = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"hp", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{hearts = Value}}, Attrs);
parse_trick_attrs(#trick{tactics = Tactics} = Trick, [{"spiritlevel", Value} | Attrs]) ->
	parse_trick_attrs(Trick#trick{tactics = Tactics#b_tactics{spirit = Value}}, Attrs);

parse_trick_attrs(Trick, [{"mana", Value} | Attrs]) when is_integer(Value) ->
	parse_trick_attrs(Trick#trick{mana = Value}, Attrs);

parse_trick_attrs(Trick, [{"iname", _Value} | Attrs]) ->
	parse_trick_attrs(Trick, Attrs);

%%                 action

parse_trick_attrs(Trick, [UnknownAttr | Attrs]) ->
	io:format("Unknown attr ~p~n", [UnknownAttr]),
	parse_trick_attrs(Trick, Attrs).




write_trick(Trick, File) ->
	io:format("WRITE TRICK ~p~n", [Trick#trick.id]),
	S = "
	<trick>
		<id>~ts</id>
		<name><![CDATA[~ts]]></name>
		<description><![CDATA[~ts]]></description>
		<class>~ts</class>
		<type>~ts</type>
		<level>~b</level>
		<delay></delay>
		~ts~ts~ts
		<self_buff>~ts</self_buff>
	</trick>",
	%io:fwrite(File, "~ts~n", [Str]).
	io:fwrite(File, S, [Trick#trick.id, Trick#trick.name, Trick#trick.description, Trick#trick.class, Trick#trick.type, Trick#trick.level,
						write_stats(Trick#trick.stats), write_tactics(Trick#trick.tactics), write_skills(Trick#trick.skills),
						Trick#trick.id]),
	ok.


write_tactics(Tactics) when Tactics =:= #b_tactics{} -> "";
write_tactics(Tactics) ->
	S = "<tactics>\n" ++ write_tactic(attack, Tactics#b_tactics.attack) ++
		write_tactic(block, Tactics#b_tactics.block) ++
		write_tactic(crit, Tactics#b_tactics.crit) ++
		write_tactic(counter, Tactics#b_tactics.counter) ++
		write_tactic(parry, Tactics#b_tactics.parry) ++
		write_tactic(hearts, Tactics#b_tactics.hearts) ++
		write_tactic(spirit, Tactics#b_tactics.spirit) ++ "\t\t</tactics>".

write_stats(Stats) when Stats =:= #u_stats{} -> "";
write_stats(Stats) ->
	S = "<stats>\n" ++ write_tactic(str, Stats#u_stats.str) ++
		write_tactic(agil, Stats#u_stats.agil) ++
		write_tactic(int, Stats#u_stats.int) ++
		write_tactic(dex, Stats#u_stats.dex) ++
		write_tactic(intel, Stats#u_stats.intel) ++
		write_tactic(wisd, Stats#u_stats.wisd) ++ "\t\t</stats>".

write_skills(Skills) when Skills =:= #u_skills{} -> "";
write_skills(Skills) ->
	S = "<skills>\n" ++ write_tactic(knife, Skills#u_skills.knife) ++
		write_tactic(axe, Skills#u_skills.axe) ++
		write_tactic(hammer, Skills#u_skills.hammer) ++
		write_tactic(sword, Skills#u_skills.sword) ++
		write_tactic(staff, Skills#u_skills.staff) ++
		write_tactic(fire, Skills#u_skills.fire) ++
		write_tactic(air, Skills#u_skills.air) ++
		write_tactic(water, Skills#u_skills.water) ++
		write_tactic(earth, Skills#u_skills.earth) ++
		write_tactic(light, Skills#u_skills.light) ++
		write_tactic(dark, Skills#u_skills.dark) ++
		write_tactic(gray, Skills#u_skills.gray) ++ "\t\t</skills>".

write_tactic(S, Value) when Value > 0 ->
	F = "\t\t\t<" ++ atom_to_list(S) ++ ">~ts</" ++ atom_to_list(S) ++ ">\n",
	io_lib:format(F, [s(Value)]);
write_tactic(S, Value) -> "".


s(A) when is_atom(A)-> atom_to_list(A);
s(A) when is_binary(A)-> io_lib:format("~ts", [A]);
s(A) when is_integer(A)-> integer_to_list(A);
s(A) when is_float(A)-> io_lib:format("~.1f", [A]).
