%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Расчеты боевки
%%% http://www.darkclan.ru/cgi/lib.pl?p=boevka
%%% http://mycombats.com/forum.php?topic=334546
%%% ====================================================================


-module(formula).
-include_lib("bme.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([is_dodge/2,
		 is_counter/2,
		 is_crit/3,
		 is_parry/2,
		 is_shield_block/2,
		 get_damage_power/2,
		 get_base_damage/3]).


%% is_dodge/2
%% ====================================================================
%% уворот
is_dodge(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),

	%% максимальный шанс уворота 80%, под своей удачей 90%, под удачей атакера - 70%
	%% удача атакера и защитника нивелируют
	MaxDodgeChance = case DefendantMf#u_mf.luck - AttackerMf#u_mf.luck of
						 Lucky  when Lucky  > 0 -> 0.9;
						 UnLuky when UnLuky < 0 -> 0.7;
						 _ -> 0.8
					 end,
	%% шанс уворота = (Уворот - Антиуворот) / Уворот) в пределах от 1% до MaxDodgeChance
	DodgeChance = math:limit(math:perc(DefendantMf#u_mf.dodge, AttackerMf#u_mf.udodge), 0.01, MaxDodgeChance),

	%% абсолютный уворот
	AbsDodgeChance = math:limit(DefendantMf#u_mf.adodge / 100, 0.0, 1.0),
	%% абсолютный антиуворот
	AbsADodgeChance = math:limit(AttackerMf#u_mf.audodge / 100, 0.0, 1.0),

	%% уворот = (сработал шанс уворота ИЛИ сработал шанс абс. уворота) И не сработал шанс абс. унтиуворота
	(is_happened(DodgeChance) or is_happened(AbsDodgeChance)) and not(is_happened(AbsADodgeChance)).


%% is_counter/2
%% ====================================================================
%% контр-удар
is_counter(_Attacker, Defendant) ->
	DefendantMf = get_mf(Defendant),

	%% минимальный мф контрудара 10%, максимальный - 80%
	CounterChance = math:limit(DefendantMf#u_mf.counter / 100, 0.1, 0.8),

	%% простая проверка на ГСЧ
	is_happened(CounterChance).


%% is_crit/3
%% ====================================================================
%% крит
is_crit(Weapon, Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),

	%% максимальный шанс крита 80%, под своей удачей 90%, под удачей защищающегося - 70%
	%% удача атакера и защитника нивелируют
	MaxCritChance = case AttackerMf#u_mf.luck - DefendantMf#u_mf.luck of
						Lucky  when Lucky  > 0 -> 0.9;
						UnLuky when UnLuky < 0 -> 0.7;
						_ -> 0.8
					 end,
	%% шанс крита = (Крит * 2 - Антикрит) / Крит * 2) в пределах от 1% до MaxCritChance
	CritChance = math:limit(math:perc(Weapon#u_weapon_damage.crit * 2, DefendantMf#u_mf.ucrit), 0.01, MaxCritChance),

	%% абсолютный крит
	AbsCritChance = math:limit(AttackerMf#u_mf.acrit / 100, 0.0, 1.0),
	%% абсолютный антикрит
	AbsACritChance = math:limit(DefendantMf#u_mf.aucrit / 100, 0.0, 1.0),

	%% крит = (сработал шанс крита ИЛИ сработал шанс абс. крита) И не сработал шанс абс. антикрита
	(is_happened(CritChance) or is_happened(AbsCritChance)) and not(is_happened(AbsACritChance)).


%% is_parry/2
%% ====================================================================
%% парир
%% Шанс парирования в бою равен разнице вашего мф. парирования и половины мф. парирования противника.
%% Модификатор парирования теряет эффективность в 1.2 раза за каждый уровень атакующего выше 8.
is_parry(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),

	%% коэффициент уменьшения шанса парира = 1.2 ^ (AttLevel - DefLevel)
	Reduce = math:pow(1.2, max(max(AttackerLevel - 8, 0) - max(DefendantLevel - 8, 0), 0)),
	%% шанс парира = (Свой Парир - Парир Противника/2) в пределах от 0% до 50%
	ParryChance = math:limit(DefendantMf#u_mf.parry - AttackerMf#u_mf.parry / 2, 0.0, 50.0) / Reduce,

	is_happened(ParryChance).


%% is_shield_block/2
%% ====================================================================
%% блок щитом
is_shield_block(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),

	%% коэффициент уменьшения шанса блока  = 1.2 ^ (AttLevel - DefLevel)
	Reduce = math:pow(1.2, max(max(AttackerLevel - 8, 0) - max(DefendantLevel - 8, 0), 0)),
	%% шанс блока = (Свой мф - мф Противника) в пределах от 0% до 50%
	BlockChance = math:limit(DefendantMf#u_mf.block - AttackerMf#u_mf.block / 2, 0.0, 50.0) / Reduce,

	is_happened(BlockChance).


%% get_damage_power/2
%% ====================================================================
%% расчет мощности удара данным типом урона
%% физический урон = мощь урона + мощь профильного урона
%% стихийный урон = мощь урона + мощь профильной магии
%% 1 + (Power / 100)
get_damage_power(Type, User) ->
	1 +
	(case user_helper:is_magic_type(Type) of
		true  -> element(user_helper:get_wpower_index(Type), User#user.wpower);
		false -> element(user_helper:get_dpower_index(Type), User#user.dpower)
	end + (User#user.dpower)#u_dpower.general) / 100.


%% get_base_damage/2
%% ====================================================================
%% расчет базового урона оружием
%% зависимость от типа урона = (Статы * Процент) / 100
%% для двуручки * 1.2
%% 0.075 за каждую умелку
%% Колющий урон зависит от 50% Силы и 40% Ловкости.
%% Рубящий - от 50% Силы, 20% Ловкости и 30% Интуиции.
%% Дробящий - от 80% Силы.
%% Режущий - от 50% Силы и 40% Интуиции.
%% Для посохов на стихийные атаки влияют параметры владения посохами, владения соответствующей магией
%% Для остального оружия маг. урон рассчитывается по формуле: 50% ловкости, 50% силы
get_base_damage(Type, Weapon, User) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_mf(User) ->
	User#user.mfs.

get_level(User) ->
	(User#user.info)#u_info.level.


%% is_happened/1
%% ====================================================================
%% проверка шанса на простом ГСЧ
is_happened(Value) when Value =< 1.0 ->
	Value >= random:uniform();

is_happened(Value) when Value =< 100 ->
	Value >= random:uniform(100).
