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
-export([get_d_interval/1]).

%% расчеты срабатывания МФ
-export([is_dodge/2,
		 is_counter/2,
		 is_crit/3,
		 is_parry/2,
		 is_shield_block/2]).

%% расчеты урона
-export([get_damage/7,
		 get_base_damage/6,
		 get_reduced_damage/6,

		 get_damage_power/2,
		 get_crit_power/2,
		 get_strength_factor/3,
		 get_skill_factor/3,
		 get_armor_damage_reduce/4,
		 get_protection_damage_reduce/4,
		 get_wprotection_damage_reduce/3]).

%% расчеты тактик
-export([get_hearts/3]).

%% get_d_interval/1
%% ====================================================================
%% возвращает интервал (минимум, максимум) для D-value
get_d_interval(DValue) when is_record(DValue, d_value) ->
	Min = DValue#d_value.n + 1,
	Max = DValue#d_value.n + DValue#d_value.k,
	{Min, Max}.


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
	CritChance = math:limit(math:perc(Weapon#u_weapon.crit * 2, DefendantMf#u_mf.ucrit), 0.01, MaxCritChance),

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


%% get_damage/7
%% ====================================================================
%% расчет величины полученного уроном от оружия
%% = (Базовый урон - Броня) - % Защиты от урона
get_damage(HitZone, DamageType, Crit, CritBreak, Attacker, AttackerWeapon, Defendant) ->
	%% считаем базовый урон данным типом атаки
	BaseDamage = formula:get_base_damage(DamageType, Crit, CritBreak, Attacker, AttackerWeapon, Defendant),
	%% реально полученный урон
	RealDamage = formula:get_reduced_damage(BaseDamage, HitZone, DamageType, Attacker, AttackerWeapon, Defendant),
	%?DBG("BASE DAMAGE ~p, REAL DAMAGE ~p~n", [BaseDamage, RealDamage]),
	round(RealDamage).


%% get_base_damage/2
%% ====================================================================
%% расчет базовой величины урона при атаке
%% Y = (B+L+S+W*(1+0,07*U))*(1+M/100) для обычного удара
%% Y = (B+L+S+W*(1+0,07*U))*(1+M/100)*2*(1+K/100) - для критического удара
%% Базовый урон (B) - мин значение 5, макс значение 9 (урон на 0м лвле при силе 3)
%% Уровень (L) - уровень персонажа
%% Урон оружия (W) (минимальный урон оружия добавляется к минимальному, максимальный - к максимальному)
%% Мастерство владения оружием (U) - усиливает только урон оружия, а не весь урон. +7% к урону оружием за каждое владение
%% Мощность урона (M) Мощность урона выбирается в соответствии с профилем атаки
%% Мощность крита (K)Влияние статов
%% (S) - влияние наших статов на профильный урон
%% ====================================================================
get_base_damage(DamageType, Crit, CritBreak, Attacker, AttackerWeapon, Defendant) ->
	%% считаем мощность урона данным типом оружия
	DamagePower = get_damage_power(DamageType, Attacker),
	%% мощность крита
	CritPower   = case Crit of true -> get_crit_power(Attacker, Defendant); false -> 1 end,
	%% снижение удара за счет пробива блока в 2 раза
	CritReduce = case Crit and CritBreak of true -> 2; false -> 1 end,
	%% расчет силы удара в зависимости от статов
	StrengthFactor = get_strength_factor(DamageType, AttackerWeapon, Attacker),
	%% фактор влияния умелок
	SkillFactor = get_skill_factor(DamageType, AttackerWeapon, Attacker),
	%% коэффициент увеличения удара за счет уровня персонажа
	AttackerLevel = get_level(Attacker),

	%% интервал урона оружия
	{WeaponMinimum, WeaponMaximum} = get_d_interval(AttackerWeapon#u_weapon.damage),
	%?DBG("~p~n", [{WeaponMinimum, WeaponMaximum, AttackerWeapon#u_weapon.damage}]),

	%% итоговый минимальный урон
	Minimum = trunc((((5 + AttackerLevel + StrengthFactor + WeaponMinimum * SkillFactor)) *
				  DamagePower * CritPower) / CritReduce),
	%% итоговый максимальный урон
	Maximum = trunc((((9 + AttackerLevel + StrengthFactor + WeaponMaximum * SkillFactor)) *
				  DamagePower * CritPower) / CritReduce),

	%% берем случайное значение урона в полученном интервале от минимального до максимального
	%% расчет на ГСЧ с учетом "удачи" атакующего
	get_from_interval({Minimum, Maximum}, (Attacker#user.mfs)#u_mf.luck).



%% get_reduced_damage/6
%% ====================================================================
%% расчет реально полученного урона за счет снижения его защитой и броней
get_reduced_damage(BaseDamage, Hit, DamageType, Attacker, AttackerWeapon, Defendant) ->
	%% расчитываем кол-во урона, поглощенного броней
	ArmorReduce = formula:get_armor_damage_reduce(Hit, DamageType, AttackerWeapon, Defendant),
	%% расчитываем процент урона, поглощенного защитой
	ProtectionReduce = formula:get_protection_damage_reduce(Hit, DamageType, Attacker, Defendant),
	%?DBG("Protection ~p~n", [ProtectionReduce]),
	%% броня не может снизить урон более чем на треть
	%% урон не может уйти в минус
	max(max(BaseDamage - ArmorReduce, BaseDamage/3) *
		(1 - ProtectionReduce/100), 0).


%% get_damage_power/2
%% ====================================================================
%% расчет мощности удара данным типом урона
%% физический урон = мощь урона + мощь профильного урона
%% стихийный урон = мощь урона + мощь профильной магии
%% 1 + (Power / 100)
get_damage_power(Type, User) ->
	1 +
	(case user_helper:is_magic_type(Type) of
		true  -> user_helper:get_wpower(Type, User);
		false -> user_helper:get_dpower(Type, User)
	end + (User#user.dpower)#u_dpower.general) / 100.


%% get_crit_power/2
%% ====================================================================
%% расчет мощности крита
%% расчитывается как разница м/у мф мощности крита атакующего и
%% мф. против мощности крита защищающегося
%% 2 * (1 + (Ka - Kd) /100)
get_crit_power(Attacker, Defendant) ->
	2 * (1 + (max((Attacker#user.dpower)#u_dpower.crit -
				  (Defendant#user.dprotection)#u_dprotection.crit, 0)) / 100).


%% get_base_damage/3
%% ====================================================================
%% расчет силы удара в зависимости от статов
%% зависимость от типа урона = (Статы * Процент) / 100
%% Колющий - 60% Силы и 40% Ловкости.
%% Рубящий - 70% Силы 20% Ловкости и 20% Интуиции.
%% Дробящий - 100% Силы.
%% Режущий - 60% Силы и 40% Интуиции.
%% Для остального оружия маг. урон рассчитывается по формуле: 50% ловкости, 50% силы
get_strength_factor(Type, _Weapon, User) ->
	Stats = User#user.stats,
	case user_helper:is_magic_type(Type) of
		true  -> Stats#u_stats.str * 0.5 + Stats#u_stats.agil * 0.5;
		false ->
			case Type of
				prick -> Stats#u_stats.str * 0.6 + Stats#u_stats.agil * 0.4;
				chop  -> Stats#u_stats.str * 0.7 + Stats#u_stats.agil * 0.2 + Stats#u_stats.int * 0.2;
				crush -> Stats#u_stats.str * 1.0;
				cut   -> Stats#u_stats.str * 0.6 + Stats#u_stats.int * 0.4
			end
	end.


%% get_skill_factor/3
%% ====================================================================
%% расчет фактора влияния на урон умелок в оружии и магии
%% 0.075 за каждую умелку
%% Для посохов на стихийные атаки влияют параметры владения посохами, владения соответствующей магией
%% для двуручки * 1.2
get_skill_factor(Type, Weapon, User) ->
	%% фактор двуручного оружия
	TwainFactor = case Weapon#u_weapon.twain of true -> 1.2; false -> 1 end,
	%% фактор умелок в оружии
	1 + (user_helper:get_skill(Weapon#u_weapon.type, User) * 0.075 +
		%% фактор умелок в магии для посоха
		case user_helper:is_magic_type(Type) and (Weapon#u_weapon.type == staff) of
			true -> user_helper:get_skill(Type, User) * 0.075;
			false -> 0
		end) * TwainFactor.



%% get_armor_damage_reduce/4
%% ====================================================================
%% расчет кол-ва урона, поглощаемого броней противника
get_armor_damage_reduce(HitZone, DamageType, AttackerWeapon, Defendant) ->
	%% магический урон полностью игнорирует броню
	case user_helper:is_magic_type(DamageType) of
		true  -> 0;
		false ->
			%% базовые значения брони от данного типа урона в данную зону
			BaseArmor = get_d_interval(user_helper:get_zone_armor(HitZone, Defendant)),
			{MinArmor, _} = BaseArmor,
			%% значение брони в данной зоне на основе ГСЧ с удачей
			Armor = get_from_interval(BaseArmor, (Defendant#user.mfs)#u_mf.luck),
			%% обработка пробоя брони
			%% считаем пробой как процент подавления брони, но не ниже минимального ее значения
			%% (Br - Br * Pr/100).
			max(Armor - Armor * AttackerWeapon#u_weapon.pierce / 100, MinArmor)
	end.


%% get_protection_damage_reduce/4
%% ====================================================================
%% расчет кол-ва урона, поглощаемого защитой
%% Каждые 250 ед. увеличивает существующую защиту на 50% (т.е. при 250 ед. будет 50% защиты, при 500 - 75%, при 750 - 87.5%)
%% X = 1-0.5^(N/250))*100
%% предельное значение защиты от оружия - 1000, от магии - 1400
%% Защита теряет эффективность в 1.2 раза за каждый уровень атакующего выше 8.
get_protection_damage_reduce(Hit, DamageType, Attacker, Defendant) ->
	Protection = case user_helper:is_natural_type(DamageType) of
		%% если удар физический считаем защиту от урона
		true ->
			%% показатель защиты в данной точке от данного урона
			min(user_helper:get_zone_protection(Hit, DamageType, Defendant), 1000);

		%% если удар магический - считаем защиту от магии
		false ->
			%% показатель защиты от данного типа магии
			min(user_helper:get_wprotection(DamageType, Defendant), 1400)
	end,

	%% коэффициент уменьшения защиты = 1.2 ^ (AttLevel - DefLevel)
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),
	Reduce = math:pow(1.2, max(max(AttackerLevel - 8, 0) - max(DefendantLevel - 8, 0), 0)),

	math:precision((1 - math:pow(0.5, ((Protection / Reduce) / 250))) * 100, 2).


%% get_wprotection_damage_reduce/3
%% ====================================================================
%% расчет кол-ва урона магией, поглощаемого защитой от магии
%% с учетом мф подавы магии
get_wprotection_damage_reduce(DamageType, Attacker, Defendant) ->
	ok.


%% get_hearts/3
%% ====================================================================
%% расчет кол-ва тактики "сердец", полученных за нанесенный урон
%% Для персонажей 1-7 уровня одно сердце начисляется за нанесение врагу урона, равного 10% НР противника.
%% Для персонажей 8 уровня и выше действуют следующие правила:
%% если у противника 8го уровня менее 1000 НР, то за 10%НР начисляется 1 сердце, как и на младших уровнях.
%% А если более 1000 НР, то 10 сердец будет начисляться за каждую выбитую из него 1000 НР.
%% Для 9го уровня, это соответственно - 1200 НР, для 10го - 1440 НР, для 11го – 1728 НР.
get_hearts(Damage, Attacker, Defendant) ->
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),
	DefendantMaxHp = (Defendant#user.vitality)#u_vitality.maxhp,

	case AttackerLevel < 8 of
		true  -> math:precision(Damage / (DefendantMaxHp / 10), 2);
		false ->
			Base = get_hearts_base(DefendantLevel),
			case DefendantMaxHp < Base of
				true  -> math:precision(Damage / (DefendantMaxHp / 10), 2);
				false -> math:precision(Damage / (Base / 10), 2)
			end
	end.


get_hearts_base(8) -> 1000;
get_hearts_base(9) -> 1200;
get_hearts_base(10) -> 1440;
get_hearts_base(_) -> 1728.

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


%% get_from_interval/1
%% ====================================================================
%% получение случайного значения в заданном интервале, учитывая "удачу"
get_from_interval(DValue, Luck) when is_record(DValue, d_value) ->
	get_from_interval(get_d_interval(DValue), Luck);

get_from_interval({Min, Max}, 0) ->
	Min + (random:uniform((Max - Min) + 1) - 1);

get_from_interval({Min, Max}, _Luck) ->
	%% @todo use gaussian
	get_from_interval({Min, Max}, 0).
