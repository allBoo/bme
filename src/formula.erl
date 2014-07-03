%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Расчеты боевки
%%% http://www.darkclan.ru/cgi/lib.pl?p=boevka
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
		 is_shield_block/2]).


%% is_dodge/2
%% ====================================================================
%% уворот
is_dodge(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),

	%% разница м/у мф уворота защищающегося и мф антиуворота атакующего
	Delta1 = math:limit(DefendantMf#u_mf.dodge - AttackerMf#u_mf.udodge, DefendantMf#u_mf.dodge),

	false.


%% is_counter/2
%% ====================================================================
%% контр-удар
is_counter(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	false.


%% is_crit/3
%% ====================================================================
%% крит
is_crit(Weapon, Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	false.


%% is_parry/2
%% ====================================================================
%% парир
%% Вероятность парирования определяется разницей модификаторов парирования противников, но не превышает 50%.
%% Модификаторы парирования и блока щитом теряют эффективность в 1.2 раза за каждый уровень атакующего выше 8.
is_parry(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),
	false.


%% is_shield_block/2
%% ====================================================================
%% блок щитом
is_shield_block(Attacker, Defendant) ->
	AttackerMf  = get_mf(Attacker),
	DefendantMf = get_mf(Defendant),
	AttackerLevel  = get_level(Attacker),
	DefendantLevel = get_level(Defendant),
	false.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_mf(User) ->
	User#user.mfs.

get_level(User) ->
	(User#user.info)#u_info.level.

