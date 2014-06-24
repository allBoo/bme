%% -*- coding: utf-8 -*-
%% uses defines http://www.darkclan.ru/cgi/lib.pl?p=boevka

-ifndef(USER_HRL).
-define(USER_HRL, true).

%% d-value (N+Kd)
-record(d_value, {n = 0.0, k = 0.0}).

%% информация о персонаже
-record(u_info, {level = 0,
                 align = 0.0,
                 klan  = <<"">>,
                 pic   = <<"">>,
                 sex   = 0
       }).

%% описание шмоток
-record(u_clother_item, {name = <<"">>
       }).

%% одетые шмотки
-record(u_clother, {cost = 0,
				   head = #'u_clother_item'{}
       }).

%% уровни ХМ и маны
-record(u_vitality, {hp      = 0,    %% уровень жизни
                     maxhp   = 0,    %% максимальный уровень жизни
                     mana    = 0,    %% уровень маны
                     maxmana = 0     %% максимальный уровень маны
       }).

%% статы
-record(u_stats, {str   = 3,   %% сила
                  agil  = 3,   %% ловкость
                  int   = 3,   %% интуиция
                  dex   = 3,   %% вынос
                  intel = 0,   %% интеллект
                  wisd  = 0,   %% мудрость
                  spir  = 0    %% духовность
       }).

%% общие модификаторы персонажа, не влияющие на урон
-record(u_mf, {%%crit    = 0.0,   %% крит @todo это параметр оружия
               ucrit   = 0.0,   %% анти-крит
               acrit   = 0.0,   %% абсолютный крит
               dodge   = 0.0,   %% уворот
               udodge  = 0.0,   %% анти-уворот
               adodge  = 0.0,   %% асолютный уворот
               counter = 0.0,   %% контр-удар
               parry   = 0.0,   %% парир
               block   = 0.0,   %% блок щитом
               luck    = 0.0    %% удача
       }).

%% тип урона оружием
-record(u_damage_type, {prick = 0.0,  %% шанс колющего урона
                        chop  = 0.0,  %% шанс рубящего урона
                        crush = 0.0,  %% шанс дробящего урона
                        cut   = 0.0,  %% шанс режущего урона
                        air   = 0.0,  %% шанс стихийной атаки воздухом
                        fire  = 0.0,  %% шанс стихийной атаки огнем
                        water = 0.0,  %% шанс стихийной атаки водой
                        earth = 0.0,  %% шанс стихийной атаки землей
                        light = 0.0,  %% шанс стихийной атаки светом
                        dark  = 0.0,  %% шанс стихийной атаки тьмой
                        gray  = 0.0   %% шанс стихийной атаки серой магией
        }).

%% характеристика урона оружия
-record(u_weapon_damage, {damage = #'d_value'{},      %% базовый урон
                          crit   = 0.0,               %% мф крита
                          pierce = 0.0,               %% мф пробоя брони
                          type   = #'u_damage_type'{} %% тип урона
       }).

%% показатель базового урона оружием (левая и правая руки)
-record(u_damage, {left = #'u_weapon_damage'{}, right = #'u_weapon_damage'{}}).

%% мф мощности урона
-record(u_dpower, {general = 0.0,   %% мощь общего урона
                   prick   = 0.0,   %% мощь колющего урона
                   chop    = 0.0,   %% мощь рубящего урона
                   crush   = 0.0,   %% мощь дробящего урона
                   cut     = 0.0,   %% мощь режущего урона
                   crit    = 0.0    %% мощь критического урона
       }).

%% мф мощности магии
-record(u_wpower, {general = 0.0,  %% мощь общей магии
                   air     = 0.0,  %% мощь магии воздуха
                   fire    = 0.0,  %% мощь магии огня
                   water   = 0.0,  %% мощь магии воды
                   earth   = 0.0,  %% мощь магии земли
                   light   = 0.0,  %% мощь магии света
                   dark    = 0.0,  %% мощь магии тьмы
                   gray    = 0.0,  %% мощь серой магии
                   reduction = 0.0 %% подавление защиты от магии
       }).

%% броня
-record(u_armor, {head   = #'d_value'{},    %% броня головы
                  torso  = #'d_value'{},    %% броня корпуса
                  belt   = #'d_value'{},    %% броня пояса
                  legs   = #'d_value'{}     %% броня ног
       }).

%% защита от оружия
-record(u_dprotection_zone, {general = 0.0,   %% защита от общего урона
                             prick   = 0.0,   %% защита от колющего урона
                             chop    = 0.0,   %% защита от рубящего урона
                             crush   = 0.0,   %% защита от дробящего урона
                             cut     = 0.0    %% защита от режущего урона
       }).
-record(u_dprotection, {head   = #'u_dprotection_zone'{},    %% броня головы
                        torso  = #'u_dprotection_zone'{},    %% броня корпуса
                        belt   = #'u_dprotection_zone'{},    %% броня пояса
                        legs   = #'u_dprotection_zone'{},    %% броня ног
                        crit   = 0.0    %% мф против мощи критического урона
       }).


%% защита от магии
-record(u_wprotection, {general = 0.0,   %% защита от общей магии
                        air     = 0.0,   %% защита от магии воздуха
                        fire    = 0.0,   %% защита от магии огня
                        water   = 0.0,   %% защита от магии воды
                        earth   = 0.0,   %% защита от магии земли
                        light   = 0.0,   %% защита от магии света
                        dark    = 0.0,   %% защита от магии тьмы
                        gray    = 0.0    %% защита от серой магии
       }).


%% боевые тактики
-record(b_tactics, {attack  = 0,
                    crit    = 0,
                    counter = 0,
                    block   = 0,
                    parry   = 0,
                    hearts  = 0,
                    spirit  = 0
       }).


%% информация о персонаже
-record(user, {id,
               login,
               city,
               room,
               alive = 1,
               info  = #'u_info'{},		%% информация о персонаже
               clother = #'u_clother'{},	%% шмот
               vitality = #'u_vitality'{},	%% уровни ХМ и маны
               stats = #'u_stats'{},	%% статы
               mfs = #'u_mf'{},			%% модификаторы
               damage = #'u_damage'{},	%% базовый урон для левой и правой руки
               dpower = #'u_dpower'{},	%% мощь урона
               wpower = #'u_wpower'{},	%% мощь магии
               armor  = #'u_armor'{},	%% броня
               dprotection = #'u_dprotection'{}, 	%% защита от урона
               wprotection = #'u_wprotection'{},	%% защита от магии
               tactics = #'b_tactics'{}				%% боевые тактики
%% умелки
%% одетые приемы
%% одетые заклинания
%% баффы
%% боевые параметры (точек удара, блока, кол-во кастов)
       }).


-endif.
