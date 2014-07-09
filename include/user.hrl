%% -*- coding: utf-8 -*-
%% uses defines http://www.darkclan.ru/cgi/lib.pl?p=boevka

-ifndef(USER_HRL).
-define(USER_HRL, true).

-define(vitality(User), User#user.vitality).
-define(hp(User), (User#user.vitality)#u_vitality.hp).
-define(maxhp(User), (User#user.vitality)#u_vitality.maxhp).
-define(mana(User), (User#user.vitality)#u_vitality.mana).
-define(maxmana(User), (User#user.vitality)#u_vitality.maxmana).
-define(level(User), (User#user.info)#u_info.level).
-define(sex(User), (User#user.info)#u_info.sex).
-define(mfs(User), User#user.mfs).
-define(drcost(User), (User#user.dress)#u_dress.cost).


%% d-value (N+Kd)
-record(d_value, {n = 0.0, k = 0.0}).

%% информация о персонаже
-record(u_info, {level = 0,
                 align = 0.0,
                 klan  = <<""/utf8>>,
                 pic   = <<""/utf8>>,
                 sex   :: male | female
       }).

%% описание шмоток
-record(u_dress_item, {name = <<"">>
       }).

%% одетые шмотки
-record(u_dress, {cost = 0,
                  wreath = #'u_dress_item'{},
                  head = #'u_dress_item'{},
                  braslet = #'u_dress_item'{},
                  weapon_left = #'u_dress_item'{},
                  shirt = #'u_dress_item'{},
                  armor = #'u_dress_item'{},
                  cloak = #'u_dress_item'{},
                  belt = #'u_dress_item'{},
                  earrings = #'u_dress_item'{},
                  amulet = #'u_dress_item'{},
                  ring0 = #'u_dress_item'{},
                  ring1 = #'u_dress_item'{},
                  ring2 = #'u_dress_item'{},
                  weapon_right = #'u_dress_item'{},
                  leggins = #'u_dress_item'{},
                  shoes = #'u_dress_item'{}
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
               aucrit  = 0.0,   %% абсолютный анти-крит
               acrit   = 0.0,   %% абсолютный крит
               dodge   = 0.0,   %% уворот
               udodge  = 0.0,   %% анти-уворот
               audodge = 0.0,   %% абсолютный анти-уворот
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

%% характеристика оружия
-record(u_weapon, {damage      = #'d_value'{},       %% базовый урон
                   damage_type = #'u_damage_type'{}, %% тип урона
                   crit        = 0.0,                %% мф крита
                   pierce      = 0.0,                %% мф пробоя брони
                   type       :: [knife | axe | hammer | sword | staff | arm | nails], %% тип оружия
                   twain       = false               %% двуручка
       }).

%% показатель базового урона оружием (левая и правая руки)
-record(u_damage, {left = #'u_weapon'{}, right = #'u_weapon'{}}).

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

%% уменя владения оружием и магией
-record(u_skills, {knife  = 0,     %% Владение ножами и кинжалами
                   axe    = 0,     %% Владение топорами и секирами
                   hammer = 0,     %% Владение молотами и дубинами
                   sword  = 0,     %% Владение мечами
                   staff  = 0,     %% Владение посохами
                   fire   = 0,     %% Владение магией Огня
                   air    = 0,     %% Владение магией Воздуха
                   water  = 0,     %% Владение магией Воды
                   earth  = 0,     %% Владение магией Земли
                   light  = 0,     %% Владение магией Света
                   dark   = 0,     %% Владение магией Тьмы
                   gray   = 0      %% Владение серой магией
       }).

%% броня
-record(u_armor, {head   = #'d_value'{},    %% броня головы
                  torso  = #'d_value'{},    %% броня корпуса (грудь, живот)
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

%% боевая спецификация юзера
-record(u_battle_spec, {hit_points = 1,    %% кол-во точек удара
                        magic_points = 1,  %% кол-во магических кастов
                        block_points = 2   %% кол-во зон блока
        }).


%% инфа о наложенных баффах
-record(u_buff, {id :: atom(),
                 time :: non_neg_integer() | infinity
        }).

%% информация о персонаже
-record(user, {id,
               name,
               ai = false,
               ai_level = 0,
               city,
               room,
               info  = #'u_info'{},		%% информация о персонаже
               dress = #'u_dress'{},	%% шмот
               vitality = #'u_vitality'{},	%% уровни ХМ и маны
               stats = #'u_stats'{},	%% статы
               mfs = #'u_mf'{},			%% модификаторы
               damage = #'u_damage'{},	%% базовый урон для левой и правой руки
               dpower = #'u_dpower'{},	%% мощь урона
               wpower = #'u_wpower'{},	%% мощь магии
               skills = #'u_skills'{},	%% умелки
               armor  = #'u_armor'{},	%% броня
               dprotection = #'u_dprotection'{}, 	%% защита от урона
               wprotection = #'u_wprotection'{},	%% защита от магии
               battle_spec = #'u_battle_spec'{},	%% боевые спецификации
               buffs = [] :: [#u_buff{}]			%% наложенные баффы
%% одетые приемы
%% одетые заклинания
%% баффы
       }).


-endif.
