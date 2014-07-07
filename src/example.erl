%% -*- coding: utf-8 -*-

-module(example).

-include_lib("bme.hrl").

-export([get/1, get1/0, get2/0, get3/0]).

get1() ->
	#user{
		id    = 104962,
		name = <<"Кошмарский/utf8">>,
		city  = 1,
		room  = 1,
		info  = #'u_info'{
			level = 10,
			align = 0.98,
			klan  = <<"DarkRunes/utf8">>,
			pic   = <<"indiv_104962/utf8">>,
			sex   = male
		},
		dress  = #'u_dress'{
			cost = 4500
		},
		vitality = #'u_vitality'{
			hp      = 2135,    %% уровень жизни
			maxhp   = 2135,    %% максимальный уровень жизни
			mana    = 4355,    %% уровень маны
			maxmana = 4355     %% максимальный уровень маны
		},
		stats = #'u_stats'{
			str   = 100,    %% сила
			agil  = 30,     %% ловкость
			int   = 10,    %% интуиция
			dex   = 35,    %% вынос
			intel = 204,   %% интеллект
			wisd  = 75,    %% мудрость
			spir  = 50     %% духовность
		},
		mfs = #'u_mf'{
			ucrit   = 800.0,   %% анти-крит
			acrit   = 0.0,     %% абсолютный крит
			dodge   = 150.0,   %% уворот
			udodge  = 100.0,   %% анти-уворот
			adodge  = 0.0,     %% асолютный уворот
			counter = 5.0,     %% контр-удар
			parry   = 5.0,     %% парир
			block   = 0.0,     %% блок щитом
			luck    = 300.0    %% удача
		},
		damage = #'u_damage'{
			left = #'u_weapon'{
				damage = #'d_value'{n=60, k=16},  %% базовый урон
				damage_type = #'u_damage_type'{   %% тип урона
					crush = 0.7,  %% шанс дробящего урона
					air   = 0.3   %% шанс стихийной атаки воздухом
				},
				crit   = 100.0,              %% мф крита
				pierce = 5.0,                %% мф пробоя брони
				type = staff
			},
			right = undefined
		},
		dpower = #'u_dpower'{
			general = 10.0,   %% мощь общего урона
			crush   = 100.0,   %% мощь дробящего урона
			crit    = 150.0    %% мощь крита
		},
		wpower = #'u_wpower'{
			general = 400.0,  %% мощь общей магии
			air     = 450.0,  %% мощь магии воздуха
			fire    = 400.0,  %% мощь магии огня
			water   = 400.0,  %% мощь магии воды
			earth   = 400.0,  %% мощь магии земли
			light   = 400.0,  %% мощь магии света
			dark    = 400.0,  %% мощь магии тьмы
			gray    = 400.0,  %% мощь серой магии
			reduction = 78.0  %% подавление защиты от магии
		},
		skills = #'u_skills'{
			staff = 3,
			air   = 18,
			gray  = 1,
			light = 4
		},
		armor  = #'u_armor'{
			head   = #'d_value'{n=14, k=10},    %% броня головы
			torso  = #'d_value'{n=8, k=24},     %% броня груди
			belt   = #'d_value'{n=6, k=24},     %% броня пояса
			legs   = #'d_value'{n=9, k=23}      %% броня ног
		},
		dprotection = #'u_dprotection'{
			head   = #'u_dprotection_zone'{    %% броня головы
				general = 992.5,   %% защита от общего урона
				prick   = 992.5,   %% защита от колющего урона
				chop    = 992.5,   %% защита от рубящего урона
				crush   = 992.5,   %% защита от дробящего урона
				cut     = 992.5    %% защита от режущего урона
			},
			torso  = #'u_dprotection_zone'{    %% броня корпуса
				general = 967.5,   %% защита от общего урона
				prick   = 967.5,   %% защита от колющего урона
				chop    = 967.5,   %% защита от рубящего урона
				crush   = 967.5,   %% защита от дробящего урона
				cut     = 967.5    %% защита от режущего урона
			},
			belt   = #'u_dprotection_zone'{    %% броня пояса
				general = 907.5,   %% защита от общего урона
				prick   = 907.5,   %% защита от колющего урона
				chop    = 907.5,   %% защита от рубящего урона
				crush   = 907.5,   %% защита от дробящего урона
				cut     = 907.5    %% защита от режущего урона
			},
			legs   = #'u_dprotection_zone'{    %% броня ног
				general = 982.5,   %% защита от общего урона
				prick   = 982.5,   %% защита от колющего урона
				chop    = 982.5,   %% защита от рубящего урона
				crush   = 982.5,   %% защита от дробящего урона
				cut     = 982.5    %% защита от режущего урона
			},
			crit   = 5.0    %% мф против мощи критического урона
		},
		wprotection = #'u_wprotection'{
			general = 1171.5,   %% защита от общей магии
			air     = 1271.5,   %% защита от магии воздуха
			fire    = 1271.5,   %% защита от магии огня
			water   = 1271.5,   %% защита от магии воды
			earth   = 1271.5,   %% защита от магии земли
			light   = 1171.5,   %% защита от магии света
			dark    = 1171.5,   %% защита от магии тьмы
			gray    = 1171.5    %% защита от серой магии
		},
		battle_spec = #'u_battle_spec'{
			hit_points = 2,     %% кол-во точек удара
			magic_points = 2,   %% кол-во магических кастов
			block_points = 3    %% кол-во зон блока
		}
	}.


get2() ->
	User = get1(),
	User#user{id = 10000104962, name = <<"Кошмарский клон1/utf8">>, dress = (User#user.dress)#u_dress{cost = 4499}}.

get3() ->
	User = get1(),
	User#user{id = 10001104962, name = <<"Кошмарский клон2/utf8">>, dress = (User#user.dress)#u_dress{cost = 100}}.

get(Id) ->
	User = get1(),
	Cost = random:uniform((User#user.dress)#u_dress.cost),
	CloneIndex = integer_to_binary(Id),
	User#user{id = Id,
			  ai = true,
			  name = <<(User#user.name)/binary, <<" клон/utf8">>/binary, CloneIndex/binary >>,
			  dress = (User#user.dress)#u_dress{cost = Cost}}.
