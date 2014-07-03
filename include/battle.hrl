%% -*- coding: utf-8 -*-

-include_lib("user.hrl").

%% уровень персонажей поединка
-record(b_level, {min = 0  :: integer(),
                  max = 21 :: integer()
        }).

%% боевые тактики
-record(b_tactics, {attack  = 0,
                    crit    = 0,
                    counter = 0,
                    block   = 0,
                    parry   = 0,
                    hearts  = 0,
                    spirit  = 0,
                    changes = 3
       }).

%% удар
-record(b_hit, {sender     :: pid(),
                recipient  :: pid(),
                hits = []  :: [head | torso | paunch | belt | legs | none | counter],
                block = [] :: [head | torso | paunch | belt | legs | none],
                timeout = 5,
                timeout_alert = false :: boolean(),
                timeout_pass = false,
                magic_pass = false
       }).

%% краткое представление оппонента в бою
-record(b_opponent, {pid :: pid(),
                     team :: pid(),
                     id  :: integer(),
                     name :: binary(),
                     ai = false :: boolean(),
                     level :: non_neg_integer(),
                     align,
                     klan :: binary(),
                     cost = 0,
                     gray = false ::boolean(),
                     timeout = false
       }).

%% участник боя
-record(b_unit, {id,
                 name,
                 ai = false,
                 battle_id = 0,
                 battle_pid :: pid(),
                 team_id = 0,
                 team_pid :: pid(),
                 user = #user{},
                 alive = true,
                 leader = false,
                 opponents = [] :: [#b_opponent{}],	%% список с краткой информацией о всех оппонентах
                 opponent = undefined :: #b_opponent{} | undefined,			%% выбранный в текущий момент оппонент
                 obtained = [] :: [{Opponent :: pid(), Hit :: pid()}],	%% список оппонентов, выставивших удары
                 hits     = [] :: [{Opponent :: pid(), Hit :: pid()}],	%% список оппонентов, которым выставил удар
                 tactics = #'b_tactics'{},		%% боевые тактики
                 total_damaged = 0,		%% кол-во нанесенного урона
                 total_healed = 0,		%% кол-во отхиленных ХП
                 total_lost = 0,		%% кол-во полученного урона
                 exp = 0				%% кол-во полученного опыта
       }).

%% команда
-record(b_team, {id,
                 battle_id = 0,
                 battle_pid :: pid(),
                 max_cost = 0,
                 units = []	:: [#b_unit{} | pid()],
                 units_count = 0,
                 alive_units = [] :: [pid()],
                 alive_count = 0,
                 leader
       }).


%% информация о бое
-record(battle, {id,
                 started_at,
                 type  = battle :: battle | haot | dungeon | tower | klan,
                 blood = false  :: boolean(),
                 city,
                 room,
                 level = #b_level{},
                 timeout = 5,
                 status = 0,
                 teams = []	:: [#b_team{}],
                 alive_teams = []
       }).


%% результат размена
-record(b_damage, {damaged     = 0 :: non_neg_integer(),   %% кол-во нанесенного урона
                   healed      = 0 :: non_neg_integer(),   %% кол-во отхиленных ХП
                   lost        = 0 :: integer(),           %% кол-во полученного урона - разница м/у полученным и отхиленным (может быть отрицательным, т.е. повышение ХП)
                   lost_mana   = 0 :: integer(),           %% кол-во потраченной маны (может быть отрицательным, т.е. повышение маны)
                   tactics     = #b_tactics{},             %% кол-во полученных тактик (может быть отрицательным у духа)
                   opponent_id = 0 :: non_neg_integer(),   %% ID юзера, с которым был размен
                   exp         = 0 :: integer()            %% кол-во экспы, полученное за размен (может быть отрицательным)
        }).

%% результат боя
-record(b_result, {winner,
				   exp_coef = 1.0}).

