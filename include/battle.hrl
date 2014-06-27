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
-record(b_hit, {sender :: pid(),
                recipient :: pid(),
                hits = [] :: [head | torso | paunch | belt | legs],
                block :: head | torso | paunch | belt | legs,
                timeout = 5
       }).

%% краткое представление оппонента в бою
-record(b_opponent, {pid :: pid(),
                     team :: pid(),
                     id  :: integer(),
                     name :: binary(),
                     level :: non_neg_integer(),
                     align,
                     klan :: binary(),
                     cost = 0,
                     gray = false ::boolean()
       }).

%% участник боя
-record(b_unit, {id,
                 name,
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
                 tactics = #'b_tactics'{}		%% боевые тактики
       }).

%% команда
-record(b_team, {id,
                 battle_id = 0,
                 battle_pid :: pid(),
                 max_cost = 0,
                 units = []	:: [#b_unit{}],
                 units_count = 0,
                 alive_units = [],
                 alive_count = 0,
                 leader
       }).


%% информация о бое
-record(battle, {id,
                 started_at,
                 type,
                 blood,
                 city,
                 room,
                 level = #b_level{},
                 timeout,
                 status,
                 teams = []	:: [#b_team{}],
                 alive_teams = []
       }).

