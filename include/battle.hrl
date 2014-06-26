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
                block :: [head | torso | paunch | belt | legs]
       }).

%% участник боя
-record(b_unit, {id,
                 name,
                 battle_id = 0,
                 team_id = 0,
                 user = #user{},
                 alive = true,
                 leader = false,
                 opponent = undefined :: pid() | undefined,			%% выбранный в текущий момент оппонент
                 obtained :: [{Opponent :: pid(), Hit :: pid()}],	%% список оппонентов, выставивших удары
                 hits     :: [{Opponent :: pid(), Hit :: pid()}],	%% список оппонентов, которым выставил удар
                 tactics = #'b_tactics'{}		%% боевые тактики
       }).

%% команда
-record(b_team, {id,
                 battle_id = 0,
                 max_cost = 0,
                 units = []	:: [#b_unit{}],
                 units_count = 0,
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
                 teams = []	:: [#b_team{}]
       }).

