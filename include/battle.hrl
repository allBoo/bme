%% -*- coding: utf-8 -*-

-include_lib("user.hrl").

%% уровень персонажей поединка
-record(b_level, {min = 0  :: integer(),
                  max = 21 :: integer()
        }).

%% информация о бое
-record(battle, {id,
                 type,
                 blood,
                 city,
                 room,
                 timeout,
                 started_at,
                 teams = []
       }).

%% команда
-record(b_team, {id,
                 max_cost = 0,
                 members = []
       }).
