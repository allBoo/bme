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
                 sides = []
       }).

%% команда
-record(b_team, {id,
                 color,
                 members = []
       }).


%% боевые тактики
-record(b_tactics, {attack  = 0,
					crit    = 0,
					counter = 0,
					block   = 0,
					parry   = 0,
					hearts  = 0
       }).


%% участник боя
-record(b_member, {id,
				   user   = #user{},
				   alive  = 1,
				   spirit = 0,
				   tactics = #'b_tactics'{}
       }).

