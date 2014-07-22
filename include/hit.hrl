%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Все что связано с ударами, кастами, уроном
%%% ====================================================================

%% удар
-record(b_hit, {sender     :: pid(),
                recipient  :: pid(),
                battle_id  :: non_neg_integer(),
                hits = []  :: [head | torso | paunch | belt | legs | none | counter],
                block = [] :: [head | torso | paunch | belt | legs | none],
                timeout = 5,
                timeout_alert = false :: boolean(),
                timeout_pass = false,
                magic_pass = false
       }).

%% результат удара
-record(b_hit_result, {hit :: head | torso | paunch | belt | legs | none,
                       blocks :: [head | torso | paunch | belt | legs | none],
                       attacker :: pid(),
                       defendant :: pid(),
                       damage :: integer(),
                       damage_type,
                       weapon_type,
                       weapon_twain = false,
                       hited = false,
                       crit = false,
                       crit_break = false,
                       dodge = false,
                       counter = false,
                       parry = false,
                       block = false,
                       shield = false,
                       transaction
        }).


%% результат удара
-record(b_magic_attack, {damage,
                         damage_type,
                         buff,
                         attacker,
                         defendant,
                         transaction
        }).


%% очередь ударов
-record(b_hit_queue, {attacker_pid,    %% PID атакующего
                      hit,             %% зона удара
                      attacker_block,  %% зоны блоков атакующего
                      defendant_pid,   %% PID защищающегося
                      defendant_block, %% зоны блоков защищающегося
                      index            %% номер удара в размене
       }).

%% результат хилла
-record(b_heal, {value = 0,
                 buff,
                 trick,
                 use_spirit = true,
                 sender,
                 recipient,
                 transaction
        }).


%% заклинание
-record(spell, {name}).
