%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% приемы
%%% ====================================================================

-define(TRICK_EV(Unit), {?GSI("trick_ev_", Unit#b_unit.id), {trick_mgr, start_ev, [Unit]}, transient, 5000, worker, dynamic}).
-define(TRICK_MGR(Unit), {?GSI("trick_mgr_", Unit#b_unit.id), {trick_mgr, start_mgr, [Unit]}, transient, 5000, worker, [trick_mgr]}).

%% классы приемов (для блокировки одновременного использования)
-type trick_class() :: novice | spirit_weapon | spirit_magic | hp.

%% типы приемов (прием/заклинание)
-type trick_type() :: trick | spell.

%% типы целей (дружественные, вражеские, любые)
-type trick_target_type() :: friendly | enemy | any.

%% описание приемов
-record(trick, {id                              :: atom(),              %% ID
                name           = <<""/utf8>>    :: bitstring(),         %% Название
                decription     = <<""/utf8>>    :: bitstring(),         %% Описание
                class                           :: trick_class(),       %% Класс приема
                type                            :: trick_type(),        %% Тип приема
                tactics        = #b_tactics{}   :: #b_tactics{},        %% Требуемые тактики
                mana           = 0              :: non_neg_integer(),   %% Требуемая мана
                level          = 0              :: non_neg_integer(),   %% Требуемый уровень персонажа
                stats          = #u_stats{}     :: #u_stats{},          %% Требуемые статы
                skills         = #u_skills{}    :: #u_skills{},         %% Требуемые умелки
                unshockable    = false          :: boolean(),           %% Не подвержен шоку
                initial_delay  = 0              :: non_neg_integer(),   %% Начальная задержка
                delay          = 1              :: non_neg_integer(),   %% Задержка
                class_delay    = false          :: boolean(),           %% Общая задержка
                uses                            :: non_neg_integer(),   %% Кол-во юзов
                expend         = false          :: boolean(),           %% Прием тратит ход
                require_target = false          :: boolean(),           %% Требует указать цель
                target_type    = any            :: trick_target_type(), %% Тип цели
                self_buff                       :: atom(),              %% Накладываемый на себя бафф
                enemy_buff                      :: atom(),              %% Накладываемый на противника бафф
                action                          :: fun()                %% Вызываемая ф-я
        }).

%% представление приема в менеджере
-record(b_trick, {trick   = #trick{}        :: #trick{},            %% описание приема
                  unit                      :: pid(),               %% владелец
                  active  = false           :: boolean(),           %% доступен в данный момент
                  locked  = false           :: boolean(),           %% блокировка приема
                  delay   = 0               :: non_neg_integer(),   %% текущая задержка
                  mana    = 0               :: non_neg_integer(),   %% требуемая мана
                  tactics = #b_tactics{}    :: #b_tactics{}         %% требуемые тактики
       }).
