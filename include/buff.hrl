%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% менеджер баффов
%%% ====================================================================

-define(puserpid(UnitPid), unit:get_user_pid(UnitPid)).
-define(puser(UnitPid), user_state:get(?puserpid(UnitPid))).
-define(plevel(UnitPid), user_state:get(unit:get_user_pid(UnitPid), 'info.level')).



-define(BUFF_EV(Unit), {?GSI("buff_ev_", Unit#b_unit.id), {buff_mgr, start_ev, [Unit]}, transient, 5000, worker, dynamic}).
-define(BUFF_MGR(Unit), {?GSI("buff_mgr_", Unit#b_unit.id), {buff_mgr, start_mgr, [Unit]}, transient, 5000, worker, [buff_mgr]}).

-record(buff, {id :: term(),                      %% идентификатор
               type :: pot | bonus,               %% тип баффа
               name = <<""/utf8>> :: bitstring(), %% текстовое название
               level :: non_neg_integer(),        %% уровень баффа
               value :: any(),                    %% величина баффа
               uniq = false :: boolean(),         %% уникальное владение
               unit  :: pid(),                    %% юнит, на которого наложен бафф
               owner :: pid(),                    %% юнит, который наложил бафф
               time = infinity :: non_neg_integer() | infinity,  %% время действия
               charges :: non_neg_integer(),      %% кол-во зарядов
               state :: any()
       }).

