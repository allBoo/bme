%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% менеджер баффов
%%% ====================================================================

-define(BUFF_EV(Unit), {?GSI("buff_ev_", Unit#b_unit.id), {buff_mgr, start_ev, [Unit]}, transient, 5000, worker, dynamic}).
-define(BUFF_MGR(Unit), {?GSI("buff_mgr_", Unit#b_unit.id), {buff_mgr, start_mgr, [Unit]}, transient, 5000, worker, [buff_mgr]}).
