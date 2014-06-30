%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

%%% ====================================================================
%%% Вспомогательные ф-и работы со списками
%%% ====================================================================


-module(list_helper).

%% ====================================================================
%% API functions
%% ====================================================================
-export([keymap/4]).

keymap(K, N, L, F) when is_integer(N), N > 0, is_function(F, 1) ->
    keymap3(K, N, L, F).

keymap3(Key, Pos, [Tup|Tail], F) when element(Pos, Tup) == Key ->
    [F(Tup)|Tail];
keymap3(Key, Pos, [H|T], New) ->
    [H|keymap3(Key, Pos, T, New)];
keymap3(_, _, [], _) -> [].


%% ====================================================================
%% Internal functions
%% ====================================================================


