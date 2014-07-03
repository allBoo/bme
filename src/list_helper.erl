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
-export([keymap/4,
		 first_nequal/2,
		 find_first/2,
		 rsublist/3]).

%% keymap/4
%% ====================================================================
%% Возвращет новый список, в котором к элементу, найденному по ключу
%% применяется переданная функция
keymap(K, N, L, F) when is_integer(N), N > 0, is_function(F, 1) ->
    keymap3(K, N, L, F).

keymap3(Key, Pos, [Tup|Tail], F) when element(Pos, Tup) == Key ->
    [F(Tup)|Tail];
keymap3(Key, Pos, [H|T], New) ->
    [H|keymap3(Key, Pos, T, New)];
keymap3(_, _, [], _) -> [].


%% first_nequal/2
%% ====================================================================
%% Возвращет первый элемент, не равный переданному значению
first_nequal(List, X) when is_list(List) ->
	first_nequal0(List, X).

first_nequal0([El|_Tail], X) when El =/= X ->
	El;
first_nequal0([_H|T], X) ->
	first_nequal0(T, X);
first_nequal0(_, _) -> false.


%% find_first/2
%% ====================================================================
%% Возвращет первый элемент, для которого ф-я возвращает true
find_first(List, F) when is_list(List),
						 is_function(F, 1) ->
	find_first0(List, F).

find_first0([El|Tail], F) ->
	case F(El) of
		true  -> El;
		false -> find_first0(Tail, F)
	end;
find_first0([], _) -> false.

%% rsublist/2
%% ====================================================================
%% Возвращет часть списка начиная с позиции на которой находится
%% Elem и длиной Length
%% если хвост под-списка достигнет хвоста основного списка
%% то будут добавлены элементы с головы списка
rsublist(List, Elem, Length) ->
	{Head, Tail} = lists:splitwith(fun(El) ->
										   El /= Elem
								   end, List),
	lists:sublist(Tail ++ Head, Length).

%% ====================================================================
%% Internal functions
%% ====================================================================


