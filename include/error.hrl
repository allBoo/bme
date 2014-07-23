%%% -*- coding: utf-8 -*-
%%% Copyright (C) 2014 Alex Kazinskiy
%%%
%%% This file is part of Combats Battle Manager
%%%
%%% Author contact: alboo@list.ru

-record(error, {code, message}).

%%% ====================================================================
%%% Обработка ошибок
%%% ====================================================================

-define(CATCH_BME_ERROR(Expr, Args),
	try Expr
	catch
		throw:{error, BmeError} ->
			erlang:error(BmeError, Args)
	end).

-define(THROW_BME_ERROR(E), throw(E)).

%%% ====================================================================
%%% Коды ошибок
%%% ====================================================================
-define(ERROR_UNCOMPLETED, #error{code = -1, message = <<"Не реализовано"/utf8>>}).
-define(ERROR_WRONG_CALL, #error{code = 10, message = <<"Wrong call"/utf8>>}).
-define(ERROR_LOW_TEAM, #error{code = 1001, message = <<"Бой не может начаться по причине - группа не набрана"/utf8>>}).

-define(ERROR_UNDEFINED, #error{code = 100, message = <<"Что-то пошло не так"/utf8>>}).
-define(ERROR_NOT_APPLICABLE, #error{code = 101, message = <<"Не применимо в данный момент"/utf8>>}).
-define(ERROR_NOT_IN_BATTLE, #error{code = 102, message = <<"Персонаж не в бою"/utf8>>}).
-define(ERROR_TOO_FAST, #error{code = 103, message = <<"Не так быстро"/utf8>>}).
-define(ERROR_WAIT_OPPONENT, #error{code = 104, message = <<"Ожидаем хода противника"/utf8>>}).
-define(ERROR_UNIT_DEAD, #error{code = 105, message = <<"Вы погибли, ожидаем окончания поединка."/utf8>>}).
-define(ERROR_EMPTY_SPIRIT, #error{code = 106, message = <<"Нет силы духа"/utf8>>}).
-define(ERROR_BUFF_EXISTS, #error{code = 107, message = <<"Прием уже активен"/utf8>>}).

-define(ERROR_TRICK_NOT_EXISTS, #error{code = 200, message = <<"Прием не существует"/utf8>>}).
-define(ERROR_TRICK_NOT_APPLICABLE, #error{code = 201, message = <<"Не применимо в данный момент"/utf8>>}).
-define(ERROR_TRICK_ENEMY_ONLY, #error{code = 202, message = <<"Нельзя использовать на дружественную цель"/utf8>>}).
-define(ERROR_TRICK_FRIEND_ONLY, #error{code = 203, message = <<"Нельзя использовать на вражескую цель"/utf8>>}).
