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
-define(ERROR_UNCOMPLETED, #error{code = -1, message = <<"Не реализовано">>}).
-define(ERROR_WRONG_CALL, #error{code = 10, message = <<"Wrong call">>}).
-define(ERROR_LOW_TEAM, #error{code = 1001, message = <<"Бой не может начаться по причине - группа не набрана">>}).

-define(ERROR_UNDEFINED, #error{code = 100, message = <<"Что-то пошло не так">>}).
-define(ERROR_NOT_APPLICABLE, #error{code = 101, message = <<"Не применимо в данный момент">>}).
-define(ERROR_NOT_IN_BATTLE, #error{code = 102, message = <<"Персонаж не в бою">>}).
-define(ERROR_TOO_FAST, #error{code = 103, message = <<"Не так быстро">>}).
-define(ERROR_WAIT_OPPONENT, #error{code = 104, message = <<"Ожидаем хода противника">>}).
-define(ERROR_UNIT_DEAD, #error{code = 105, message = <<"Вы погибли, ожидаем окончания поединка.">>}).
