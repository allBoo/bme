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
		throw:{gproc_error, BmeError} ->
			erlang:error(BmeError, Args)
	end).

-define(THROW_BME_ERROR(E), throw(E)).

%%% ====================================================================
%%% Коды ошибок
%%% ====================================================================
-define(ERROR_UNCOMPLETED, #error{code = -1, message = <<"Не реализовано">>}).
-define(ERROR_WRONG_CALL, #error{code = -1, message = <<"Wrong call">>}).
-define(ERROR_LOW_TEAM, #error{code = 1001, message = <<"Бой не может начаться по причине - группа не набрана">>}).
