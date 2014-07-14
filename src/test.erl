%% @author alboo
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test_haots/0, test_haots/1, test_b/0]).


test_haots() ->
	bme:start_haot([1, 2, 3, 4], 1, [{timeout, 1}]).

test_haots(Count) when Count > 4 ->
	bme:start_haot(lists:seq(1, Count), 1, [{timeout, 1}]).


test_b() ->
	bme:start_battle([1], [2], 1, [{timeout, 1}]).

%% ====================================================================
%% Internal functions
%% ====================================================================


