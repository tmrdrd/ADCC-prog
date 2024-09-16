-module(debug).

-export([
    delete_close_tables/0,
	log/1, 
	time_it/1, 
	test_spreadsheet_set/0, 
	run_trials/0
]).

delete_close_tables() ->
    mnesia:start(),
    TabelleLocali = mnesia:system_info(tables), 
    lists:foreach(fun(T) -> mnesia:delete_table(T) end, TabelleLocali),
    mnesia:delete_table(owner),
    mnesia:delete_table(policy),
    mnesia:delete_table(format),
    mnesia:stop()
.

log(Message) ->
    io:format("~p~n", [Message]).

time_it(Fun) ->
    Start = erlang:monotonic_time(),
    Fun(),
    End = erlang:monotonic_time(),
    Elapsed = End - Start,
    io:format("Time elapsed: ~p ms~n", [Elapsed / 1000]),
    Elapsed.

test_spreadsheet_set() ->
    % inizializzo la tabella del proprietario
    distribution:create_table(), % Oppure distribution:create_table_distr() per l'app distribuita
    spreadsheet:new("MySheet", 2, 10, 10), 
    spreadsheet:set("MySheet", 1, 5, 5, "Test Value", 5000). 

run_trials() ->
    Trials = 100,
    Times = lists:foldl(
        fun(_, Acc) ->
            Result = time_it(fun() -> test_spreadsheet_set() end),
            case Result of
                ok -> Acc;
                _  -> [Result | Acc]
            end
        end,
        [],
        lists:seq(1, Trials)
    ),
    AverageTime = lists:sum(Times) / length(Times),
    io:format("Average time: ~p ms~n", [AverageTime / 1000]).
