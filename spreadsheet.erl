% modulo per la creazione e modifica dei fogli
-module(spreadsheet).


% per fare query
-include_lib("stdlib/include/qlc.hrl").


-export([
    new/1,
    new/4,
    share/2,
    get/4,
    get/5,
    set/5,
    set/6,
    info/1
]).

% definizione record
% foglio
-record(spreadsheet, {table, row, colonne}).
% proprietario del foglio
-record(owner, {foglio, pid}).
% policy di proprieta
-record(policy, {pid, foglio, politica}).
% formato dei fogli
-record(format, {foglio, tab_index, num_row, num_col}).

% creo lo schema 
new(TabName, N, M, K) ->
    mnesia:start(),
    TabelleLocali = mnesia:system_info(tables),
    % controllo che TabName non sia gia' presente
    case lists:member(TabName, TabelleLocali) of
        true -> {error, invalid_name};
        false ->
            % creo lo schema delle due tabelle del DB coi campi che prendo dai records
            SpreadsheetFields = record_info(fields, spreadsheet),
            % NB il nome della tabella e' == al nome dei records che essa ospita
            % specifico i parametri opzionali per avere una copia del DB
            % su disco e in RAM anche nei nodi distribuiti
            NodeList = [node()]++nodes(),
            mnesia:create_table(TabName, [
                {attributes, SpreadsheetFields},
                {disc_copies, NodeList},
                {type, bag}
            ]),
            % popolo il foglio con k tabelle di n righe e m colonne
            popola_foglio(TabName, K, N, M),
            % creo una nuova tabella in cui dico che il nodo è proprietario del foglio (tabella)
            popola_owner_table(TabName),
            % salvo le informazioni per il formato della tabella
            popola_format_table(TabName, K, N, M)
    end
.

new(TabName) -> spreadsheet:new(TabName, 10, 10, 10).

popola_owner_table(Foglio)->
    F = fun()->
        Data = #owner{foglio=Foglio, pid=self()},
        mnesia:write(Data)
    end,
    {atomic, ok} = mnesia:transaction(F),
    % aggiorno le policy
    F1 = fun() ->
            Data = #policy{pid=self(), foglio=Foglio, politica=write},
            mnesia:write(Data)
        end,
    {atomic, ok} = mnesia:transaction(F1)
.

crea_riga(TabName, I, J, M) -> {TabName, I, J, crea_colonne(M)}.

crea_colonne(M)-> lists:duplicate(M, undef).

popola_foglio(Name, K, N, M) when K > 1, N > 1, M > 1 ->
    Fila = fun(I) -> 
        lists:map(
            fun(J) -> 
                crea_riga(Name, I, J, M) 
            end, 
            lists:seq(1, N)
        )
    end,
    Matrix = lists:flatmap(Fila, lists:seq(1, K)),
    save_db(Name, Matrix)
.

save_db(Foglio, Matrix) ->
    F = fun() ->
        lists:foreach(
            fun(Elem)-> 
                mnesia:write(Foglio, Elem, write)
            end,
            Matrix
        ) 
    end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted,Reason} -> {error,Reason};
        {atomic,Res} -> Res
    end
.
% num di celle e' N per M

popola_format_table(TabName, K, N, M) ->
    Fun = fun() -> lists:foreach(
            fun(I) -> mnesia:write(#format{
                foglio=TabName, 
                tab_index=I, 
                num_row=N, 
                num_col=M}
            ) end,
            lists:seq(1, K)
        )
    end,
    Result = mnesia:transaction(Fun),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, ok} -> ok
    end  
.

% controllo policy in lettura
% il foglio puo' essere letto solo se condiviso
get(SpreadSheet, TableIndex, I, J) ->
    MioPid = self(),
    PolicyQuery = qlc:q(
        % list comprehension
        [ X#policy.politica ||
            % seleziona tutte righe tabella policy
            X <- mnesia:table(policy),
            X#policy.pid == MioPid,
            X#policy.foglio == SpreadSheet
        ]
    ),
    Fun = fun() -> qlc:e(PolicyQuery) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, Res} ->
            case Res of
                [] -> {error, not_allowed};
                [Policy] ->
                    % solo se il file e' condiviso Policy e' popolata
                    Condition = (Policy == read) or (Policy == write), 
                    case Condition of
                        false -> {error, not_allowed};
                        true -> get_value(SpreadSheet, TableIndex, I, J)
                    end;
                Msg -> {error, {unknown, Msg}}
            end
    end
.

% get senza controlli
get_value(SpreadSheet, TableIndex, I, J) ->
    TakeRowQuery = qlc:q(
        % list comprehension
        [ X#spreadsheet.colonne ||
            % seleziona tutte righe tabella SpreadSheet
            X <- mnesia:table(SpreadSheet),
            X#spreadsheet.table == TableIndex,
            X#spreadsheet.row == I
        ]
    ),
    Fun1 = fun() -> qlc:e(TakeRowQuery) end,
    Result1 = mnesia:transaction(Fun1),
    case Result1 of
        {aborted, Reason1} -> {error, Reason1};
        {atomic, []} -> {error, not_found};
        {atomic, [RowI]} -> lists:nth(J, RowI);
        Msg -> {error, {unknown, Msg}}
    end
.

get(SpreadSheet, TableIndex, I, J, Timeout) ->
    MioPid = self(),
    PolicyQuery = qlc:q(
        % list comprehension
        [ X#policy.politica ||
            % seleziona tutte righe tabella policy
            X <- mnesia:table(policy),
            X#policy.pid == MioPid,
            X#policy.foglio == SpreadSheet
        ]
    ),
    Fun = fun() -> qlc:e(PolicyQuery) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, Res} ->
            case Res of
                [] -> {error, not_allowed};
                [Policy] ->
                    % solo se il file e' condiviso Policy e' popolata
                    Condition = (Policy == read) or (Policy == write), 
                    case Condition of
                        false -> {error, not_allowed};
                        % GET TIMEOUT
                        true -> get_timeout(SpreadSheet, TableIndex, I, J, Timeout)
                    end;
                Msg -> {error, {unknown, Msg}}
            end
    end
.


get_timeout(SpreadSheet, TableIndex, I, J, Timeout) ->
    myflush(),
    MioPid = self(),
    % creo un processo timer
    spawn(fun() ->
        receive after Timeout -> MioPid!{timeout} end
    end),
    % creo un processo getter
    spawn(fun() ->
        MioPid!{result, get_value(SpreadSheet, TableIndex, I, J)}
    end),
    ToReturn = receive
        {result, Res} -> Res;
        {timeout} -> timeout
    after 10000 -> {error, no_message_received}
    end,
    myflush(),
    ToReturn
.

% con timeout
% implemento mia flush
myflush() ->
    receive
        % consuma il pattern e va in loop
        _AnyPattern -> myflush()
    after
        % se non ha messaggi nella coda 
        % restituisce subito (0 sec) ok
        0 -> ok
    end
.

set(SpreadSheet, TableIndex, I, J, Value) ->
    MioPid = self(),
    PolicyQuery = qlc:q(
        % list comprehension
        [ X#policy.politica ||
            % seleziona tutte righe tabella policy
            X <- mnesia:table(policy),
            X#policy.pid == MioPid,
            X#policy.foglio == SpreadSheet
        ]
    ),
    Fun = fun() -> qlc:e(PolicyQuery) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, Res} ->
            case Res of
                [] -> {error, not_allowed};
                [Policy] -> 
                    case Policy == write of
                        false -> {error, not_allowed};
                        true -> set_value(SpreadSheet, TableIndex, I, J, Value)
                    end;
                Msg1 -> {error, {unknown, Msg1}} 
            end
    end
.

% set senza controlli
set_value(SpreadSheet, TableIndex, I, J, Value) ->
    TakeColumnQuery = qlc:q(
        % list comprehension
        [ X#spreadsheet.colonne ||
            % seleziona tutte righe tabella SpreadSheet
            X <- mnesia:table(SpreadSheet),
            X#spreadsheet.table == TableIndex,
            X#spreadsheet.row == I
        ]
    ),
    Fun1 = fun() -> qlc:e(TakeColumnQuery) end,
    Result1 = mnesia:transaction(Fun1),
    case Result1 of
        {aborted, Reason1} -> {error, Reason1};
        {atomic, Res1} ->
            case Res1 of
                [] -> {error, not_found};
                [RowI] -> 
                    {L1, L2} = lists:split(J, RowI),
                    L1WithoutLast = lists:droplast(L1),
                    FinalRow = L1WithoutLast ++ [Value] ++ L2,
                    F2 = fun() ->
                        Record = {SpreadSheet,
                            TableIndex, 
                            I,
                            % prendo il record di prima e lo cancello
                            RowI},  
                        mnesia:delete_object(Record),
                        NewRecord = {SpreadSheet, TableIndex, I, FinalRow},
                        mnesia:write(NewRecord)
                    end,
                    {atomic, ok} = mnesia:transaction(F2), ok;
                Msg -> {error, {unknown, Msg}}
            end
    end
.

set(SpreadSheet, TableIndex, I, J, Value, Timeout) ->
    MioPid = self(),
    PolicyQuery = qlc:q(
        % list comprehension
        [ X#policy.politica ||
            % seleziona tutte righe tabella policy
            X <- mnesia:table(policy),
            X#policy.pid == MioPid,
            X#policy.foglio == SpreadSheet
        ]
    ),
    Fun = fun() -> qlc:e(PolicyQuery) end,
    Result = mnesia:transaction(Fun),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, Res} ->
            case Res of
                [] -> {error, not_allowed};
                [Policy] -> 
                    case Policy == write of
                        false -> {error, not_allowed};
                        % set tmout
                        true -> set_timeout(SpreadSheet, TableIndex, I, J, Value, Timeout)
                    end;
                Msg1 -> {error, {unknown, Msg1}} 
            end
    end
.
% non esportata
set_timeout(SpreadSheet, TableIndex, I, J, Value, Timeout) ->
    myflush(),
    % a questo punto ho i permessi in scrittura (e lettura) gia' verificati
    ValueToRestore = get_value(SpreadSheet, TableIndex, I, J),
    ToReturn = case ValueToRestore of
        {error, Reason1} -> {error, Reason1};
        _ -> 
            MioPid = self(),
            % creo un processo timer
            spawn(fun() ->
                receive after Timeout -> MioPid!{timeout} end
            end),
            % creo un processo setter
            spawn(fun() ->
                MioPid!{result, set_value(SpreadSheet, TableIndex, I, J, Value)}
            end),
            receive
                {result, Res} -> Res;
                {timeout} ->
                    % aspetto che finisce altrimenti entra in race condition
                    receive
                        {result, _} -> ok
                    end,
                    % rimetto le cose come prima
                    Result = set_value(SpreadSheet, TableIndex, I, J, ValueToRestore),
                    case Result of
                        {error, Reason} -> {error, Reason};
                        ok -> timeout
                    end
            after 10000 -> {error, no_message_received}
            end
    end,
    myflush(),
    ToReturn
.
share(Foglio, AccessPolicies)->
    % AccessPolicies = {Proc, AP}
    % Proc = Pid
    % AP = read | write
    % controllo che share la chiami solo il proprietario della tabella
    {Proc, Ap} = AccessPolicies,
    Condition = (Ap == read) or (Ap == write),
    case Condition of
        false -> {error, wrong_policy_format};
        true -> 
            F = fun() -> mnesia:read({owner, Foglio}) end,
            Result = mnesia:transaction(F),
            case Result of
                {aborted, Reason} -> {error, Reason};
                {atomic, Res} ->
                    case Res of
                        % il foglio non esiste
                        [] ->  {error, sheet_not_found};
                        % il foglio esiste
                        [{owner, Foglio, Value}] -> 
                            % controllo che chi voglia condividere sia il proprietario del foglio
                            case Value == self() of
                                % non sono il proprietario
                                false -> {error, not_the_owner};
                                % sono il proprietario
                                true -> 
                                    Query = qlc:q([X || 
                                        X <- mnesia:table(policy),
                                        X#policy.pid =:= Proc,
                                        X#policy.foglio =:= Foglio 
                                    ]),
                                    F2 = fun() -> qlc:e(Query) end,
                                    % cerco il pid e il foglio nella tabella
                                    Result1 = mnesia:transaction(F2),
                                    case Result1 of
                                        {aborted, Reason1} -> {error, Reason1};
                                        {atomic, Res1} ->
                                            case Res1 of
                                                % tabella "vuota" quindi posso scrivere
                                                [] -> 
                                                    F3 = fun()->
                                                            Data = #policy{pid=Proc, foglio=Foglio, politica=Ap},
                                                            mnesia:write(Data)
                                                        end,
                                                    Result2 = mnesia:transaction(F3),
                                                    case Result2 of
                                                        {aborted, Reason2} -> {error, Reason2};
                                                        {atomic, _} -> ok
                                                    end;
                                                [{policy, PidTrovato, FoglioTrovato, PolicyTrovata}] ->
                                                    % elemento gia' presente quindi devo prima eliminarlo
                                                    F4 = fun() ->
                                                            mnesia:delete_object({policy, PidTrovato, FoglioTrovato, PolicyTrovata})
                                                        end,
                                                    Result4 = mnesia:transaction(F4),
                                                    case Result4 of
                                                        {aborted, Reason4} -> {error, Reason4};
                                                        {atomic, _} -> 
                                                            % scrivere nella tabella le policy
                                                            F3 = fun()->
                                                                    Data = #policy{pid=Proc, foglio=Foglio, politica=Ap},
                                                                    mnesia:write(Data)
                                                                end,
                                                            Result3 = mnesia:transaction(F3),
                                                            case Result3 of
                                                                {aborted, Reason3} -> {error, Reason3};
                                                                {atomic, _} -> ok
                                                            end
                                                    end;
                                                Msg -> {error, {unknown, Msg}}
                                            end 
                                    end
                            end;
                        Msg2 -> {error, {unknown, Msg2}} 
                    end             
            end
    end
.

info(Foglio) ->
    % controllo se gia' presente
    mnesia:start(),
    TabelleLocali = mnesia:system_info(tables),
    case lists:member(Foglio, TabelleLocali) of
        false -> {error, not_exist};
        true ->
            % trovo i PID con permessi di scrittura
            QueryScrittura = qlc:q([X#policy.pid || 
                X <- mnesia:table(policy),
                X#policy.foglio == Foglio,
                X#policy.politica == write]),
            FScrittura = fun() -> qlc:e(QueryScrittura) end,
            ResultScrittura = mnesia:transaction(FScrittura),
            case ResultScrittura of
                {aborted, ReasonS} -> {error, ReasonS};
                {atomic, ListaPidScrittura} ->
                    % trovo i PID con permessi di lettura
                    QueryLettura = qlc:q([X#policy.pid || 
                        X <- mnesia:table(policy),
                        X#policy.foglio == Foglio,
                        X#policy.politica == read]),
                    FLettura = fun() -> qlc:e(QueryLettura) end,
                    ResultLettura = mnesia:transaction(FLettura),
                    case ResultLettura of
                        {aborted, ReasonL} -> {error, ReasonL};
                        {atomic, ListaPidLettura} ->
                            ListaPermessi = {policy_list, 
                                {read, ListaPidLettura}, 
                                {write, ListaPidScrittura}
                            },
                            % calcolo il numero di celle per tabella
                            ResultCelle = celle_per_tab(Foglio),
                            case ResultCelle of
                                {error, ReasonCelle} -> {error, ReasonCelle};
                                {result, ResCelle} ->
                                    CellePerTabella = {celle_per_tab, ResCelle},
                                    Info = [ListaPermessi, CellePerTabella],
                                    Info
                            end 
                    end
            end
    end
.

% CellePerTab = [{TabIndex, NCelle}]
% per ogni Tabella il numero di celle e' N*M
celle_per_tab(Foglio) ->
    Query = qlc:q([
        {X#format.tab_index, 
            (X#format.num_row * X#format.num_col)} || 
                X <- mnesia:table(format),
                X#format.foglio == Foglio]),
    F = fun() -> qlc:e(Query) end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} -> {error, Reason};
        % numero di celle per tabella !
        {atomic, CellePerTab} -> {result, CellePerTab}
    end
.