-module(distribution).

-export([
    create_table/0,
    create_table_distr/0,
    start/0,
    start_distrib/0,
    stop/0,
    stop_distrib/0
]).

% proprietario del foglio
-record(owner, {foglio, pid}).
% le policy
-record(policy, {pid, foglio, politica}).
% formato dei fogli 
-record(format, {foglio, tab_index, nrighe, ncolonne}).

% LOCALE
% da chiamare solo una volta per creare il DB Mnesia in locale
create_table() ->
    % creo il DB solo in locale -> node()
    NodeList = [node()],
    mnesia:create_schema(NodeList),
    % faccio partire Mnesia
    mnesia:start(),
    % creo lo schema delle due tabelle del DB coi campi che prendo dai records
    OwnerFields = record_info(fields, owner),
    PolicyFields = record_info(fields, policy),
    FormatFields = record_info(fields, format),
    mnesia:create_table(owner, [
        {attributes, OwnerFields},
        {disc_copies, NodeList}
    ]),
    mnesia:create_table(policy, [
        {attributes, PolicyFields},
        {disc_copies, NodeList},
        {type, bag}
    ]),
    mnesia:create_table(format, [
        {attributes, FormatFields},
        {disc_copies, NodeList},
        {type, bag}
    ])
.

% DISTRIBUITA
% da chiamare solo una volta per creare il DB Mnesia distr
create_table_distr() ->
    % creo il DB in locale -> node() e in remoto -> nodes()
    NodeList = [node()] ++ nodes(),
    mnesia:create_schema(NodeList),
    % faccio partire Mnesia nei nodi remoti e da me
    start_remote(),
    % creo lo schema delle due tabelle del DB coi campi che prendo dai records
    OwnerFields = record_info(fields, owner),
    PolicyFields = record_info(fields, policy),
    FormatFields = record_info(fields, format),
    % NB il nome della tabella e' == al nome dei records che essa ospita
    % specifico i parametri opzionali per avere una copia del DB
    % su disco e in RAM anche nei nodi distribuiti
    mnesia:create_table(owner, [
        {attributes, OwnerFields},
        {disc_copies, NodeList}
    ]),
    mnesia:create_table(policy, [
        {attributes, PolicyFields},
        {disc_copies, NodeList},
        {type, bag}
    ]),
    mnesia:create_table(format, [
        {attributes, FormatFields},
        {disc_copies, NodeList},
        {type, bag}
    ]),
    % stop per ogni nodo remoto e per me
    distribution:stop_distrib()
.

% LOCALE
% da utilizzare ogni volta su ogni nodo del sistema
start() ->
    % faccio partire il DBMS Mnesia
    mnesia:start(),
    % aspetto (5 sec) che vengano caricate le tabelle del DB (distribuito)
    mnesia:wait_for_tables([owner, policy, format], 5000)
.

% DISTRIBUITA
% fa partire Mnesia e carica le tabelle
start_distrib() ->
    % start per ogni nodo remoto e per me
    start_remote(),
    % aspetto (5 sec) che vengano caricate le tabelle del DB (distribuito)
    mnesia:wait_for_tables([owner, policy, format], 5000)
.

% DISTRIBUITA
% fa partire Mnesia ma non carica le tabelle
start_remote() ->
    % NB nel DISTRIBUITO devo far partire Mnesia da dentro
    % ciascun nodo remoto e poi lo faccio partire da me in locale
    MioPid = self(),
    lists:foreach(
        fun(Node) ->
            spawn(
                Node, 
                fun() ->
                    % NB devo farlo "dentro" ogni nodo remoto 
                    mnesia:start(),
                    MioPid!{mnesia_started}
                end
            ) 
        end,
        nodes()
    ),
    % mi metto in attesa che Mnesia sia partito in tutti i nodi
    % remoti e poi lo eseguo nel mio nodo locale (master)
    lists:foreach(
        fun(_Node) -> 
            receive 
                {mnesia_started} -> ok 
            end
        end,
        nodes()
    ),
    % faccio partire Mnesia in locale
    mnesia:start()
.

% LOCALE
stop() -> mnesia:stop().

% DISTRIBUITA
% stop per ogni nodo remoto e per me
stop_distrib() ->
    % NB nel DISTRIBUITO devo far terminare Mnesia da dentro
    % ciascun nodo remoto e poi lo faccio terminare da me in locale
    MioPid = self(),
    lists:foreach(
        fun(Node) ->
            spawn(
                Node, 
                fun() ->
                    % NB devo farlo "dentro" ogni nodo remoto 
                    mnesia:stop(),
                    MioPid!{mnesia_stopped} 
                end
            ) 
        end,
        nodes()
    ),
    % mi metto in attesa che Mnesia sia terminato in tutti i nodi
    % remoti e poi lo termino nel mio nodo locale (master)
    lists:foreach(
        fun(_Node) -> 
            receive 
                {mnesia_stopped} -> ok 
            end
        end,
        nodes()
    ),
    % faccio terminare Mnesia in locale
    mnesia:stop()
.