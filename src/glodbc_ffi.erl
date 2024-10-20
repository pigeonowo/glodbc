-module(glodbc_ffi).
-export([sql_query/2]).

sql_query(Connection, SQL_Query) ->
    case odbc:sql_query(Connection, SQL_Query) of
        {error, E} -> {error, E};
        {updated, Rows} -> {ok, {updated, Rows}};
        {selected, Col_Names, Rows} -> {ok, {selected, Col_Names, Rows}}
    end.
