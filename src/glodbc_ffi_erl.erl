-module(glodbc_ffi_erl).
-export([sql_query/2, param_query/3, coerce/1, null/0, text/1]).

null() -> null.

coerce(Value) -> Value.

text(Bin) when is_binary(Bin) ->
    binary_to_list(Bin). 

sql_query(Connection, SQL_Query) ->
    case odbc:sql_query(Connection, SQL_Query) of
        {error, E} -> {error, E};
        {updated, Rows} -> {ok, {updated, Rows}};
        {selected, Col_Names, Rows} -> 
            Binary_Names = [list_to_binary(X) || X <- Col_Names],
            {ok, {selected, Binary_Names, Rows}}
    end.

bin_to_list_if_bin(X) when is_binary(X) ->  
    binary_to_list(X);
bin_to_list_if_bin(X) -> X.

param_query(Connection, SQL_Query, Params) ->
    io:format("Params: ~p~n", [Params]),
    case odbc:param_query(Connection, SQL_Query, [Params]) of
        {error, E} -> {error, E};
        {updated, Rows} -> {ok, {updated, Rows}};
        {selected, Col_Names, Rows} -> 
            Binary_Names = [list_to_binary(X) || X <- Col_Names],
            {ok, {selected, Binary_Names, Rows}}
    end.
    
