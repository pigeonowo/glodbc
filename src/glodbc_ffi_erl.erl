-module(glodbc_ffi_erl).
-export([sql_query/2, param_query/3]).

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
    io:format("Params: ~w", Params),
    Converted_Params = [[{Type, [bin_to_list_if_bin(X) || X <- Values]} || {Type, Values} <- Params]],
    io:format("Converted Params: ~w", Converted_Params),
    case odbc:param_query(Connection, SQL_Query, Converted_Params) of
        {error, E} -> {error, E};
        {updated, Rows} -> {ok, {updated, Rows}};
        {selected, Col_Names, Rows} -> 
            Binary_Names = [list_to_binary(X) || X <- Col_Names],
            {ok, {selected, Binary_Names, Rows}}
    end.
    
