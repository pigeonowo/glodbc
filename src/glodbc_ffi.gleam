@external(erlang, "odbc", "connect")
pub fn odbc_connect(
  connection_string: connstring,
  options: options,
) -> odbcresult

@external(erlang, "odbc", "disconnect")
pub fn odbc_disconnect(connection: conn) -> odbcresult

@external(erlang, "odbc", "start")
pub fn odbc_start() -> odbcresult

// timeout?
@external(erlang, "glodbc_ffi_erl", "sql_query")
pub fn odbc_sql_query(connection: conn, sql_query: query) -> queryresult

//timeout?
@external(erlang, "glodbc_ffi_erl", "param_query")
pub fn odbc_param_query(
  connection: conn,
  sql_query: query,
  query_params: params,
) -> queryresult
// TODO
// commit
// describe_table
// first
// last
// next
// prev
// select
// select_count
