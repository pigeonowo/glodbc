pub type Connection

pub type Value

pub type ODBCSelectPosition {
  Next
  Relative(n: Int)
  Absolute(n: Int)
}

pub type ODBCType {
  SqlChar(size: Int)
  SqlWchar(size: Int)
  SqlNumeric(precision: Int, scale: Int)
  SqlDecimal(precision: Int, scale: Int)
  SqlInteger
  SqlSmallint
  SqlFloat(precision: Int)
  SqlReal
  SqlDouble
  SqlVarchar(size: Int)
  SqlWvarchar(size: Int)
  SqlTypeDate
  SqlTypeTime
  SqlTypeTimestamp
  SqlLongvarchar(size: Int)
  SqlWlongvarchar(size: Int)
  SqlBinary
  SqlVarbinary
  SqlLongvarbinary
  SqlTinyint
  SqlBit
}

pub type ODBCDescription {
  Description(name: String, datatype: ODBCType)
}

pub type ODBCError {
  ConnectionError
  DisconnectionError
  QueryError
  CommitError
  DescribeTableError
  SelectError
  SelectCountError
}

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

@external(erlang, "odbc", "commit")
pub fn odbc_commit(connection: conn, commit_mode: mode) -> odbcresult

@external(erlang, "odbc", "first")
pub fn odbc_first(connection: Connection) -> queryresult

@external(erlang, "odbc", "last")
pub fn odbc_last(connection: Connection) -> queryresult

@external(erlang, "odbc", "next")
pub fn odbc_next(connection: Connection) -> queryresult

@external(erlang, "odbc", "prev")
pub fn odbc_prev(connection: Connection) -> queryresult

@external(erlang, "odbc", "select")
pub fn odbc_select(
  conn: Connection,
  pos: ODBCSelectPosition,
  n: Int,
) -> selectresult

@external(erlang, "odbc", "select_count")
pub fn odbc_select_count(
  conn: Connection,
  query: query,
) -> Result(Int, ODBCError)

@external(erlang, "odbc", "describe_table")
pub fn odbc_describe_table(
  connection: conn,
  table: table,
) -> Result(List(#(name, datatype)), ODBCError)

@external(erlang, "glodbc_ffi_erl", "convert_odbcdescription")
pub fn convert_odbcdescription(description: desc) -> ODBCDescription

@external(erlang, "glodbc_ffi_erl", "coerce")
pub fn coerce(x: t) -> Value
