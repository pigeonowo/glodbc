import gleam/dict
import gleam/erlang/charlist
import gleam/io
import gleam/list
import glodbc_ffi.{
  odbc_connect, odbc_disconnect, odbc_param_query, odbc_sql_query, odbc_start,
}

pub type Connection

pub type Row

// Types
pub type OptionSwitch {
  On
  Off
}

pub type ODBCOption {
  // Timeout, dynamic makes it hard to give this along with bool values as options to erlang
  AutoCommit
  BinaryStrings
  TupleRow
  ScrollableCursors
  TraceDriver
  ExtendedErrors
}

/// For erlang functions when they return `ok`
pub type ODBCSuccess {
  ODBCOk
}

pub type ODBCError {
  ConnectionError
  DisconnectionError
  QueryError
}

pub type QueryResult {
  Updated(rows: Int)
  Selected(col_names: List(String), rows: List(Row))
}

// Int or Float
pub type Integer

pub type SQLParam(number) {
  SqlChar(values: List(String))
  SqlWchar(values: List(Int))
  SqlNumeric(values: List(number))
  SqlDecimal(values: List(number))
  SqlInteger(values: List(Int))
  SqlSmallint(values: List(Int))
  SqlFloat(values: List(Float))
  SqlReal(values: List(Float))
  SqlDouble(values: List(Float))
  SqlVarchar(values: List(String))
  SqlWvarchar(values: List(String))
  SqlTypeDate(values: List(String))
  SqlTypeTime(values: List(String))
  SqlTypeTimestamp(values: List(#(#(Int, Int, Int), #(Int, Int, Int))))
  SqlLongvarchar(values: List(String))
  SqlWlongvarchar(values: List(String))
  SqlBinary(values: List(String))
  SqlVarbinary(values: List(String))
  SqlLongvarbinary(values: List(String))
  SqlTinyint(values: List(Int))
  SqlBit(values: List(Bool))
}

pub type Value

// functions

fn bool_to_onoff(b: Bool) -> OptionSwitch {
  case b {
    True -> On
    False -> Off
  }
}

pub fn default_options() {
  [
    #(AutoCommit, Off),
    #(BinaryStrings, On),
    #(TupleRow, On),
    #(ScrollableCursors, On),
    #(TraceDriver, Off),
    #(ExtendedErrors, Off),
  ]
  |> dict.from_list
}

/// Connect to a database with a connection string and options
/// # Example:
/// ```rs
/// let connstring = "Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;"
/// let assert Ok(conn) = connect(connstring, [#(AutoCommit, True)])
/// ```
pub fn connect(
  connection_string: String,
  options: List(#(ODBCOption, Bool)),
) -> Result(Connection, ODBCError) {
  let _start = odbc_start()
  let options =
    options
    |> list.map(fn(x) { #(x.0, bool_to_onoff(x.1)) })
    |> dict.from_list
  let erl_opts =
    default_options()
    |> dict.combine(options, fn(_default, given) { given })
    |> dict.to_list
  case odbc_connect(charlist.from_string(connection_string), erl_opts) {
    Ok(conn) -> Ok(conn)
    Error(e) -> {
      io.debug(charlist.to_string(e))
      Error(ConnectionError)
    }
  }
}

pub fn disconnect(connection: Connection) -> Result(ODBCSuccess, ODBCError) {
  case odbc_disconnect(connection) {
    Error(_) -> Error(DisconnectionError)
    _ -> Ok(ODBCOk)
  }
}

pub fn sql_query(
  connection: Connection,
  sql_query: String,
) -> Result(QueryResult, ODBCError) {
  case odbc_sql_query(connection, charlist.from_string(sql_query)) {
    Error(_) -> Error(QueryError)
    result -> result
  }
}

pub fn param_query(
  connection: Connection,
  sql_query: String,
  params: List(SQLParam(number)),
) -> Result(QueryResult, ODBCError) {
  case odbc_param_query(connection, sql_query, params) {
    Error(_) -> Error(QueryError)
    result -> result
  }
}

// test

pub fn main() {
  let connstring =
    "Driver={MariaDB ODBC 3.0 Driver};DSN=localhost;UID=testuser;PWD=password"
  let assert Ok(conn) = connect(connstring, [#(AutoCommit, True)])

  let assert Ok(Selected(_col_names, _rows)) =
    sql_query(conn, "Select * from test.testtable;")

  let assert Ok(Updated(_rowcount)) =
    sql_query(conn, "Update test.testtable set name='Fred' where id=1")

  let _res =
    sql_query(
      conn,
      "Insert into test.testtable (id, name, age) values (2, 'Jonas', 18)",
    )

  let res =
    param_query(conn, "Select age from test.testtable where id=? and name=?", [
      SqlInteger([1]),
      SqlVarchar(["Jonas"]),
    ])
  let _ = io.debug(res)

  let assert Ok(ODBCOk) = disconnect(conn)
}
