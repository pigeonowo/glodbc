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

pub type Value

pub type Param {
  Char(values: List(Value), size: Int)
  Wchar(values: List(Value), size: Int)
  Numeric(values: List(Value), precision: Int, scale: Int)
  Decimal(values: List(Value), precision: Int, scale: Int)
  Integer(values: List(Value))
  Smallint(values: List(Value))
  Float(values: List(Value), precision: Int)
  Real(values: List(Value))
  Double(values: List(Value))
  Varchar(values: List(Value), size: Int)
  Wvarchar(values: List(Value), size: Int)
  Date(values: List(Value))
  Time(values: List(Value))
  Timestamp(values: List(Value))
  Longvarchar(values: List(Value), size: Int)
  Wlongvarchar(values: List(Value), size: Int)
  Binary(values: List(Value))
  Varbinary(values: List(Value))
  Longvarbinary(values: List(Value))
  Tinyint(values: List(Value))
  Bit(values: List(Value))
}

// functions
@external(erlang, "glodbc_ffi_erl", "null")
pub fn null() -> Value

@external(erlang, "glodbc_ffi_erl", "coerce")
pub fn bool(bool: Bool) -> Value

@external(erlang, "glodbc_ffi_erl", "coerce")
pub fn int(int: Int) -> Value

@external(erlang, "glodbc_ffi_erl", "coerce")
pub fn float(float: Float) -> Value

/// any string
@external(erlang, "glodbc_ffi_erl", "text")
pub fn text(text: String) -> Value

@external(erlang, "glodbc_ffi_erl", "coerce")
pub fn timesstap(timesstap: #(#(Int, Int, Int), #(Int, Int, Int))) -> Value

fn convert_to_erl(param: Param) -> #(ODBCType, List(Value)) {
  case param {
    Char(vals, size) -> #(SqlChar(size), vals)
    Wchar(vals, size) -> #(SqlWchar(size), vals)
    Varchar(vals, size) -> #(SqlVarchar(size), vals)
    Longvarchar(vals, size) -> #(SqlLongvarchar(size), vals)
    Wvarchar(vals, size) -> #(SqlWvarchar(size), vals)
    Wlongvarchar(vals, size) -> #(SqlWlongvarchar(size), vals)
    Numeric(vals, prec, scale) -> #(SqlNumeric(prec, scale), vals)
    Decimal(vals, prec, scale) -> #(SqlDecimal(prec, scale), vals)
    Float(vals, prec) -> #(SqlFloat(prec), vals)
    Binary(vals) -> #(SqlBinary, vals)
    Varbinary(vals) -> #(SqlVarbinary, vals)
    Longvarbinary(vals) -> #(SqlLongvarbinary, vals)
    Date(vals) -> #(SqlTypeDate, vals)
    Time(vals) -> #(SqlTypeTime, vals)
    Timestamp(vals) -> #(SqlTypeTimestamp, vals)
    Integer(vals) -> #(SqlInteger, vals)
    Smallint(vals) -> #(SqlSmallint, vals)
    Real(vals) -> #(SqlReal, vals)
    Double(vals) -> #(SqlDouble, vals)
    Tinyint(vals) -> #(SqlTinyint, vals)
    Bit(vals) -> #(SqlBit, vals)
  }
}

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
  params: List(Param),
) -> Result(QueryResult, ODBCError) {
  let params = list.map(params, convert_to_erl)
  case odbc_param_query(connection, charlist.from_string(sql_query), params) {
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
    param_query(conn, "Select age from test.testtable where id=?", [
      Integer([int(1)]),
      Varchar([text("Jonas")], 100),
    ])
  let _ = io.debug(res)

  let assert Ok(ODBCOk) = disconnect(conn)
}
