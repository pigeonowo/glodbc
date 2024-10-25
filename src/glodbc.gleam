import gleam/dict
import gleam/erlang/charlist
import gleam/io
import gleam/list

pub type Connection

pub type Row

// Types
pub type OptionSwitch {
  On
  Off
}

pub type ODBCCommitMode {
  Commit
  Rollback
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
  CommitError
  DescribeTableError
  SelectError
  SelectCountError
}

pub type ODBCSelectPosition {
  Next
  Relative(n: Int)
  Absolute(n: Int)
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

pub type ODBCDescription {
  Description(name: String, datatype: ODBCType)
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

// timeout?
@external(erlang, "glodbc_ffi_erl", "sql_query")
pub fn odbc_sql_query(connection: Connection, sql_query: query) -> queryresult

//timeout?
@external(erlang, "glodbc_ffi_erl", "param_query")
pub fn odbc_param_query(
  connection: Connection,
  sql_query: query,
  query_params: params,
) -> queryresult

@external(erlang, "odbc", "connect")
pub fn odbc_connect(
  connection_string: connstring,
  options: options,
) -> odbcresult

@external(erlang, "odbc", "disconnect")
pub fn odbc_disconnect(connection: Connection) -> odbcresult

@external(erlang, "odbc", "start")
pub fn odbc_start() -> odbcresult

@external(erlang, "odbc", "commit")
pub fn odbc_commit(connection: Connection, commit_mode: mode) -> odbcresult

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

pub fn commit(
  connection: Connection,
  commit_mode: ODBCCommitMode,
) -> Result(ODBCSuccess, ODBCError) {
  case odbc_commit(connection, commit_mode) {
    Error(_) -> Error(CommitError)
    _ -> Ok(ODBCOk)
  }
}

pub fn describe_table(
  conn: Connection,
  table: String,
) -> Result(List(ODBCDescription), ODBCError) {
  let charlist_table = charlist.from_string(table)
  case odbc_describe_table(conn, charlist_table) {
    Ok(description) -> {
      Ok(list.map(description, convert_odbcdescription))
    }
    Error(_) -> Error(DescribeTableError)
  }
}

pub fn select(conn: Connection, position: ODBCSelectPosition, n: Int) {
  case odbc_select(conn, position, n) {
    Error(err) -> {
      let _ = io.debug(err)
      Error(SelectError)
    }
    selected -> Ok(selected)
  }
}

pub fn select_count(conn: Connection, query: String) -> Result(Int, ODBCError) {
  case odbc_select_count(conn, query) {
    Error(_) -> Error(SelectCountError)
    rows -> rows
  }
}

// test

pub fn main() {
  // Testing with MariaDB, other ODBC Databases work too of course! Just have the right driver!
  let connstring =
    "Driver={MariaDB ODBC 3.0 Driver};DSN=localhost;UID=testuser;PWD=password"
  let assert Ok(conn) = connect(connstring, [#(AutoCommit, True)])

  let assert Ok(Selected(_col_names, _rows)) =
    sql_query(conn, "Select * from test.testtable;")

  let assert Ok(Updated(_rowcount)) =
    sql_query(conn, "Update test.testtable set name='Fred' where id=1")

  let assert Ok(Updated(_rowcount)) =
    sql_query(
      conn,
      "Insert into test.testtable (id, name, age) values (2, 'Jonas', 18)",
    )

  let assert Ok(Selected(_col_names, _rows)) =
    param_query(conn, "Select age from test.testtable where id=?", [
      Integer([int(1)]),
      Varchar([text("Jonas")], 100),
    ])

  let assert Ok(ODBCOk) = commit(conn, Commit)
  // or Rollback

  let assert Ok(_tabledescription) = describe_table(conn, "test.testtable")
  // -> [Description("id", SqlInteger), Description("name", SqlVarchar(100)), Description("age", SqlInteger)]

  let assert Ok(ODBCOk) = disconnect(conn)
}
