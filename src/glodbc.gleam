import gleam/dict
import gleam/erlang/charlist
import gleam/io
import gleam/list
import glodbc_ffi.{
  type Connection, type ODBCDescription, type ODBCError, type ODBCSelectPosition,
  type ODBCType, type Value, coerce, convert_odbcdescription, odbc_commit,
  odbc_connect, odbc_describe_table, odbc_disconnect, odbc_param_query,
  odbc_select, odbc_select_count, odbc_sql_query, odbc_start,
}

pub type Row

pub type OptionSwitch {
  On
  Off
}

fn bool_to_onoff(b: Bool) -> OptionSwitch {
  case b {
    True -> On
    False -> Off
  }
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

pub type QueryResult {
  Updated(rows: Int)
  Selected(col_names: List(String), rows: List(Row))
}

pub type ODBCQueryParam {
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

fn convert_to_erl(param: ODBCQueryParam) -> #(ODBCType, List(Value)) {
  case param {
    Char(vals, size) -> #(glodbc_ffi.SqlChar(size), vals)
    Wchar(vals, size) -> #(glodbc_ffi.SqlWchar(size), vals)
    Varchar(vals, size) -> #(glodbc_ffi.SqlVarchar(size), vals)
    Longvarchar(vals, size) -> #(glodbc_ffi.SqlLongvarchar(size), vals)
    Wvarchar(vals, size) -> #(glodbc_ffi.SqlWvarchar(size), vals)
    Wlongvarchar(vals, size) -> #(glodbc_ffi.SqlWlongvarchar(size), vals)
    Numeric(vals, prec, scale) -> #(glodbc_ffi.SqlNumeric(prec, scale), vals)
    Decimal(vals, prec, scale) -> #(glodbc_ffi.SqlDecimal(prec, scale), vals)
    Float(vals, prec) -> #(glodbc_ffi.SqlFloat(prec), vals)
    Binary(vals) -> #(glodbc_ffi.SqlBinary, vals)
    Varbinary(vals) -> #(glodbc_ffi.SqlVarbinary, vals)
    Longvarbinary(vals) -> #(glodbc_ffi.SqlLongvarbinary, vals)
    Date(vals) -> #(glodbc_ffi.SqlTypeDate, vals)
    Time(vals) -> #(glodbc_ffi.SqlTypeTime, vals)
    Timestamp(vals) -> #(glodbc_ffi.SqlTypeTimestamp, vals)
    Integer(vals) -> #(glodbc_ffi.SqlInteger, vals)
    Smallint(vals) -> #(glodbc_ffi.SqlSmallint, vals)
    Real(vals) -> #(glodbc_ffi.SqlReal, vals)
    Double(vals) -> #(glodbc_ffi.SqlDouble, vals)
    Tinyint(vals) -> #(glodbc_ffi.SqlTinyint, vals)
    Bit(vals) -> #(glodbc_ffi.SqlBit, vals)
  }
}

pub fn char(c: String, size: Int) -> ODBCQueryParam {
  Char([coerce(c)], size)
}

pub fn wchar(s: String, size: Int) -> ODBCQueryParam {
  Wchar([coerce(s)], size)
}

pub fn varchar(s: String, size: Int) -> ODBCQueryParam {
  Varchar([coerce(s)], size)
}

pub fn longvarchar(s: String, size: Int) -> ODBCQueryParam {
  Longvarchar([coerce(s)], size)
}

pub fn wvarchar(s: String, size: Int) -> ODBCQueryParam {
  Wvarchar([coerce(s)], size)
}

pub fn wlongvarchar(s: String, size: Int) -> ODBCQueryParam {
  Wlongvarchar([coerce(s)], size)
}

pub fn numeric(i: numeric, prec: Int, size: Int) -> ODBCQueryParam {
  Numeric([coerce(i)], prec, size)
}

pub fn decimal(i: Float, prec: Int, size: Int) -> ODBCQueryParam {
  Decimal([coerce(i)], prec, size)
}

pub fn float(i: Float, prec: Int) -> ODBCQueryParam {
  Float([coerce(i)], prec)
}

pub fn binary(s: String) -> ODBCQueryParam {
  Binary([coerce(s)])
}

pub fn varbinary(s: String) -> ODBCQueryParam {
  Varbinary([coerce(s)])
}

pub fn longvarbinary(s: String) -> ODBCQueryParam {
  Longvarbinary([coerce(s)])
}

pub fn date(s: String) -> ODBCQueryParam {
  Date([coerce(s)])
}

pub fn time(s: String) -> ODBCQueryParam {
  Time([coerce(s)])
}

pub fn timestamp(dt: #(#(Int, Int, Int), #(Int, Int, Int))) -> ODBCQueryParam {
  Timestamp([coerce(dt)])
}

pub fn integer(i: Int) -> ODBCQueryParam {
  Integer([coerce(i)])
}

pub fn smallint(i: Int) -> ODBCQueryParam {
  Smallint([coerce(i)])
}

pub fn real(i: Int) -> ODBCQueryParam {
  Real([coerce(i)])
}

pub fn double(i: Int) -> ODBCQueryParam {
  Double([coerce(i)])
}

pub fn tinyint(i: Int) -> ODBCQueryParam {
  Tinyint([coerce(i)])
}

pub fn bit(i: Int) -> ODBCQueryParam {
  Bit([coerce(i)])
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
      Error(glodbc_ffi.ConnectionError)
    }
  }
}

pub fn disconnect(connection: Connection) -> Result(ODBCSuccess, ODBCError) {
  case odbc_disconnect(connection) {
    Error(_) -> Error(glodbc_ffi.DisconnectionError)
    _ -> Ok(ODBCOk)
  }
}

pub fn sql_query(
  connection: Connection,
  sql_query: String,
) -> Result(QueryResult, ODBCError) {
  case odbc_sql_query(connection, charlist.from_string(sql_query)) {
    Error(_) -> Error(glodbc_ffi.QueryError)
    result -> result
  }
}

fn all_same_length(lists) {
  case lists {
    [] -> True
    [list, ..lists] -> {
      let desired = list.length(list)
      list.all(lists, fn(list) { list.length(list) == desired })
    }
  }
}

/// I know now a good name but its NOT for public use :)
/// asserts that each row is the same length and is not empty
pub fn paramlist_to_erlparamlist(
  paramlist: List(List(ODBCQueryParam)),
) -> List(ODBCQueryParam) {
  // all list not empty
  assert paramlist |> list.map(list.length) |> list.all(fn(x) { x != 0 })
  // all list same length
  assert paramlist |> all_same_length()

  let values =
    paramlist
    |> list.map(fn(x) {
      list.map(x, fn(y) {
        case y {
          Char(vals, _) -> vals
          Binary(vals) -> vals
          Bit(vals) -> vals
          Date(vals) -> vals
          Decimal(vals, _, _) -> vals
          Double(vals) -> vals
          Float(vals, _) -> vals
          Integer(vals) -> vals
          Longvarbinary(vals) -> vals
          Longvarchar(vals, _) -> vals
          Numeric(vals, _, _) -> vals
          Real(vals) -> vals
          Smallint(vals) -> vals
          Time(vals) -> vals
          Timestamp(vals) -> vals
          Tinyint(vals) -> vals
          Varbinary(vals) -> vals
          Varchar(vals, _) -> vals
          Wchar(vals, _) -> vals
          Wlongvarchar(vals, _) -> vals
          Wvarchar(vals, _) -> vals
        }
      })
    })
    |> list.transpose()
    |> list.map(list.flatten)

  let types = paramlist |> list.transpose()

  list.zip(values, types)
  |> list.map(fn(x) {
    let assert Ok(t) = list.first(x.1)
    case t {
      Binary(_) -> Binary(x.0)
      Bit(_) -> Bit(x.0)
      Char(_, s) -> Char(x.0, s)
      Date(_) -> Date(x.0)
      Decimal(_, p, s) -> Decimal(x.0, p, s)
      Double(_) -> Double(x.0)
      Float(_, p) -> Float(x.0, p)
      Integer(_) -> Integer(x.0)
      Longvarbinary(_) -> Longvarbinary(x.0)
      Longvarchar(_, s) -> Longvarchar(x.0, s)
      Numeric(_, p, s) -> Numeric(x.0, p, s)
      Real(_) -> Real(x.0)
      Smallint(_) -> Smallint(x.0)
      Time(_) -> Time(x.0)
      Timestamp(_) -> Timestamp(x.0)
      Tinyint(_) -> Tinyint(x.0)
      Varbinary(_) -> Varbinary(x.0)
      Varchar(_, s) -> Varchar(x.0, s)
      Wchar(_, s) -> Wchar(x.0, s)
      Wlongvarchar(_, s) -> Wlongvarchar(x.0, s)
      Wvarchar(_, s) -> Wvarchar(x.0, s)
    }
  })
}

pub fn param_query(
  connection: Connection,
  sql_query: String,
  params: List(List(ODBCQueryParam)),
) -> Result(QueryResult, ODBCError) {
  let erl_params =
    params
    |> paramlist_to_erlparamlist
    |> list.map(convert_to_erl)

  case
    odbc_param_query(connection, charlist.from_string(sql_query), erl_params)
  {
    Error(_) -> Error(glodbc_ffi.QueryError)
    result -> result
  }
}

pub fn commit(
  connection: Connection,
  commit_mode: ODBCCommitMode,
) -> Result(ODBCSuccess, ODBCError) {
  case odbc_commit(connection, commit_mode) {
    Error(_) -> Error(glodbc_ffi.CommitError)
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
    Error(_) -> Error(glodbc_ffi.DescribeTableError)
  }
}

pub fn select(conn: Connection, position: ODBCSelectPosition, n: Int) {
  case odbc_select(conn, position, n) {
    Error(err) -> {
      let _ = io.debug(err)
      Error(glodbc_ffi.SelectError)
    }
    selected -> Ok(selected)
  }
}

pub fn select_count(conn: Connection, query: String) -> Result(Int, ODBCError) {
  case odbc_select_count(conn, query) {
    Error(_) -> Error(glodbc_ffi.SelectCountError)
    rows -> rows
  }
}
