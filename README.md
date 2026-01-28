# glodbc

[![Package Version](https://img.shields.io/hexpm/v/glodbc)](https://hex.pm/packages/glodbc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glodbc/)

Gleam ODBC Library that is based on the odbc erlang library.

```sh
gleam add glodbc@1
```

Example Usage
```gleam
import glodbc

pub fn main() {
  // test with mariadb but can be done with any other odbc database
  let connstring =
    "Driver={MariaDB ODBC 3.0 Driver};DSN=localhost;UID=user;PWD=password"
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
```

Further documentation can be found at <https://hexdocs.pm/glodbc>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
