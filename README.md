# glodbc

[![Package Version](https://img.shields.io/hexpm/v/glodbc)](https://hex.pm/packages/glodbc)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glodbc/)

Gleam ODBC Library that is based on the odbc erlang library.

```sh
gleam add glodbc@1
```

Example Usage
```gleam
import glodbc.{varchar, integer}

pub fn main() {
  // Testing with MariaDB, other ODBC Databases work too of course! Just have the right driver!
  let connstring =
    "Driver={MariaDB ODBC 3.0 Driver};DSN=localhost;UID=testuser;PWD=password"
  let assert Ok(conn) = glodbc.connect(connstring, [#(AutoCommit, True)])

  let assert Ok(glodbc.Selected(_col_names, _rows)) =
    glodbc.sql_query(conn, "Select * from test.testtable;")

  let assert Ok(glodbc.Updated(_rowcount)) =
    glodbc.sql_query(conn, "Update test.testtable set name='Fred' where id=1")

  let assert Ok(glodbc.Updated(_rowcount)) =
    glodbc.sql_query(
      conn,
      "Insert into test.testtable (id, name, age) values (2, 'Jonas', 18)",
    )

  let assert Ok(Selected(_col_names, _rows)) =
    glodbc.param_query(conn, "Select age from test.testtable where name=? and id=?", [
      [varchar("Fred", 100), integer(1)],
      [varchar("Jonas", 100), integer(2)],
    ])

  let assert Ok(glodbc.ODBCOk) = glodbc.commit(conn, glodbc.Commit)
  // or Rollback

  let assert Ok(_tabledescription) = glodbc.describe_table(conn, "test.testtable")
  // -> [Description("id", SqlInteger), Description("name", SqlVarchar(100)), Description("age", SqlInteger)]

  let assert Ok(glodbc.ODBCOk) = glodbc.disconnect(conn)
}
```

Further documentation can be found at <https://hexdocs.pm/glodbc>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
