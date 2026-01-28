import gleeunit
import gleeunit/should
import glodbc
import glodbc_ffi

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let test_vals = [
    [glodbc.integer(1), glodbc.varchar("Hii", 10)],
    [glodbc.integer(1), glodbc.varchar("Hii", 10)],
    [glodbc.integer(1), glodbc.varchar("Hii", 10)],
  ]
  let result = glodbc.paramlist_to_erlparamlist(test_vals)
  let expected = [
    glodbc.Integer([
      glodbc_ffi.coerce(1),
      glodbc_ffi.coerce(1),
      glodbc_ffi.coerce(1),
    ]),
    glodbc.Varchar(
      [
        glodbc_ffi.coerce("Hii"),
        glodbc_ffi.coerce("Hii"),
        glodbc_ffi.coerce("Hii"),
      ],
      10,
    ),
  ]
  result |> should.equal(expected)
}
