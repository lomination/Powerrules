package lomination.ddnettools.writers

trait Writable[A]:
  extension (a: A) def write: String

