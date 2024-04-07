package lomination.ddnettools.writers

import lomination.ddnettools.DefaultTile

trait Writable[A]:
  extension (a: A) def write(using DefaultTile): String
