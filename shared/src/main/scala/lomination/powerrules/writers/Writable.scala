package lomination.powerrules.writers

import lomination.powerrules.DefaultTile

trait Writable[A]:
  extension (a: A) def write(using DefaultTile): String
