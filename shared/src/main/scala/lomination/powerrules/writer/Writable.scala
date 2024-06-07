package lomination.powerrules.writer

import lomination.powerrules.TmpTile

trait Writable[A]:
  extension (a: A) def write(using TmpTile): String
