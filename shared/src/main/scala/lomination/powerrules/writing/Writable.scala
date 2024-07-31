package lomination.powerrules.writing

import lomination.powerrules.TmpTile

trait Writable[A]:

  extension (a: A)
    /** Converts this Powerrules token into DDNet rules */
    def write(using TmpTile): String

  extension (a: A)
    /** Converts this Powerrules token into DDNet rules */
    @inline def w(using TmpTile): String = a.write
