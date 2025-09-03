package lomination.powerrules.writing

import lomination.powerrules.config.Config

trait Writable[A]:

  extension (a: A)
    /** Converts this PowerRules token into DDNet rules */
    def write(using Config): String

  extension (a: A)
    /** Converts this PowerRules token into DDNet rules */
    @inline def w(using Config): String = a.write
