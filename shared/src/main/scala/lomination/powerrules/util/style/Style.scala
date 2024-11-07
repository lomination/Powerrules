package lomination.powerrules.util.style

// Note that the following codes do not support parameters but may need some

/** Concatenates the given ansi escape code */
def ansi(codes: Int*): String =
  codes.map(s"\u001b[" + _ + "m").mkString

/** Reset */
def ansi0: String = s"\u001b[0m"

/** Increased intensity */
def ansi1: String = s"\u001b[1m"

/** Decreased intensity */
def ansi2: String = s"\u001b[2m"

/** Italic */
def ansi3: String = s"\u001b[3m"

/** Underline */
def ansi4: String = s"\u001b[4m"

/** Slow blink */
def ansi5: String = s"\u001b[5m"

/** Rapid blink */
def ansi6: String = s"\u001b[6m"

/** Reverse video */
def ansi7: String = s"\u001b[7m"

/** Conceal or hide */
def ansi8: String = s"\u001b[8m"

/** Crossed-out */
def ansi9: String = s"\u001b[9m"

/** Default font */
def ansi10: String = s"\u001b[10m"

/** Alternative font 1 */
def ansi11: String = s"\u001b[11m"

/** Alternative font 2 */
def ansi12: String = s"\u001b[12m"

/** Alternative font 3 */
def ansi13: String = s"\u001b[13m"

/** Alternative font 4 */
def ansi14: String = s"\u001b[14m"

/** Alternative font 5 */
def ansi15: String = s"\u001b[15m"

/** Alternative font 6 */
def ansi16: String = s"\u001b[16m"

/** Alternative font 7 */
def ansi17: String = s"\u001b[17m"

/** Alternative font 8 */
def ansi18: String = s"\u001b[18m"

/** Alternative font 9 */
def ansi19: String = s"\u001b[19m"

/** Fraktur (Gothic) */
def ansi20: String = s"\u001b[20m"

/** Doubly underlined */
def ansi21: String = s"\u001b[21m"

/** Normal intensity */
def ansi22: String = s"\u001b[22m"

/** Neither italic, nor blackletter */
def ansi23: String = s"\u001b[23m"

/** Not underlined */
def ansi24: String = s"\u001b[24m"

/** Not underlined */
def ansi25: String = s"\u001b[25m"

/** Proportional spacing */
def ansi26: String = s"\u001b[26m"

/** Not reversed */
def ansi27: String = s"\u001b[27m"

/** Reveal */
def ansi28: String = s"\u001b[28m"

/** Not crossed out */
def ansi29: String = s"\u001b[29m"

/** Black foreground */
def ansi30: String = s"\u001b[30m"

/** Red foreground */
def ansi31: String = s"\u001b[31m"

/** Green foreground */
def ansi32: String = s"\u001b[32m"

/** Yellow foreground */
def ansi33: String = s"\u001b[33m"

/** Blue foreground */
def ansi34: String = s"\u001b[34m"

/** Magenta foreground */
def ansi35: String = s"\u001b[35m"

/** Cyan foreground */
def ansi36: String = s"\u001b[36m"

/** White foreground */
def ansi37: String = s"\u001b[37m"

/** Custom foreground color */
def ansi38: String = s"\u001b[38m"

/** Default foreground color */
def ansi39: String = s"\u001b[39m"

/** Black background */
def ansi40: String = s"\u001b[40m"

/** Red background */
def ansi41: String = s"\u001b[41m"

/** Green background */
def ansi42: String = s"\u001b[42m"

/** Yellow background */
def ansi43: String = s"\u001b[43m"

/** Blue background */
def ansi44: String = s"\u001b[44m"

/** Magenta background */
def ansi45: String = s"\u001b[45m"

/** Cyan background */
def ansi46: String = s"\u001b[46m"

/** White background */
def ansi47: String = s"\u001b[47m"

/** Custom background color */
def ansi48: String = s"\u001b[48m"

/** Default background color */
def ansi49: String = s"\u001b[49m"

/** Disable proportional spacing */
def ansi50: String = s"\u001b[50m"

/** Framed */
def ansi51: String = s"\u001b[51m"

/** Encircled */
def ansi52: String = s"\u001b[52m"

/** Overlined */
def ansi53: String = s"\u001b[53m"

/** Neither framed nor encircled */
def ansi54: String = s"\u001b[54m"

/** Not overlined */
def ansi55: String = s"\u001b[55m"

def ansi56: String = s"\u001b[56m"

def ansi57: String = s"\u001b[57m"

/** Set underline color */
def ansi58: String = s"\u001b[58m"

/** Default underline color */
def ansi59: String = s"\u001b[59m"

/** Ideogram underline or right side line */
def ansi60: String = s"\u001b[60m"

/** Ideogram double underline, or double line on the right side */
def ansi61: String = s"\u001b[61m"

/** Ideogram overline or left side line */
def ansi62: String = s"\u001b[62m"

/** Ideogram double overline, or double line on the left side */
def ansi63: String = s"\u001b[63m"

/** Ideogram stress marking */
def ansi64: String = s"\u001b[64m"

/** No ideogram attributes */
def ansi65: String = s"\u001b[65m"

def ansi66: String = s"\u001b[66m"

def ansi67: String = s"\u001b[67m"

def ansi68: String = s"\u001b[68m"

def ansi69: String = s"\u001b[69m"

def ansi70: String = s"\u001b[70m"

def ansi71: String = s"\u001b[71m"

def ansi72: String = s"\u001b[72m"

/** Superscript */
def ansi73: String = s"\u001b[73m"

/** Subscript */
def ansi74: String = s"\u001b[74m"

/** Neither superscript nor subscript */
def ansi75: String = s"\u001b[75m"

def ansi76: String = s"\u001b[76m"

def ansi77: String = s"\u001b[77m"

def ansi78: String = s"\u001b[78m"

def ansi79: String = s"\u001b[79m"

def ansi80: String = s"\u001b[80m"

def ansi81: String = s"\u001b[81m"

def ansi82: String = s"\u001b[82m"

def ansi83: String = s"\u001b[83m"

def ansi84: String = s"\u001b[84m"

def ansi85: String = s"\u001b[85m"

def ansi86: String = s"\u001b[86m"

def ansi87: String = s"\u001b[87m"

def ansi88: String = s"\u001b[88m"

def ansi89: String = s"\u001b[89m"

/** Bright black (grey) foreground */
def ansi90: String = s"\u001b[90m"

/** Bright red foreground */
def ansi91: String = s"\u001b[91m"

/** Bright green foreground */
def ansi92: String = s"\u001b[92m"

/** Bright yellow foreground */
def ansi93: String = s"\u001b[93m"

/** Bright blue foreground */
def ansi94: String = s"\u001b[94m"

/** Bright magenta foreground */
def ansi95: String = s"\u001b[95m"

/** Bright cyan foreground */
def ansi96: String = s"\u001b[96m"

/** Bright white foreground */
def ansi97: String = s"\u001b[97m"

def ansi98: String = s"\u001b[98m"

def ansi99: String = s"\u001b[99m"

/** Bright black (grey) background */
def ansi100: String = s"\u001b[100m"

/** Bright red background */
def ansi101: String = s"\u001b[101m"

/** Bright green background */
def ansi102: String = s"\u001b[102m"

/** Bright yellow background */
def ansi103: String = s"\u001b[103m"

/** Bright blue background */
def ansi104: String = s"\u001b[104m"

/** Bright magenta background */
def ansi105: String = s"\u001b[105m"

/** Bright cyan background */
def ansi106: String = s"\u001b[106m"

/** Bright white background */
def ansi107: String = s"\u001b[107m"
