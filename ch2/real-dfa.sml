structure RealDfa =
struct
  val dead = 0
  val start = 1
  val beforeDot = 2
  val dot = 3
  val afterDot = 4

  fun mkDead _ = dead

  fun mkAfterDot i =
    if Char.isDigit (Char.chr i) then afterDot else dead

  fun mkDot i =
    if Char.isDigit (Char.chr i) then afterDot else dead

  fun mkBeforeDot i =
    let val chr = Char.chr i
    in if Char.isDigit chr then beforeDot else if chr = #"." then dot else dead
    end

  fun mkStart i =
    if Char.isDigit (Char.chr i) then beforeDot else dead

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val beforeDotTable = Vector.tabulate (255, mkBeforeDot)
  val dotTable = Vector.tabulate (255, mkDot)
  val afterDotTable = Vector.tabulate (255, mkAfterDot)

  val states = vector
    [deadTable, startTable, beforeDotTable, dotTable, afterDotTable]

  fun isFinal i = i = afterDot

  fun helpIsReal (str, pos, state) =
    if pos = String.size str then
      isFinal state
    else
      let
        val table = Vector.sub (states, state)
        val chr = String.sub (str, pos)
        val state = Vector.sub (table, Char.ord chr)
      in
        helpIsReal (str, pos + 1, state)
      end

  fun isReal (str, pos) = helpIsReal (str, pos, start)

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
