structure IntDfa =
struct
  val dead = 0
  val start = 1
  val final = 2

  fun mkDead _ = dead

  fun mkStart i =
    if Char.isDigit (Char.chr i) then start else dead

  fun mkFinal i =
    if Char.isDigit (Char.chr i) then final else dead

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val finalTable = Vector.tabulate (255, mkFinal)

  val states = vector [deadTable, startTable, finalTable]

  fun isFinal i = i = final

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
