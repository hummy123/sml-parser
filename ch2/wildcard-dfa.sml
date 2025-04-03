structure WildcardDfa =
struct
  val dead = 0
  val final = 1
  val start = 2

  fun mkDead _ = dead

  fun mkStart num =
    if Char.chr num = #"_" then final else dead

  val deadTable = Vector.tabulate (255, mkDead)
  val finalTable = deadTable
  val startTable = Vector.tabulate (255, mkStart)

  val states = vector [deadTable, finalTable, startTable]

  fun isFinal num = num = final

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
