structure IdDfa =
struct
  val dead = 0
  val start = 1
  val final = 2

  fun mkDead _ = 0

  fun mkStart i =
    let val chr = Char.chr i
    in if Char.isAlpha chr then final else dead
    end

  fun mkFinal i =
    let val chr = Char.chr i
    in if Char.isAlphaNum chr orelse chr = #"_" then final else dead
    end

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val finalTable = Vector.tabulate (255, mkFinal)

  val states = vector [deadTable, startTable, finalTable]

  fun isFinal i = i = final orelse i = start

  fun helpIsId (str, pos, state) =
    if pos = String.size str then
      isFinal state
    else
      let
        val table = Vector.sub (states, state)
        val chr = String.sub (str, pos)
        val state = Vector.sub (table, Char.ord chr)
      in
        helpIsId (str, pos + 1, state)
      end

  fun isId (str, pos) = helpIsId (str, pos, start)

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
