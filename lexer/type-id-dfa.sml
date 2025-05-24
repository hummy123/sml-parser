structure TypeIdDfa =
struct
  (* dfa processed from reverse, right to left: 
   * so first char processed can be any alphanumeric or _ or '
   * but last char processed must be a ' prime. *)

  val dead = 0
  val start = 1
  val consecutiveChar = 2
  val final = 3

  fun makeDead _ = dead

  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if Char.isAlphaNum chr orelse chr = #"_" then consecutiveChar
      else if chr = #"'" then final
      else dead
    end

  fun isFinal i = i = final

  val deadTable = Vector.tabulate (255, makeDead)
  val startTable = Vector.tabulate (255, makeStart)
  val consecutiveCharTable = startTable
  val finalTable = startTable

  val states = vector [deadTable, startTable, consecutiveCharTable, finalTable]

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
