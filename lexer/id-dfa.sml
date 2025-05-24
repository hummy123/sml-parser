structure IdDfa =
struct
  (* dfa is processed in reverse, from right to left 
  * (from last char in string to first char)
   * first char of ID must be alpha, 
   * but consecutive chars could be alpha, numeric, or underscore.
   * This DFA works when an ID is read from the end of the string to the start. *)

  val dead = 0
  val start = 1
  val consecutiveChar = 2
  val final = 3

  fun mkDead _ = 0

  fun mkStart i =
    let
      val chr = Char.chr i
    in
      if Char.isAlpha chr then
        final
      else if Char.isDigit chr orelse chr = #"_" orelse chr = #"'" then
        consecutiveChar
      else
        dead
    end

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val consecutiveCharTable = startTable
  val finalTable = startTable

  val states = vector [deadTable, startTable, consecutiveCharTable, finalTable]

  fun isFinal i = i = final

  fun helpIsId (str, pos, state) =
    if pos < 0 then
      isFinal state
    else
      let
        val chr = String.sub (str, pos)
        val table = Vector.sub (states, state)
        val state = Vector.sub (table, Char.ord chr)
      in
        helpIsId (str, pos - 1, state)
      end

  fun isId str =
    helpIsId (str, String.size str - 1, start)

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
