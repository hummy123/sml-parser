structure LongIdDfa =
struct
  (* DFA is from right-to-left, not left-to-right. *)

  val dead = 0
  val start = 1
  val consecutiveAfterDot = 2
  val alphaAfterDot = 3
  val dot = 4
  val consecutiveBeforeDot = 5
  val alphaBeforeDot = 6

  fun isFinal curState = curState = alphaBeforeDot

  fun makeDead _ = dead

  (* consecutiveAfterDot has same transitions *)
  fun makeStart i =
    let
      val chr = Char.chr i
    in
      if Char.isAlpha chr then
        alphaAfterDot
      else if Char.isDigit chr orelse chr = #"_" orelse chr = #"'" then
        consecutiveAfterDot
      else
        dead
    end

  fun makeAlphaAfterDot i =
    let
      val chr = Char.chr i
    in
      if Char.isAlpha chr then
        alphaAfterDot
      else if chr = #"." then
        dot
      else if Char.isDigit chr orelse chr = #"_" orelse chr = #"'" then
        consecutiveAfterDot
      else
        dead
    end

  (* consecutiveBeforeDot has same transitions *)
  fun makeDot i =
    let
      val chr = Char.chr i
    in
      if Char.isAlpha chr then
        alphaBeforeDot
      else if Char.isDigit chr orelse chr = #"_" orelse chr = #"'" then
        consecutiveBeforeDot
      else
        dead
    end

  fun makeAlphaBeforeDot i =
    let
      val chr = Char.chr i
    in
      if Char.isAlpha chr then
        alphaBeforeDot
      else if chr = #"." then
        dot
      else if chr = #"_" orelse chr = #"'" orelse Char.isDigit chr then
        consecutiveBeforeDot
      else
        dead
    end

  val deadTable = Vector.tabulate (255, makeDead)
  val startTable = Vector.tabulate (255, makeStart)
  val consecutiveAfterDotTable = startTable
  val alphaAfterDotTable = Vector.tabulate (255, makeAlphaAfterDot)
  val dotTable = Vector.tabulate (255, makeDot)
  val consecutiveBeforeDotTable = dotTable
  val alphaBeforeDotTable = Vector.tabulate (255, makeAlphaBeforeDot)

  val states = vector
    [ deadTable
    , startTable
    , consecutiveAfterDotTable
    , alphaAfterDotTable
    , dotTable
    , consecutiveBeforeDotTable
    , alphaBeforeDotTable
    ]

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
