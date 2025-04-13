structure TypeIdDfa =
struct
  val deadState = 0
  val startState = 1
  val primeState = 2

  val primeLoopState = 3

  fun makeDead _ = deadState

  fun makeStart pos =
    if Char.chr pos = #"'" then primeState else deadState

  fun makePrime pos =
    let val chr = Char.chr pos
    in if Char.isAlpha chr then primeLoopState else deadState
    end

  fun makePrimeLoop pos =
    let
      val chr = Char.chr pos
    in
      if Char.isAlphaNum chr orelse chr = #"'" then primeLoopState
      else deadState
    end

  fun isFinal i = i = primeLoopState

  val deadTable = Vector.tabulate (255, makeDead)
  val startTable = Vector.tabulate (255, makeStart)
  val primeTable = Vector.tabulate (255, makePrime)
  val primeLoopTable = Vector.tabulate (255, makePrimeLoop)

  val states = vector [deadTable, startTable, primeTable, primeLoopTable]

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
