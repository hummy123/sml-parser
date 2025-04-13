structure TypeIdDfa =
struct
  val dead = 0
  val start = 1
  val prime = 2

  fun makeDead _ = dead

  fun makeStart pos =
    if Char.chr pos = #"'" then prime else dead

  fun makePrime pos =
    let val chr = Char.chr pos
    in if Char.isAlpha chr orelse chr = #"'" then prime else dead
    end

  fun isFinal i = i = prime

  val deadTable = Vector.tabulate (255, makeDead)
  val startTable = Vector.tabulate (255, makeStart)
  val primeTable = Vector.tabulate (255, makePrime)

  val states = vector [deadTable, startTable, primeTable]

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
