structure PunctDfa =
struct
  val dead = 0
  val start = 1
  val final = 2

  fun mkDead _ = dead

  fun mkStart i =
    let
      val chr = Char.chr i
    in
      case chr of
        #"!" => final
      | #"%" => final
      | #"&" => final
      | #"$" => final
      | #"#" => final
      | #"+" => final
      | #"-" => final
      | #"/" => final
      | #":" => final
      | #"<" => final
      | #"=" => final
      | #">" => final
      | #"?" => final
      | #"@" => final
      | #"\\" => final
      | #"~" => final
      (* todo: apostrophe? *)
      | #"^" => final
      | #"|" => final
      | #"*" => final
      | _ => dead
    end

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val finalTable = startTable

  val states = vector [deadTable, startTable, finalTable]

  fun isFinal i = i = final

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
