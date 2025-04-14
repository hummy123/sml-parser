structure PunctDfa =
struct
  val dead = 0
  val start = 1
  val singleton = 2
  val dot = 3
  val dotdot = 4
  val dotdotdot = 5
  val final = 6

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

      | #"(" => singleton
      | #")" => singleton
      | #"[" => singleton
      | #"]" => singleton
      | #"{" => singleton
      | #"}" => singleton
      | #"," => singleton
      | #"." => dot
      | _ => dead
    end

  fun mkLoop i =
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

  fun makeDot i =
    if Char.chr i = #"." then dotdot else dead

  fun makeDotDot i =
    if Char.chr i = #"." then dotdotdot else dead

  val deadTable = Vector.tabulate (255, mkDead)
  val startTable = Vector.tabulate (255, mkStart)
  val singletonTable = deadTable

  val dotTable = Vector.tabulate (255, makeDot)
  val dotDotTable = Vector.tabulate (255, makeDotDot)
  val dotDotDotTable = deadTable

  val loopTable = Vector.tabulate (255, mkLoop)

  val states = vector
    [ deadTable
    , startTable
    , singletonTable
    , dotTable
    , dotDotTable
    , dotDotDotTable
    , loopTable
    ]

  fun isFinal i =
    i = final orelse i = singleton orelse i = dotdotdot

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
