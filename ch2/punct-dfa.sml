structure PunctDfa =
struct
  val dead = 0
  val comma = 1
  val colon = 2
  val colonEquals = 3
  val semicolon = 4
  val lparen = 5
  val rparen = 6
  val lbracket = 7
  val rbracket = 8
  val lbrace = 9
  val rbrace = 10
  val dot = 11
  val plus = 12
  val minus = 13
  val asterisk = 14
  val slash = 15
  val equals = 16

  val lessThan = 17
  val greaterThan = 18
  val ampersand = 19
  val pipe = 20

  val notEquals = 21
  val lessThanOrEqual = 22
  val greaterThanOrEqual = 23
  val start = 24

  fun mkStart i =
    let
      val chr = Char.chr i
    in
      case chr of
        #"," => comma
      | #":" => colon
      | #";" => semicolon
      | #"(" => lparen
      | #")" => rparen
      | #"[" => lbracket
      | #"]" => rbracket
      | #"{" => lbrace
      | #"}" => rbrace
      | #"." => dot
      | #"+" => plus
      | #"-" => minus
      | #"*" => asterisk
      | #"/" => slash
      | #"=" => equals
      | #"<" => lessThan
      | #">" => greaterThan
      | #"&" => ampersand
      | #"|" => pipe
      | _ => dead
    end

  fun mkDead _ = 0

  fun mkColon i =
    if Char.chr i = #"=" then colonEquals else dead

  fun mkLessThan i =
    let
      val chr = Char.chr i
    in
      if chr = #">" then notEquals
      else if chr = #"=" then lessThanOrEqual
      else dead
    end

  fun mkGreaterThan i =
    if Char.chr i = #"=" then greaterThanOrEqual else dead

  val deadTable = Vector.tabulate (255, mkDead)

  val commaTable = deadTable
  val colonTable = Vector.tabulate (255, mkColon)
  val colonEqualsTable = deadTable
  val semicolonTable = deadTable

  val lparenTable = deadTable
  val rparenTable = deadTable

  val lbracketTable = deadTable
  val rbracketTable = deadTable

  val lbraceTable = deadTable
  val rbraceTable = deadTable

  val lbraceTable = deadTable
  val rbraceTable = deadTable

  val dotTable = deadTable
  val plusTable = deadTable
  val minusTable = deadTable
  val asteriskTable = deadTable
  val slashTable = deadTable
  val equalsTable = deadTable

  val lessThanTable = Vector.tabulate (255, mkLessThan)
  val greaterThanTable = Vector.tabulate (255, mkGreaterThan)

  val ampersandTable = deadTable
  val pipeTable = deadTable

  val notEqualsTable = deadTable
  val lessThanOrEqualTable = deadTable
  val greaterThanOrEqualTable = deadTable

  val startTable = Vector.tabulate (255, mkStart)

  val states = vector
    [ deadTable
    , commaTable
    , colonTable
    , colonEqualsTable
    , semicolonTable
    , lparenTable
    , rparenTable
    , lbracketTable
    , rbracketTable
    , lbraceTable
    , rbraceTable
    , dotTable
    , plusTable
    , minusTable
    , asteriskTable
    , slashTable
    , equalsTable
    , lessThanTable
    , greaterThanTable
    , ampersandTable
    , pipeTable
    , notEqualsTable
    , lessThanOrEqualTable
    , greaterThanOrEqualTable
    , startTable
    ]

  fun isFinal i = i <> dead andalso i <> start

  fun getNewState (chr, prevState) =
    let val table = Vector.sub (states, prevState)
    in Vector.sub (table, Char.ord chr)
    end
end
