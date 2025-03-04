structure Lexer =
struct
  fun areAllDead (idState, intState) = idState = 0 andalso intState = 0

  datatype token =
    INT of int
  | ID of string
  | BLANK

  (* reserved words *)
  | WHILE
  | FOR
  | TO
  | BREAK
  | LET
  | IN
  | END
  | FUNCTION
  | VAR
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | DO
  | OF
  | NIL

  (* punctuation *)
  | COMMA
  | COLON
  | SEMI_COLON
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_BRACE
  | R_BRACE
  | DOT
  | PLUS
  | MINUS
  | ASTERISK
  | SLASH
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_OR_EQUAL
  | GREATER_THAN
  | GREATER_THAN_OR_EQUAL
  | AMPERSAND
  | PIPE
  | COLON_EQUALS

  fun getWordOrID str =
    case str of
      "while" => WHILE
    | "for" => FOR
    | "to" => TO
    | "break" => BREAK
    | "let" => LET
    | "in" => IN
    | "end" => END
    | "function" => FUNCTION
    | "var" => VAR
    | "type" => TYPE
    | "array" => ARRAY
    | "if" => IF
    | "then" => THEN
    | "else" => ELSE
    | "do" => DO
    | "of" => OF
    | "nil" => NIL
    | _ => ID str

  fun getPunct str =
    case str of
      "," => COMMA
    | "" => COLON
    | ";" => SEMI_COLON
    | "(" => L_PAREN
    | ")" => R_PAREN
    | "[" => L_BRACKET
    | "]" => R_BRACKET
    | "{" => L_BRACE
    | "}" => R_BRACE
    | "." => DOT
    | "+" => PLUS
    | "-" => MINUS
    | "*" => ASTERISK
    | "/" => SLASH
    | "=" => EQUALS
    | "<>" => NOT_EQUALS
    | "<" => LESS_THAN
    | "<=" => LESS_OR_EQUAL
    | ">" => GREATER_THAN
    | ">=" => GREATER_THAN_OR_EQUAL
    | "&" => AMPERSAND
    | "|" => PIPE
    | ":=" => COLON_EQUALS
    | _ => raise Empty

  fun getMax (str, start, lastFinalID, lastFinalInt, lastFinalPunct) =
    let
      val max = Int.max (lastFinalID, lastFinalInt)
      val max = Int.max (max, lastFinalPunct)
      val str = String.substring (str, start, max - start + 1)
    in
      if max = lastFinalID then
        (lastFinalID, getWordOrID str)
      else if max = lastFinalInt then
        case Int.fromString str of
          SOME num => (lastFinalInt, INT num)
        | NONE => raise Size
      else
        (* max = lastFinalPunct *)
        (lastFinalPunct, getPunct str)
    end

  fun getToken (str, start, lastFinalID, lastFinalInt, lastFinalPunct) =
    if lastFinalID = ~1 andalso lastFinalInt = ~1 andalso lastFinalPunct = ~1 then
      (start, BLANK)
    else
      getMax (str, start, lastFinalID, lastFinalInt, lastFinalPunct)

  fun helpGetTokenEndPos
    ( pos
    , str
    , idState
    , intState
    , punctState
    , lastFinalID
    , lastFinalInt
    , lastFinalPunct
    , start
    ) =
    if pos = String.size str then
      getToken (str, start, lastFinalID, lastFinalInt, lastFinalPunct)
    else
      let
        val chr = String.sub (str, pos)
        val newIdState = IdDfa.getNewState (chr, idState)
        val newIntState = IntDfa.getNewState (chr, intState)
        val newPunctState = PunctDfa.getNewState (chr, punctState)
      in
        if areAllDead (newIdState, newIntState) then
          getToken (str, start, lastFinalID, lastFinalInt, lastFinalPunct)
        else
          let
            val lastFinalID =
              if IdDfa.isFinal newIdState then pos else lastFinalID

            val lastFinalInt =
              if IntDfa.isFinal newIntState then pos else lastFinalInt
          in
            helpGetTokenEndPos
              ( pos + 1
              , str
              , newIdState
              , newIntState
              , newPunctState
              , lastFinalID
              , lastFinalInt
              , lastFinalPunct
              , start
              )
          end
      end

  fun getTokenEndPos (pos, str) =
    helpGetTokenEndPos
      (pos, str, IdDfa.start, IntDfa.start, PunctDfa.start, ~1, ~1, ~1, pos)

  fun helpGetTokens (pos, str, acc) =
    if pos = String.size str then
      List.rev acc
    else
      let
        val (newPos, token) = getTokenEndPos (pos, str)
      in
        case token of
          BLANK => helpGetTokens (pos + 1, str, acc)
        | _ => helpGetTokens (newPos + 1, str, token :: acc)
      end

  fun getTokens str = helpGetTokens (0, str, [])
end
