signature LEXER =
sig
  datatype token =
    INT of int
  | ID of string
  | STRING of string
  | BOOL of bool
  | TYPE_ID of {isEqType: bool, id: string}

  (* reserved words *)
  | LET
  | IN
  | END
  | FUN
  | VAL
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | OF
  | INFIX
  | AS
  | OP

  (* punctuation *)
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_BRACE
  | R_BRACE
  | COMMA
  | COLON
  | SEMI_COLON
  | TRIPLE_DOT
  | WILDCARD
  | PIPE
  | EQUAL_ARROW
  | DASH_ARROW
  | HASH

  (* singleton dot is not in grammar; maybe remove? *)
  | DOT
  | EOF

  val tokenToString: token -> string

  val getTokens: string -> token list
end

structure Lexer :> LEXER =
struct
  datatype token =
    INT of int
  | ID of string
  | STRING of string
  | BOOL of bool
  | TYPE_ID of {isEqType: bool, id: string}

  (* reserved words *)
  | LET
  | IN
  | END
  | FUN
  | VAL
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | OF
  | INFIX
  | AS
  | OP

  (* punctuation *)
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_BRACE
  | R_BRACE
  | COMMA
  | COLON
  | SEMI_COLON
  | TRIPLE_DOT
  | WILDCARD
  | PIPE
  | EQUAL_ARROW
  | DASH_ARROW
  | HASH

  (* singleton dot is not in grammar; maybe remove? *)
  | DOT
  | EOF

  type all_dfa =
    { curID: int
    , curInt: int
    , curPunct: int
    , curWild: int
    , curType: int
    , curSpace: int

    , lastID: int
    , lastInt: int
    , lastPunct: int
    , lastWild: int
    , lastType: int
    , lastSpace: int
    }

  val initialDfa =
    { curID = IdDfa.start
    , curInt = IntDfa.start
    , curPunct = PunctDfa.start
    , curWild = WildcardDfa.start
    , curType = TypeIdDfa.start
    , curSpace = SpaceDfa.start

    , lastID = ~1
    , lastInt = ~1
    , lastPunct = ~1
    , lastWild = ~1
    , lastType = ~1
    , lastSpace = ~1
    }

  fun areAllDead (dfa: all_dfa) =
    let
      val {curID, curInt, curPunct, curWild, curType, curSpace, ...} = dfa
    in
      curID = 0 andalso curInt = 0 andalso curPunct = 0 andalso curWild = 0
      andalso curType = 0 andalso curSpace = 0
    end

  fun updateDfa (chr, dfa: all_dfa, pos) =
    let
      val
        { curID
        , curInt
        , curPunct
        , curWild
        , curType
        , curSpace

        , lastID
        , lastInt
        , lastPunct
        , lastWild
        , lastType
        , lastSpace
        } = dfa

      val curID = IdDfa.getNewState (chr, curID)
      val curInt = IntDfa.getNewState (chr, curInt)
      val curPunct = PunctDfa.getNewState (chr, curPunct)
      val curWild = WildcardDfa.getNewState (chr, curWild)
      val curType = TypeIdDfa.getNewState (chr, curType)
      val curSpace = SpaceDfa.getNewState (chr, curSpace)

      val lastID = if IdDfa.isFinal curID then pos else lastID
      val lastInt = if IntDfa.isFinal curInt then pos else lastInt
      val lastPunct = if PunctDfa.isFinal curPunct then pos else lastPunct
      val lastWild = if WildcardDfa.isFinal curWild then pos else lastWild
      val lastType = if TypeIdDfa.isFinal curType then pos else lastType
      val lastSpace = if SpaceDfa.isFinal curSpace then pos else lastSpace
    in
      { curID = curID
      , curInt = curInt
      , curPunct = curPunct
      , curWild = curWild
      , curType = curType
      , curSpace = curSpace

      , lastID = lastID
      , lastInt = lastInt
      , lastPunct = lastPunct
      , lastWild = lastWild
      , lastType = lastType
      , lastSpace = lastSpace
      }
    end

  fun tokenToString tok =
    case tok of
      INT num => "INT(" ^ Int.toString num ^ ")"
    | ID id => "ID(" ^ id ^ ")"
    | STRING str => "STRING(" ^ str ^ ")"
    | BOOL b => "BOOL(" ^ Bool.toString b ^ ")"
    | TYPE_ID {isEqType, id} =>
        "TYPE_ID{isEqType: " ^ Bool.toString isEqType ^ ", id: " ^ id ^ ")"

    (* reserved words *)
    | LET => "let"
    | IN => "in"
    | END => "end"
    | FUN => "fun"
    | VAL => "val"
    | TYPE => "type"
    | ARRAY => "array"
    | IF => "if"
    | THEN => "then"
    | ELSE => "else"
    | OF => "of"
    | INFIX => "infix"
    | AS => "as"
    | OP => "op"

    (* punctuation *)
    | L_PAREN => "("
    | R_PAREN => ")"
    | L_BRACKET => "["
    | R_BRACKET => "]"
    | L_BRACE => "{"
    | R_BRACE => "}"
    | COMMA => ","
    | COLON => ":"
    | SEMI_COLON => ";"
    | TRIPLE_DOT => "..."
    | WILDCARD => "_"
    | PIPE => "|"
    | EQUAL_ARROW => "=>"
    | DASH_ARROW => "->"
    | HASH => "#"

    | DOT => "."
    | EOF => "EOF"

  fun getWordOrID str =
    case str of
      "let" => LET
    | "in" => IN
    | "end" => END
    | "fun" => FUN
    | "val" => VAL
    | "type" => TYPE
    | "array" => ARRAY
    | "if" => IF
    | "then" => THEN
    | "else" => ELSE
    | "of" => OF
    | "true" => BOOL true
    | "false" => BOOL false
    | "infix" => INFIX
    | "as" => AS
    | "op" => OP
    | _ => ID str

  fun getPunct str =
    case str of
      "(" => L_PAREN
    | ")" => R_PAREN
    | "[" => L_BRACKET
    | "]" => R_BRACKET
    | "{" => L_BRACE
    | "}" => R_BRACE
    | "," => COMMA
    | ":" => COLON
    | ";" => SEMI_COLON
    | "..." => TRIPLE_DOT
    | "|" => PIPE
    | "=>" => EQUAL_ARROW
    | "->" => DASH_ARROW
    | "#" => HASH
    | "." => DOT
    | _ => ID str

  fun getTypeID str =
    let val isEqType = String.size str >= 2 andalso String.sub (str, 1) = #"'"
    in TYPE_ID {isEqType = isEqType, id = str}
    end

  fun getToken (str, start, dfa, acc) =
    let
      val {lastID, lastInt, lastPunct, lastWild, lastType, lastSpace, ...} = dfa
      val max = Int.max (lastID, lastInt)
      val max = Int.max (max, lastPunct)
      val max = Int.max (max, lastWild)
      val max = Int.max (max, lastType)
      val max = Int.max (max, lastSpace)
    in
      if max = ~1 then
        let
          val chr = String.sub (str, start)
          val str = String.implode [chr]
        in
          raise Fail ("unknown token: " ^ str)
        end
      else if max = lastSpace then
        (lastSpace, acc)
      else if max = lastWild then
        (lastWild, WILDCARD :: acc)
      else
        let
          val str = String.substring (str, start, max - start + 1)
        in
          if max = lastID then
            (lastID, getWordOrID str :: acc)
          else if max = lastInt then
            case Int.fromString str of
              SOME num => (lastInt, INT num :: acc)
            | NONE => raise Size
          else if max = lastPunct then
            (lastPunct, getPunct str :: acc)
          else
            (* if max = lastType *)
            (lastType, getTypeID str :: acc)
        end
    end

  fun skipFormattingChars (pos, str) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\\" then
          pos
        else if chr = #"\"" then
          (* error: string closed while searching 
           * for end of format chars *)
          raise Size
        else if Char.isSpace chr then
          skipFormattingChars (pos + 1, str)
        else
          (* error: found non-formatting char *)
          raise Size
      end

  fun getEscapeString (pos, str, parsedString) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        case chr of
        (* common escape sequences *)
          #"a" => (pos, #"\a" :: parsedString)
        | #"b" => (pos, #"\b" :: parsedString)
        | #"t" => (pos, #"\t" :: parsedString)
        | #"n" => (pos, #"\n" :: parsedString)
        | #"v" => (pos, #"\v" :: parsedString)
        | #"f" => (pos, #"\f" :: parsedString)
        | #"r" => (pos, #"\r" :: parsedString)
        | #"\\" => (pos, #"\\" :: parsedString)
        | #"\"" => (pos, #"\"" :: parsedString)

        (* few different cases we have to handle:
        * 1. \ddd where there are three decimal digits,
        * 2. \uxxxx where there are four hexadecimal digits
        * 3. \f ___ f\ which contains characters that should be ignored
        *    and edxist only to format the string in source code
        * 4. control characters. *)
        | #"u" =>
            (* hex char *)
            let
              val endPos = pos + 4
              val chr1 = String.sub (str, pos + 1)
              val chr2 = String.sub (str, pos + 2)
              val chr3 = String.sub (str, pos + 3)
              val chr4 = String.sub (str, pos + 4)
            in
              if chr1 <> #"0" orelse chr2 <> #"0" then
                (* invalid hex constants: can only go up to 00FF *)
                raise Size
              else if Char.isHexDigit chr3 andalso Char.isHexDigit chr4 then
                let
                  val chr3 = Char.ord chr3 * 16
                  val chr4 = Char.ord chr4
                  val chr = Char.chr (chr3 + chr4)
                in
                  (endPos, chr :: parsedString)
                end
              else
                (* invalid hex digit *)
                raise Size
            end
        | _ =>
            let
              val chrCode = Char.ord chr
              val ctrlCode = chrCode - 64
            in
              if chrCode >= 64 andalso Char.isCntrl (Char.chr ctrlCode) then
                (* control code *)
                (pos, Char.chr ctrlCode :: parsedString)
              else if Char.isDigit chr then
                (* \ddd *)
                let
                  val str = String.substring (str, pos, 3)
                in
                  case Int.fromString str of
                    SOME x => (pos, Char.chr x :: parsedString)
                  | NONE => raise Size
                end
              else
                (* assuming we have formatting chars next *)
                (skipFormattingChars (pos + 1, str), parsedString)
            end
      end

  fun getString (pos, str, parsedString) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        case chr of
          #"\"" =>
            (* close string and return *)
            (pos, String.implode (List.rev parsedString))
        | #"\\" =>
            let
              (* either we have an escape sequence,
               * or we need to skip formatting strings *)
              val (newPos, parsedString) =
                getEscapeString (pos + 1, str, parsedString)
            in
              getString (newPos + 1, str, parsedString)
            end
        | _ => getString (pos + 1, str, chr :: parsedString)
      end

  fun helpGetTokenEndPos (pos, str, dfa, start, acc) =
    if pos = String.size str then
      getToken (str, start, dfa, acc)
    else
      let
        val chr = String.sub (str, pos)
        val dfa = updateDfa (chr, dfa, pos)
      in
        if areAllDead dfa then
          getToken (str, start, dfa, acc)
        else if chr = #"\"" then
          (* found double quotes so we would like to parse string. *)
          let val (newPos, str) = getString (pos + 1, str, [])
          in (newPos, STRING str :: acc)
          end
        else
          helpGetTokenEndPos (pos + 1, str, dfa, start, acc)
      end

  fun getTokenEndPos (pos, str, acc) =
    helpGetTokenEndPos (pos, str, initialDfa, pos, acc)

  fun helpGetTokens (pos, str, acc) =
    if pos = String.size str then
      List.rev (EOF :: acc)
    else
      let val (newPos, acc) = getTokenEndPos (pos, str, acc)
      in helpGetTokens (newPos + 1, str, acc)
      end

  fun getTokens str = helpGetTokens (0, str, [])
end
