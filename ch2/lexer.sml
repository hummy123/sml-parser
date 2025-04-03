signature LEXER =
sig
  datatype token =
    INT of int
  | ID of string
  | STRING of string
  | BOOL of bool

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
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_OR_EQUAL
  | GREATER_THAN
  | GREATER_THAN_OR_EQUAL
  | AMPERSAND
  | PIPE
  | TILDE
  | HASH
  | WILDCARD

  | EOF

  val tokenToString: token -> string

  val getTokens: string -> token list
end

structure Lexer :> LEXER =
struct
  fun areAllDead (idState, intState, punctState, wildcardState) =
    idState = 0 andalso intState = 0 andalso punctState = 0
    andalso wildcardState = 0

  datatype token =
    INT of int
  | ID of string
  | STRING of string
  | BOOL of bool

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
  | EQUALS
  | NOT_EQUALS
  | LESS_THAN
  | LESS_OR_EQUAL
  | GREATER_THAN
  | GREATER_THAN_OR_EQUAL
  | AMPERSAND
  | PIPE
  | TILDE
  | HASH
  | WILDCARD

  | EOF

  fun tokenToString tok =
    case tok of
      INT num => "INT(" ^ Int.toString num ^ ")"
    | ID id => "ID(" ^ id ^ ")"
    | STRING str => "STRING(" ^ str ^ ")"
    | BOOL b => "BOOL(" ^ Bool.toString b ^ ")"

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

    (* punctuation *)
    | COMMA => ","
    | COLON => ":"
    | SEMI_COLON => ";"
    | L_PAREN => "("
    | R_PAREN => ")"
    | L_BRACKET => "["
    | R_BRACKET => "]"
    | L_BRACE => "{"
    | R_BRACE => "}"
    | DOT => "."
    | EQUALS => "="
    | NOT_EQUALS => "<>"
    | LESS_THAN => "<"
    | LESS_OR_EQUAL => "<="
    | GREATER_THAN => ">"
    | GREATER_THAN_OR_EQUAL => ">="
    | AMPERSAND => "&"
    | PIPE => "|"
    | TILDE => "~"
    | HASH => "#"
    | WILDCARD => "_"

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
    | _ => ID str

  fun getPunct str =
    case str of
      "," => COMMA
    | ":" => COLON
    | ";" => SEMI_COLON
    | "(" => L_PAREN
    | ")" => R_PAREN
    | "[" => L_BRACKET
    | "]" => R_BRACKET
    | "{" => L_BRACE
    | "}" => R_BRACE
    | "." => DOT
    | "+" => ID "+"
    | "-" => ID "-"
    | "*" => ID "*"
    | "/" => ID "/"
    | "=" => EQUALS
    | "<>" => NOT_EQUALS
    | "<" => LESS_THAN
    | "<=" => LESS_OR_EQUAL
    | ">" => GREATER_THAN
    | ">=" => GREATER_THAN_OR_EQUAL
    | "&" => AMPERSAND
    | "|" => PIPE
    | "~" => TILDE
    | "#" => HASH
    | _ => (print str; raise Empty)

  fun getMax
    ( str
    , start
    , lastFinalID
    , lastFinalInt
    , lastFinalPunct
    , lastFinalWildcard
    , acc
    ) =
    let
      val max = Int.max (lastFinalID, lastFinalInt)
      val max = Int.max (max, lastFinalPunct)
      val max = Int.max (max, lastFinalWildcard)
    in
      if max = lastFinalWildcard then
        (lastFinalWildcard, WILDCARD :: acc)
      else
        let
          val str = String.substring (str, start, max - start + 1)
        in
          if max = lastFinalID then
            (lastFinalID, getWordOrID str :: acc)
          else if max = lastFinalInt then
            case Int.fromString str of
              SOME num => (lastFinalInt, INT num :: acc)
            | NONE => raise Size
          else
            (* max = lastFinalPunct *)
            (lastFinalPunct, getPunct str :: acc)
        end
    end

  fun getToken
    ( str
    , start
    , lastFinalID
    , lastFinalInt
    , lastFinalPunct
    , lastFinalWildcard
    , acc
    ) =
    if
      lastFinalID = ~1 andalso lastFinalInt = ~1 andalso lastFinalPunct = ~1
      andalso lastFinalWildcard = ~1
    then
      (start, acc)
    else
      getMax
        ( str
        , start
        , lastFinalID
        , lastFinalInt
        , lastFinalPunct
        , lastFinalWildcard
        , acc
        )

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

  fun helpGetTokenEndPos
    ( pos
    , str
    , idState
    , intState
    , punctState
    , wildcardState
    , lastFinalID
    , lastFinalInt
    , lastFinalPunct
    , lastFinalWildcard
    , start
    , acc
    ) =
    if pos = String.size str then
      getToken
        ( str
        , start
        , lastFinalID
        , lastFinalInt
        , lastFinalPunct
        , lastFinalWildcard
        , acc
        )
    else
      let
        val chr = String.sub (str, pos)

        val newIdState = IdDfa.getNewState (chr, idState)
        val newIntState = IntDfa.getNewState (chr, intState)
        val newPunctState = PunctDfa.getNewState (chr, punctState)
        val newWildcardState = WildcardDfa.getNewState (chr, wildcardState)
      in
        if chr = #"\"" then
          (* found double quotes so we would like to parse string. 
           * edge case: we might find that we went over another token 
           * before we saw the double quotes, and we would like to 
           * add that token to the token list.
           * For example, if we parse the string `"hello"+"world"`,
           * despite there being no space between + and world,
           * we would like to add + to the token list in this function call,
           * and let the helpGetTokens' loop call this function to parse the
           * string onh the next iteration.
           *
           * The "pos = start" check helps us do that indirectly. *)
          if pos = start then
            let val (newPos, str) = getString (pos + 1, str, [])
            in (newPos, STRING str :: acc)
            end
          else
            getToken
              ( str
              , start
              , lastFinalID
              , lastFinalInt
              , lastFinalPunct
              , lastFinalWildcard
              , acc
              )
        else if areAllDead (newIdState, newIntState, punctState, wildcardState) then
          getToken
            ( str
            , start
            , lastFinalID
            , lastFinalInt
            , lastFinalPunct
            , lastFinalWildcard
            , acc
            )
        else
          let
            val lastFinalID =
              if IdDfa.isFinal newIdState then pos else lastFinalID

            val lastFinalInt =
              if IntDfa.isFinal newIntState then pos else lastFinalInt

            val lastFinalPunct =
              if PunctDfa.isFinal newPunctState then pos else lastFinalPunct

            val lastFinalWildcard =
              if WildcardDfa.isFinal newWildcardState then pos
              else lastFinalWildcard
          in
            helpGetTokenEndPos
              ( pos + 1
              , str
              , newIdState
              , newIntState
              , newPunctState
              , newWildcardState
              , lastFinalID
              , lastFinalInt
              , lastFinalPunct
              , lastFinalWildcard
              , start
              , acc
              )
          end
      end

  fun getTokenEndPos (pos, str, acc) =
    helpGetTokenEndPos
      ( pos
      , str
      , IdDfa.start
      , IntDfa.start
      , PunctDfa.start
      , WildcardDfa.start
      , ~1
      , ~1
      , ~1
      , ~1
      , pos
      , acc
      )

  fun helpGetTokens (pos, str, acc) =
    if pos = String.size str then
      List.rev (EOF :: acc)
    else
      let val (newPos, acc) = getTokenEndPos (pos, str, acc)
      in helpGetTokens (newPos + 1, str, acc)
      end

  fun getTokens str = helpGetTokens (0, str, [])
end
