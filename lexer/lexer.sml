structure Lexer =
struct
  open Token

  fun getWordOrID str =
    case str of
      "let" => LET
    | "in" => IN
    | "end" => END
    | "fun" => FUN
    | "val" => VAL
    | "rec" => REC
    | "and" => AND
    | "type" => TYPE
    | "array" => ARRAY
    | "if" => IF
    | "then" => THEN
    | "else" => ELSE
    | "case" => CASE
    | "of" => OF
    | "true" => BOOL true
    | "false" => BOOL false
    | "infix" => INFIX
    | "as" => AS
    | "op" => OP
    | "andalso" => ANDALSO
    | "orelse" => ORELSE
    | "raise" => RAISE
    | "handle" => HANDLE
    | "while" => WHILE
    | "do" => DO
    | "fn" => FN
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

  fun getMin (lst, minSoFar) =
    case lst of
      ~1 :: tl => getMin (tl, minSoFar)
    | hd :: tl => getMin (tl, Int.min (hd, minSoFar))
    | [] => minSoFar

  fun getToken (str, finish, dfa, acc) =
    let
      val {lastID, lastInt, lastPunct, lastWild, lastType, lastSpace, ...} = dfa
      val min = getMin
        ([lastID, lastInt, lastPunct, lastWild, lastType, lastSpace], ~1)
    in
      if min = ~1 then
        let
          val chr = String.sub (str, finish)
          val str = String.implode [chr]
        in
          raise Fail ("unknown token: " ^ str)
        end
      else if min = lastSpace then
        (lastSpace, acc)
      else if min = lastWild then
        (lastWild, WILDCARD :: acc)
      else
        let
          val str = String.substring (str, min, finish - min)
        in
          if min = lastID then
            (lastID, getWordOrID str :: acc)
          else if min = lastInt then
            case Int.fromString str of
              SOME num => (lastInt, INT num :: acc)
            | NONE => raise Size
          else if min = lastPunct then
            (lastPunct, getPunct str :: acc)
          else
            (* if min = lastType *)
            (lastType, getTypeID str :: acc)
        end
    end

  fun scanStep (pos, str, dfa, finish, acc) =
    if pos < 0 then
      getToken (str, finish, dfa, acc)
    else
      let
        val chr = String.sub (str, pos)
        val dfa = AllDfa.update (chr, dfa, pos)
      in
        if AllDfa.areAllDead dfa then
          getToken (str, finish, dfa, acc)
        else if chr = #"\"" then
          (* found double quotes so we would like to parse string. *)
          let val (newPos, str) = LexString.getString (pos + 1, str, [])
          in (newPos, STRING str :: acc)
          end
        else
          scanStep (pos - 1, str, dfa, finish, acc)
      end

  fun scanString (pos, str, acc) =
    if pos = String.size str then
      acc
    else
      let val (newPos, acc) = scanStep (pos, str, AllDfa.initial, pos, acc)
      in scanString (newPos, str, acc)
      end

  fun getTokens str =
    if String.size str = 0 then []
    else scanString (String.size str - 1, str, [EOF])
end
