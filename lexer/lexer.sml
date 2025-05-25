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
    | _ => ID str

  fun getTypeID str =
    let val isEqType = String.size str >= 2 andalso String.sub (str, 1) = #"'"
    in TYPE_ID {isEqType = isEqType, id = str}
    end

  fun helpSplitLongID (inStr, pos, strList, chrList) =
    if pos < 0 then
      let val outStr = String.implode chrList
      in outStr :: strList
      end
    else
      let
        val chr = String.sub (inStr, pos)
      in
        if chr = #"." then
          let val outStr = String.implode chrList
          in helpSplitLongID (inStr, pos - 1, outStr :: strList, [])
          end
        else
          helpSplitLongID (inStr, pos - 1, strList, chr :: chrList)
      end

  fun splitLastLongID str =
    helpSplitLongID (str, String.size str - 1, [], [])

  fun getMin (lst, minSoFar) =
    case lst of
      ~1 :: tl => getMin (tl, minSoFar)
    | hd :: tl =>
        let val minSoFar = if minSoFar = ~1 then hd else Int.min (hd, minSoFar)
        in getMin (tl, Int.min (hd, minSoFar))
        end
    | [] => minSoFar

  fun getToken (str, finish, dfa, acc) =
    let
      val
        { lastID
        , lastInt
        , lastPunct
        , lastWild
        , lastType
        , lastSpace
        , lastLongID
        , ...
        } = dfa
      val min = getMin
        ( [ lastID
          , lastInt
          , lastPunct
          , lastWild
          , lastType
          , lastSpace
          , lastLongID
          ]
        , ~1
        )
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
          val str = String.substring (str, min, finish - min + 1)
        in
          if min = lastID then
            (lastID, getWordOrID str :: acc)
          else if min = lastLongID then
            let
              val strList = splitLastLongID str
              val token = LONG_ID strList
            in
              (lastLongID, token :: acc)
            end
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
    else if AllDfa.areAllDead dfa then
      getToken (str, finish, dfa, acc)
    else
      let
        val chr = String.sub (str, pos)
        val dfa = AllDfa.update (chr, dfa, pos)
      in
        if chr = #"\"" then
          (* found double quotes so we would like to parse string. *)
          let val (newPos, str) = StringRewrite.rewrite (pos - 1, str)
          in (newPos, STRING str :: acc)
          end
        else
          scanStep (pos - 1, str, dfa, finish, acc)
      end

  fun scanString (pos, str, acc) =
    if pos < 0 then
      acc
    else
      let val (newPos, acc) = scanStep (pos, str, AllDfa.initial, pos, acc)
      in if newPos = 0 then acc else scanString (newPos - 1, str, acc)
      end

  fun lex str =
    if String.size str = 0 then [EOF]
    else scanString (String.size str - 1, str, [EOF])
end

fun main () =
  let
    val str = "Int.toString"
    val tokens = Lexer.lex str
  in
    case tokens of
      Token.LONG_ID strList :: _ =>
        let val str = String.concatWith "." strList
        in print ("str found:\n" ^ String.toString str ^ "\n")
        end
    | _ => print ("str not found\n")
  end

val () = main ()
