structure Ascent =
struct
  structure L = Lexer

  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | BOOL_LITERAL of bool
  | IGNORE_LITERAL

  datatype exp =
    LITERAL of literal
  | GROUP of exp
  | VAL_ID of string
  | FUNCTION_CALL of string * exp list
  | LET_EXPR of dec list * exp
  | IF_THEN_ELSE of exp * exp * exp
  | TYPE_ANNOTATED of exp * string

  and dec =
    VAL_DEC of string * exp
  | FUN_DEC of string * string list * exp

  datatype result = OK of L.token list * exp | ERR

  fun ifErr (f, tokens, result) =
    case result of
      ERR => f tokens
    | OK _ => result

  fun ifOK (f, result) =
    case result of
      ERR => ERR
    | OK (tokens, _) => f tokens

  fun scon tokens =
    case tokens of
      L.INT num :: tl => OK (tl, LITERAL (INT_LITERAL num))
    | L.STRING str :: tl => OK (tl, LITERAL (STRING_LITERAL str))
    | L.BOOL b :: tl => OK (tl, LITERAL (BOOL_LITERAL b))
    | _ => ERR

  fun wilcard tokens =
    case tokens of
      L.WILDCARD :: tl => OK (tl, LITERAL IGNORE_LITERAL)
    | _ => ERR

  fun loopLongvid (tokens, acc) =
    case tokens of
      L.DOT :: L.ID vid :: tl => loopLongvid (tl, acc ^ vid)
    | _ => (tokens, acc)

  fun longvid tokens =
    case tokens of
      L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, VAL_ID longvid)
        end
    | _ => ERR

  fun longvidOrOpLongvid tokens =
    case tokens of
      L.ID "op" :: L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, VAL_ID longvid)
        end
    | L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, VAL_ID longvid)
        end
    | _ => ERR

  fun vid tokens =
    case tokens of
      L.ID vid :: tl => OK (tl, VAL_ID vid)
    | _ => ERR

  fun parenPat tokens =
    case tokens of
      L.L_PAREN :: tl =>
        let in
          case startPat tl of
            OK (tokens, pat) =>
              let in
                case tokens of
                  L.R_PAREN :: tl => OK (tl, pat)
                | _ => ERR
              end
          | _ => ERR
        end
    | _ => ERR

  and atpat tokens =
    let
      val result = ERR
      val result = ifErr (wilcard, tokens, result)
      val result = ifErr (scon, tokens, result)
      val result = ifErr (longvidOrOpLongvid, tokens, result)
      val result = ifErr (parenPat, tokens, result)
    (* todo: record *)
    in
      result
    end

  and constructedPattern tokens =
    let
      val result = ERR
      val longvidResult = ifErr (longvidOrOpLongvid, tokens, result)
      val atpatResult = ifOK (atpat, longvidResult)
    in
      case (longvidResult, atpatResult) of
        (ERR, _) => ERR
      | (_, ERR) => ERR
      | (OK (_, VAL_ID constructor), OK (tokens, atpat)) =>
          OK (tokens, FUNCTION_CALL (constructor, [atpat]))
      | _ => raise Fail "90"
    end

  and typedPattern (tokens, exp) =
    case tokens of
      L.COLON :: L.ID vid :: tl => OK (tl, TYPE_ANNOTATED (exp, vid))
    | _ => ERR

  and infixedValueConstruction (tokens, exp) =
    let
      val vidExp = vid tokens
      val pat2 = ifOK (pat, vidExp)
    in
      case (vidExp, pat2) of
        (OK (_, VAL_ID vid), OK (tokens, pat2)) =>
          OK (tokens, FUNCTION_CALL (vid, [exp, pat2]))
      | (ERR, _) => ERR
      | (_, ERR) => ERR
      | _ => raise Fail "142"
    end

  and typedPatLoop (tokens, exp) =
    case typedPattern (tokens, exp) of
      OK (tokens, exp) => typedPatLoop (tokens, exp)
    | ERR => OK (tokens, exp)

  and infixLoop (tokens, exp) =
    case infixedValueConstruction (tokens, exp) of
      OK (tokens, exp) => infixLoop (tokens, exp)
    | ERR => OK (tokens, exp)

  and pat tokens =
    let
      val result = ERR
      (* constructed pattern *)
      val result = ifErr (constructedPattern, tokens, result)

      (* atomic *)
      val result = ifErr (atpat, tokens, result)

    (* todo: layered. *)
    in
      case result of
        OK (tokens, exp) => infixLoop (tokens, exp)
      | ERR => ERR
    end

  (* 'pat' function is a loop, 
   * and because recursive descent zooms into the * smallest unit, 
   * type annotation at end will bind to the rightmost/smallest unit 
   * if this is made tail-recursive.
   * However, we want the type annotation to bind to the whole pattern.
   * So, we use the stack which lets us remember when this pattern started
   * and will let us place the type annotation for the whole pattern 
   * because of that. *)
  and startPat tokens =
    let
      val result = pat tokens
    in
      case result of
        OK (tokens, exp) => typedPatLoop (tokens, exp)
      | ERR => ERR
    end
end

fun parse str =
  let
    val tokens = Lexer.getTokens str
  in
    case Ascent.startPat tokens of
      Ascent.OK (_, result) => result
    | _ => raise Fail "145"
  end
