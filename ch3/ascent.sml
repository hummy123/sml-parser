structure Ascent =
struct
  structure L = Lexer

  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | BOOL_LITERAL of bool

  datatype exp =
    LITERAL of literal
  | GROUP of exp
  | VAL_ID of string
  | FUNCTION_CALL of string * exp list
  | LET_EXPR of dec list * exp
  | IF_THEN_ELSE of exp * exp * exp

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

  fun atpat tokens =
    let
      val result = ERR
      val result = ifErr (scon, tokens, result)
      val result = ifErr (longvidOrOpLongvid, tokens, result)
    (* todo: record, parenthesis-pattern, wildcard *)
    in
      result
    end

  fun constructedPattern tokens =
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

  fun pat tokens =
    let
      val result = ERR
      (* constructed pattern *)
      val result = ifErr (constructedPattern, tokens, result)

      (* atomic *)
      val result = ifErr (atpat, tokens, result)

      (* todo: infixed value construction, typed pattern, layered. *)
    in
      result
    end
end

fun parse str =
  let val tokens = Lexer.getTokens str
  in Ascent.pat tokens
  end
