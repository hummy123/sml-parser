structure Exp =
struct
  open ParseType

  structure L = Lexer

  fun ifErr (f, tokens, result) =
    case result of
      ERR => f tokens
    | OK _ => result

  fun scon tokens =
    case tokens of
      L.INT num :: tl => OK (tl, INT_EXP num)
    | L.BOOL b :: tl => OK (tl, BOOL_EXP b)
    | L.STRING s :: tl => OK (tl, STRING_EXP s)
    | _ => ERR

  fun longvid (tokens, acc) =
    case tokens of
      L.DOT :: L.ID id :: tl => longvid (tl, acc ^ "." ^ id)
    | _ => OK (tokens, EXP_VAL_ID acc)

  fun valueIdentifier tokens =
    case tokens of
      L.OP :: L.ID id :: tl => longvid (tl, id)
    | L.ID id :: tl => longvid (tl, id)
    | _ => ERR

  fun recordSelector tokens =
    case tokens of
      L.HASH :: L.ID fieldName :: tl => OK (tl, RECORD_SELECTOR fieldName)
    | _ => ERR

  fun unitExp tokens =
    case tokens of
      L.L_PAREN :: L.R_PAREN :: tl => OK (tl, UNIT_EXP)
    | _ => ERR

  fun atExp tokens =
    let
      val result = ERR
      val result = ifErr (scon, tokens, result)
      val result = ifErr (valueIdentifier, tokens, result)

      (* todo: record exp *)
      val result = ifErr (recordSelector, tokens, result)
      val result = ifErr (unitExp, tokens, result)

    (* todo: tuple exp, list exp, sequence exp, let exp, paren exp *)
    in
      result
    end

  and exprow tokens = raise Fail "unimplmented"

  and appExp tokens = raise Fail ""

  and infexp tokens = raise Fail ""

  and raiseExp tokens = raise Fail ""

  and ifExp tokens = raise Fail ""

  and whileExp tokens = raise Fail ""

  and caseExp tokens = raise Fail ""

  and fnExp tokens = raise Fail ""

  and exp tokens =
    let
      val result = ERR
      val result = ifErr (infexp, tokens, result)
      val result = ifErr (raiseExp, tokens, result)
      val result = ifErr (ifExp, tokens, result)
      val result = ifErr (whileExp, tokens, result)
      val result = ifErr (caseExp, tokens, result)
      val result = ifErr (fnExp, tokens, result)
    in
      case result of
        OK (tokens, exp) => afterExp (tokens, exp)
      | ERR => ERR
    end

  and afterExp (tokens, exp) = raise Fail ""
end
