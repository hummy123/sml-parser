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

  and parseTuple (tokens, count, acc) =
    case exp tokens of
      OK (tokens, exp) =>
        let in
          case tokens of
            L.COMMA :: tl =>
              parseTuple (tl, count + 1, (Int.toString count, exp) :: acc)
          | L.R_PAREN :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => ERR
        end
    | ERR => ERR

  and parenExp tokens =
    case tokens of
      L.L_PAREN :: tl =>
        let in
          case tl of
            L.R_PAREN :: tl => (* unit exp *) OK (tl, UNIT_EXP)
          | _ =>
              (* possibly tuple-exp or paren-exp *)
              let in
                case exp tl of
                  OK (tokens, exp) =>
                    let in
                      case tokens of
                        L.R_PAREN :: tl => (* paren-exp *) OK (tl, exp)
                      | L.COMMA :: tl =>
                          (* tuple-exp *)
                          parseTuple (tl, 2, [("1", exp)])
                      | _ => raise Fail "exp.sml 55"
                    end
                | ERR => raise Fail "exp.sml 57"
              end
        end
    | _ => ERR

  and atExp tokens =
    let
      val result = ERR
      val result = ifErr (scon, tokens, result)
      val result = ifErr (valueIdentifier, tokens, result)

      (* todo: record exp *)
      val result = ifErr (recordSelector, tokens, result)
      val result = ifErr (parenExp, tokens, result)

    (* todo: list exp, sequence exp, let exp *)
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
