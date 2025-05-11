structure Exp =
struct
  open ParseType

  structure L = Lexer

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

  and helpParseRecordLoop (tokens, fieldName, acc) =
    case exp tokens of
      OK (tokens, exp) =>
        let
          val acc = (fieldName, exp) :: acc
        in
          case tokens of
            L.COMMA :: tl => parseRecordLoop (tl, acc)
          | L.R_BRACE :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => raise Fail "exp.sml 47"
        end
    | ERR => raise Fail "exp.sml 41"

  and parseRecordLoop (tokens, acc) =
    case tokens of
      L.ID fieldName :: L.ID "=" :: tl =>
        helpParseRecordLoop (tl, fieldName, acc)
    | L.INT fieldNum :: L.ID "=" :: tl =>
        if fieldNum > 0 then
          helpParseRecordLoop (tl, Int.toString fieldNum, acc)
        else
          raise Fail "exp.sml 56: label must be: 1, 2, 3, ..."
    | _ => raise Fail "exp.sml 57"

  and parseRecord tokens =
    case tokens of
      L.L_BRACE :: tl => parseRecordLoop (tl, [])
    | _ => ERR

  and parseTuple (tokens, count, acc) =
    case exp tokens of
      OK (tokens, exp) =>
        let
          val acc = (Int.toString count, exp) :: acc
        in
          case tokens of
            L.COMMA :: tl => parseTuple (tl, count + 1, acc)
          | L.R_PAREN :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => ERR
        end
    | ERR => ERR

  and sequenceExp (tokens, mostRecentExp) =
    case exp tokens of
      OK (tokens, mostRecentExp) =>
        let in
          case tokens of
            L.COLON :: tl => sequenceExp (tl, mostRecentExp)
          | L.R_PAREN :: tl => OK (tl, mostRecentExp)
          | _ => raise Fail "exp.sml 84"
        end
    | ERR => raise Fail "exp.sml 86"

  and parenExpAfterExp (tokens, exp) =
    case tokens of
      L.R_PAREN :: tl => (* paren-exp *) OK (tl, GROUP_EXP exp)
    | L.COMMA :: tl => (* tuple-exp *) parseTuple (tl, 2, [("1", exp)])
    | L.COLON :: tl => (* sequence-exp *) sequenceExp (tl, exp)
    | _ => raise Fail "exp.sml 81"

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
                  OK (tokens, exp) => parenExpAfterExp (tokens, exp)
                | ERR => raise Fail "exp.sml 57"
              end
        end
    | _ => ERR

  and listExpLoop (tokens, acc) =
    case exp tokens of
      OK (tokens, exp) =>
        let
          val acc = exp :: acc
        in
          case tokens of
            L.COMMA :: tl => listExpLoop (tl, acc)
          | L.R_BRACKET :: tl => OK (tl, LIST_EXP (List.rev acc))
          | _ => raise Fail "exp.sml 108"
        end
    | ERR => raise Fail "exp.sml 102"

  and listExp tokens =
    case tokens of
      L.L_BRACKET :: tl => listExpLoop (tl, [])
    | _ => ERR

  and vectorExpLoop (tokens, acc) =
    case exp tokens of
      OK (tokens, exp) =>
        let
          val acc = exp :: acc
        in
          case tokens of
            L.COMMA :: tl => vectorExpLoop (tl, acc)
          | L.R_BRACKET :: tl =>
              let
                val acc = List.rev acc
                val acc = Vector.fromList acc
              in
                OK (tl, VECTOR_EXP acc)
              end
          | _ => raise Fail "exp.sml 132"
        end
    | ERR => raise Fail "exp.sml 134"

  and vectorExp tokens =
    case tokens of
      L.HASH :: L.L_BRACKET :: tl => vectorExpLoop (tl, [])
    | _ => ERR

  and atExp tokens =
    Combo.choice
      ( [ scon
        , valueIdentifier
        , parseRecord
        , recordSelector
        , parenExp
        , listExp
        , vectorExp
        (* todo: let exp *)
        ]
      , tokens
      )

  and loopAppExp (tokens, acc) =
    case atExp tokens of
      OK (tokens, exp) => loopAppExp (tokens, exp :: acc)
    | ERR => OK (tokens, APP_EXP (List.rev acc))

  and appExp tokens =
    case atExp tokens of
      OK (tokens, exp) => loopAppExp (tokens, [exp])
    | ERR => ERR

  and infExp tokens = raise Fail ""

  and raiseExp tokens = raise Fail ""

  and ifExp tokens = raise Fail ""

  and whileExp tokens = raise Fail ""

  and caseExp tokens = raise Fail ""

  and fnExp tokens = raise Fail ""

  and exp tokens : exp result =
    let
      val result = Combo.choice
        ([infExp, raiseExp, ifExp, whileExp, caseExp, fnExp], tokens)
    in
      case result of
        OK (tokens, exp) => afterExp (tokens, exp)
      | ERR => ERR
    end

  and boolExp (tokens, exp) =
    case tokens of
      L.ANDALSO :: tl => Combo.next (exp tl, OK)
    | L.ORELSE :: tl => Combo.next (exp tl, OK)
    | _ => ERR

  and afterExp (tokens, exp) =
    Combo.choiceData ([boolExp], tokens, exp)
end
