structure Exp =
struct
  open ParseType

  structure L = Lexer

  fun scon (tokens, _) =
    case tokens of
      L.INT num :: tl => OK (tl, INT_EXP num)
    | L.BOOL b :: tl => OK (tl, BOOL_EXP b)
    | L.STRING s :: tl => OK (tl, STRING_EXP s)
    | _ => ERR

  fun longvid (tokens, acc) =
    case tokens of
      L.DOT :: L.ID id :: tl => longvid (tl, acc ^ "." ^ id)
    | _ => OK (tokens, EXP_VAL_ID acc)

  fun valueIdentifier (tokens, _) =
    case tokens of
      L.OP :: L.ID id :: tl => longvid (tl, id)
    | L.ID id :: tl => longvid (tl, id)
    | _ => ERR

  fun recordSelector (tokens, _) =
    case tokens of
      L.HASH :: L.ID fieldName :: tl => OK (tl, RECORD_SELECTOR fieldName)
    | _ => ERR

  and helpParseRecordLoop (tokens, fieldName, acc, infixMap) =
    case exp (tokens, infixMap) of
      OK (tokens, exp) =>
        let
          val acc = (fieldName, exp) :: acc
        in
          case tokens of
            L.COMMA :: tl => parseRecordLoop (tl, acc, infixMap)
          | L.R_BRACE :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => raise Fail "exp.sml 47"
        end
    | ERR => raise Fail "exp.sml 41"

  and parseRecordLoop (tokens, acc, infixMap) =
    case tokens of
      L.ID fieldName :: L.ID "=" :: tl =>
        helpParseRecordLoop (tl, fieldName, acc, infixMap)
    | L.INT fieldNum :: L.ID "=" :: tl =>
        if fieldNum > 0 then
          helpParseRecordLoop (tl, Int.toString fieldNum, acc, infixMap)
        else
          raise Fail "exp.sml 56: label must be: 1, 2, 3, ..."
    | _ => raise Fail "exp.sml 57"

  and parseRecord (tokens, infixMap) =
    case tokens of
      L.L_BRACE :: tl => parseRecordLoop (tl, [], infixMap)
    | _ => ERR

  and parseTuple (tokens, count, acc, infixMap) =
    case exp (tokens, infixMap) of
      OK (tokens, exp) =>
        let
          val acc = (Int.toString count, exp) :: acc
        in
          case tokens of
            L.COMMA :: tl => parseTuple (tl, count + 1, acc, infixMap)
          | L.R_PAREN :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => ERR
        end
    | ERR => ERR

  and sequenceExp (tokens, mostRecentExp, infixMap) =
    case exp (tokens, infixMap) of
      OK (tokens, mostRecentExp) =>
        let in
          case tokens of
            L.COLON :: tl => sequenceExp (tl, mostRecentExp, infixMap)
          | L.R_PAREN :: tl => OK (tl, mostRecentExp)
          | _ => raise Fail "exp.sml 84"
        end
    | ERR => raise Fail "exp.sml 86"

  and parenExpAfterExp (tokens, exp, infixMap) =
    case tokens of
      L.R_PAREN :: tl => (* paren-exp *) OK (tl, GROUP_EXP exp)
    | L.COMMA :: tl => (* tuple-exp *)
        parseTuple (tl, 2, [("1", exp)], infixMap)
    | L.COLON :: tl => (* sequence-exp *) sequenceExp (tl, exp, infixMap)
    | _ => raise Fail "exp.sml 81"

  and parenExp (tokens, infixMap) =
    case tokens of
      L.L_PAREN :: tl =>
        let in
          case tl of
            L.R_PAREN :: tl => (* unit exp *) OK (tl, UNIT_EXP)
          | _ =>
              (* possibly tuple-exp or paren-exp *)
              let in
                case exp (tl, infixMap) of
                  OK (tokens, exp) => parenExpAfterExp (tokens, exp, infixMap)
                | ERR => raise Fail "exp.sml 57"
              end
        end
    | _ => ERR

  and listExpLoop (tokens, acc, infixMap) =
    case exp (tokens, infixMap) of
      OK (tokens, exp) =>
        let
          val acc = exp :: acc
        in
          case tokens of
            L.COMMA :: tl => listExpLoop (tl, acc, infixMap)
          | L.R_BRACKET :: tl => OK (tl, LIST_EXP (List.rev acc))
          | _ => raise Fail "exp.sml 108"
        end
    | ERR => raise Fail "exp.sml 102"

  and listExp (tokens, infixMap) =
    case tokens of
      L.L_BRACKET :: tl => listExpLoop (tl, [], infixMap)
    | _ => ERR

  and vectorExpLoop (tokens, acc, infixMap) =
    case exp (tokens, infixMap) of
      OK (tokens, exp) =>
        let
          val acc = exp :: acc
        in
          case tokens of
            L.COMMA :: tl => vectorExpLoop (tl, acc, infixMap)
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

  and vectorExp (tokens, infixMap) =
    case tokens of
      L.HASH :: L.L_BRACKET :: tl => vectorExpLoop (tl, [], infixMap)
    | _ => ERR

  and atExp (tokens, infixMap) =
    Combo.choiceData
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
      , infixMap
      )

  and loopAppExp (tokens, acc, infixMap) =
    case atExp (tokens, infixMap) of
      OK (tokens, exp) => loopAppExp (tokens, exp :: acc, infixMap)
    | ERR => OK (tokens, List.rev acc)

  and appExp (tokens, infixMap) =
    case atExp tokens of
      OK (tokens, exp) => loopAppExp (tokens, [exp], infixMap)
    | ERR => ERR

  (* keep consuming expressions until we hit an infix opreator
   * and turn whole result into an APP_EXP at the end *)
  and precedenceFunApplication (expList, infixMap, astList) =
    case expList of
      EXP_VAL_ID id :: tl =>
        let in
          case StringMap.get (infixMap, id) of
            NONE => precedenceFunApplication (tl, infixMap, hd :: astList)
          | SOME _ => let val acc = List.rev acc in (tl, APP_EXP acc) end
          | _ => precedenceFunApplication (tl, infixMap, hd :: astList)
        end
    | hd :: tl => precedenceFunApplication (tl, infixMap, hd :: astList)
    | [] =>
        let in
          case acc of
            [hd] => ([], hd)
          | _ => ([], APP_EXP (List.rev acc))
        end

  and helpClimb (rhs, expList, optPower, isOptLeft, infixMap) =
    case expList of
      EXP_VAL_ID lookahead :: tl =>
        let in
          case InfixMap.get (lookahead, infixMap) of
            SOME {isLeft = isAheadLeft, power = aheadPower} =>
              if
                aheadPower > optPower
                orelse (not isAheadLeft andalso aheadPower = optPower)
              then
                let
                  val nextPower =
                    if aheadPower > optPower then optPower + 1 else optPower
                  val (expList, rhs) = climb (rhs, tl, infixMap, nextPower)
                in
                  helpClimb (rhs, expList, optPower, isOptLeft, infixMap)
                end
              else
                (expList, rhs)
          | NONE => (expList, rhs)
        end
    | _ => (expList, rhs)

  and climb (lhs, expList, infixMap, minPower) =
    case expList of
      EXP_VAL_ID opt :: tl =>
        let in
          case InfixMap.get (opt, infixMap) of
            SOME {isLeft = isOptLeft, power = optPower} =>
              (* outer while loop *)
              if optPower >= minPower then
                let val (tl, rhs) = precedenceFunApplication (tl, infixMap, [])
                in helpClimb (rhs, tl, optPower, isOptLeft, infixMap)
                end
              else
                (expList, lhs)
          | NONE => (expList, lhs)
        end
    | _ => (expList, lhs)

  and infixExp (tokens, infixMap) =
    case appExp (tokens, infixMap) of
      OK (tokens, expList) =>
        let val finalExp = climb (expList, infixMap)
        in OK (tokens, finalExp)
        end
    | ERR => ERR

  and raiseExp (tokens, infixMap) =
    case tokens of
      L.RAISE :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, raisedExp) =>
          let val result = RAISE_EXP raisedExp
          in OK (tokens, result)
          end)
    | _ => ERR

  and ifExp (tokens, infixMap) =
    case tokens of
      L.IF :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, predicate) =>
          case tokens of
            L.THEN :: tl =>
              Combo.next (exp (tl, infixMap), fn (tokens, consequent) =>
                case tokens of
                  L.ELSE :: tl =>
                    Combo.next (exp (tl, infixMap), fn (tokens, alternate) =>
                      OK (tokens, IF_EXP (predicate, consequent, alternate)))
                | _ => ERR)
          | _ => ERR)
    | _ => ERR

  and whileExp (tokens, infixMap) =
    case tokens of
      L.WHILE :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, predicate) =>
          case tokens of
            L.DO :: tl =>
              Combo.next (exp (tl, infixMap), fn (tokens, action) =>
                OK (tokens, WHILE_EXP (predicate, action)))
          | _ => ERR)
    | _ => ERR

  and mrule (tokens, infixMap) =
    Combo.next (Pat.startPat tokens, fn (tokens, curPat) =>
      case tokens of
        L.EQUAL_ARROW :: tl =>
          Combo.next (exp (tl, infixMap), fn (tokens, curExp) =>
            OK (tokens, (curPat, curExp)))
      | _ => ERR)

  and loopCaseExp (tokens, acc, predicate, infixMap) =
    case tokens of
      L.PIPE :: tl =>
        Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
          loopCaseExp (tokens, matchRow :: acc, predicate, infixMap))
    | _ =>
        let
          val acc = List.rev acc
          val result = CASE_EXP (predicate, acc)
        in
          OK (tokens, result)
        end

  and caseExp (tokens, infixMap) =
    case tokens of
      L.CASE :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, predicate) =>
          case tokens of
            L.OF :: tl =>
              Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
                loopCaseExp (tokens, [matchRow], predicate, infixMap))
          | _ => ERR)
    | _ => ERR

  and loopFnExp (tokens, acc, infixMap) =
    case tokens of
      L.PIPE :: tl =>
        Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
          loopFnExp (tokens, matchRow :: acc, infixMap))
    | _ =>
        let
          val acc = List.rev acc
          val result = FN_EXP acc
        in
          OK (tokens, result)
        end

  and fnExp (tokens, infixMap) =
    case tokens of
      L.FN :: tl =>
        Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
          loopFnExp (tokens, [matchRow], infixMap))
    | _ => ERR

  and exp (tokens, infixMap) =
    let
      val result = Combo.choiceData
        ( [infixExp, raiseExp, ifExp, whileExp, caseExp, fnExp]
        , tokens
        , infixMap
        )
    in
      case result of
        OK (tokens, exp) => afterExp (tokens, exp, infixMap)
      | ERR => ERR
    end

  and boolExp (tokens, exp1, infixMap) =
    case tokens of
      L.ANDALSO :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, exp2) =>
          OK (tokens, ANDALSO_EXP (exp1, exp2)))
    | L.ORELSE :: tl =>
        Combo.next (exp (tl, infixMap), fn (tokens, exp2) =>
          OK (tokens, (ORELSE_EXP (exp1, exp2))))
    | _ => ERR

  and typedExpLoop (tokens, exp) =
    case tokens of
      L.COLON :: tl =>
        Combo.next (Type.ty tl, fn (tokens, typ) =>
          let val exp = TYPED_EXP (exp, typ)
          in typedExpLoop (tokens, exp)
          end)
    | _ => OK (tokens, exp)

  and typedExp (tokens, exp, _) =
    case tokens of
      L.COLON :: tl =>
        Combo.next (Type.ty tl, fn (tokens, typ) =>
          let val exp = TYPED_EXP (exp, typ)
          in typedExpLoop (tokens, exp)
          end)
    | _ => ERR

  and loopHandlExp (tokens, acc, infixMap) =
    case tokens of
      L.PIPE :: tl =>
        Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
          loopHandlExp (tokens, matchRow :: acc, infixMap))
    | _ =>
        let
          val acc = List.rev acc
          val result = HANDLE_EXP acc
        in
          OK (tokens, result)
        end

  and handleExp (tokens, exp1, infixMap) =
    case tokens of
      L.HANDLE :: tl =>
        Combo.next (mrule (tl, infixMap), fn (tokens, matchRow) =>
          loopHandlExp (tokens, [matchRow], infixMap))
    | _ => ERR

  and afterExp (tokens, exp, infixMap) =
    let
      val result =
        Combo.choiceData2
          ([boolExp, typedExp, handleExp], tokens, exp, infixMap)
    in
      case result of
        OK _ => result
      | ERR => OK (tokens, exp)
    end
end
