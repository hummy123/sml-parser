structure Pat =
struct
  structure L = Lexer

  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | BOOL_LITERAL of bool
  | IGNORE_LITERAL

  datatype pat =
    INT_PAT of int
  | STRING_PAT of string
  | BOOL_PAT of bool
  | RECORD_PAT of (string * pat) list
  | ID_PAT of string
  | UNIT_PAT
  | LIST_PAT of pat list
  | VECTOR_PAT of pat vector
  | WILDCARD_PAT
  | CONSTRUCTED_PAT of string * pat
  | TYPE_ANNOTATED of pat * string
  | AS_PAT of pat * pat

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

  datatype result = OK of L.token list * pat | ERR

  fun ifErr (f, tokens, result) =
    case result of
      ERR => f tokens
    | OK _ => result

  fun ifOK (f, result) =
    case result of
      ERR => ERR
    | OK (tokens, _) => f tokens

  fun tryOrDefault (f, exp, tokens) =
    case f (tokens, exp) of
      ERR => (tokens, exp)
    | OK (tokens, exp) => (tokens, exp)

  fun firstIfOK (res1, res2) =
    case res1 of
      OK _ => res1
    | ERR => res2

  fun scon tokens =
    case tokens of
      L.INT num :: tl => OK (tl, INT_PAT num)
    | L.STRING str :: tl => OK (tl, STRING_PAT str)
    | L.BOOL b :: tl => OK (tl, BOOL_PAT b)
    | _ => ERR

  fun wilcard tokens =
    case tokens of
      L.WILDCARD :: tl => OK (tl, WILDCARD_PAT)
    | _ => ERR

  fun loopLongvid (tokens, acc) =
    case tokens of
      L.DOT :: L.ID vid :: tl => loopLongvid (tl, acc ^ vid)
    | _ => (tokens, acc)

  fun longvid tokens =
    case tokens of
      L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, ID_PAT longvid)
        end
    | _ => ERR

  fun longvidOrOpLongvid tokens =
    case tokens of
      L.OP :: L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, ID_PAT longvid)
        end
    | L.ID vid :: tl =>
        let val (tl, longvid) = loopLongvid (tl, vid)
        in OK (tl, ID_PAT longvid)
        end
    | _ => ERR

  fun vid tokens =
    case tokens of
      L.ID vid :: tl => OK (tl, ID_PAT vid)
    | _ => ERR

  and loopRecordPat (tokens, acc) =
    case tokens of
      L.ID fieldName :: L.ID "=" :: tl =>
        let in
          case startPat tl of
            OK (tl, pat) =>
              let
                val acc = (fieldName, pat) :: acc
              in
                case tl of
                  L.COMMA :: tl => loopRecordPat (tl, acc)
                | L.R_BRACE :: tl => OK (tl, RECORD_PAT (List.rev acc))
                | _ =>
                    raise Fail "107: expecting comma or } when parsing record"
              end
          | ERR => ERR
        end
    | L.ID fieldName :: tl =>
        let
          val exp = ID_PAT fieldName
          val (tl, exp) = tryOrDefault (typedPattern, exp, tl)
        in
          case tl of
            L.COMMA :: tl =>
              let val acc = (fieldName, exp) :: acc
              in loopRecordPat (tl, acc)
              end
          | L.R_BRACE :: tl =>
              let
                val acc = (fieldName, exp) :: acc
                val acc = List.rev acc
              in
                OK (tl, RECORD_PAT acc)
              end
          | L.AS :: tl =>
              (* we have an as-pattern *)
              let
                val nextExp = startPat tl
              in
                case nextExp of
                  OK (tokens, nextExp) =>
                    (* we do have an as-pattern *)
                    let
                      val fulExp = AS_PAT (exp, nextExp)
                      val acc = (fieldName, fulExp) :: acc
                    in
                      case tokens of
                        L.COMMA :: tl => loopRecordPat (tl, acc)
                      | L.R_BRACE :: tl => OK (tl, RECORD_PAT (List.rev acc))
                      | _ => ERR
                    end
                | ERR => raise Fail "139: missing as-pattern"
              end
          | _ => raise Fail "119: expecting comma or } when parsing record"
        end
    | L.INT label :: L.ID "=" :: tl =>
        if label > 0 then
          let
            val label = Int.toString label
          in
            case startPat tl of
              OK (tl, pat) =>
                let
                  val acc = (label, pat) :: acc
                in
                  case tl of
                    L.COMMA :: tl => loopRecordPat (tl, acc)
                  | L.R_BRACE :: tl => OK (tl, RECORD_PAT (List.rev acc))
                  | _ =>
                      raise Fail "107: expecting comma or } when parsing record"
                end
            | ERR => ERR
          end
        else
          raise Fail "131: label must be: 1, 2, 3, ..."
    | L.TRIPLE_DOT :: L.R_BRACE :: tl =>
        (* record-specific wildcard *)
        OK (tl, RECORD_PAT (List.rev acc))
    | _ => ERR

  and startRecordPat tokens =
    case tokens of
      L.L_BRACE :: tl => loopRecordPat (tl, [])
    | _ => ERR

  and unitPat tokens =
    case tokens of
      L.L_BRACE :: L.R_BRACE :: tl => OK (tl, UNIT_PAT)
    | L.L_PAREN :: L.R_PAREN :: tl => OK (tl, UNIT_PAT)
    | _ => ERR

  and loopListPat (tokens, acc) =
    case startPat tokens of
      OK (tokens, el) =>
        let
          val acc = el :: acc
        in
          case tokens of
            L.COMMA :: tl => loopListPat (tl, acc)
          | L.R_BRACKET :: tl => OK (tl, LIST_PAT (List.rev acc))
          | _ => ERR
        end
    | ERR => ERR

  and startListPat tokens =
    case tokens of
      L.L_BRACKET :: tl => loopListPat (tl, [])
    | _ => ERR

  and loopTuplePat (tokens, acc, count) =
    case startPat tokens of
      OK (tokens, el) =>
        let
          val strCount = Int.toString count
          val acc = (strCount, el) :: acc
        in
          case tokens of
            L.COMMA :: tl => loopTuplePat (tl, acc, count + 1)
          | L.R_PAREN :: tl =>
              if count = 1 then
                (* parenthesised pattern instead of tuple *)
                OK (tl, el)
              else
                OK (tl, RECORD_PAT (List.rev acc))
          | _ => ERR
        end
    | ERR => ERR

  and startTuplePat tokens =
    case tokens of
      L.L_PAREN :: tl => loopTuplePat (tl, [], 1)
    | _ => ERR

  and helpLayeredPat (tl, vid) =
    let
      val exp = ID_PAT vid
      val (tl, exp) = tryOrDefault (typedPattern, exp, tl)
    in
      case tl of
        L.AS :: tl =>
          let
            val nextExp = startPat tl
          in
            case nextExp of
              OK (tl, nextExp) => OK (tl, AS_PAT (exp, nextExp))
            | _ => ERR
          end
      | _ => ERR
    end

  and layeredPat tokens =
    case tokens of
      L.OP :: L.ID vid :: tl => helpLayeredPat (tl, vid)
    | L.ID vid :: tl => helpLayeredPat (tl, vid)
    | _ => ERR

  and atpat tokens =
    let
      val result = ERR
      val result = ifErr (wilcard, tokens, result)
      val result = ifErr (scon, tokens, result)
      val result = ifErr (longvidOrOpLongvid, tokens, result)
      val result = ifErr (startRecordPat, tokens, result)
      val result = ifErr (unitPat, tokens, result)
      val result = ifErr (startListPat, tokens, result)
      (* tuple pattern function also matches parenthesised pattern like (1) *)
      val result = ifErr (startTuplePat, tokens, result)
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
      | (OK (_, ID_PAT constructor), OK (tokens, atpat)) =>
          OK (tokens, CONSTRUCTED_PAT (constructor, atpat))
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
        (OK (_, ID_PAT vid), OK (tokens, pat2)) =>
          let
            val record = RECORD_PAT [("1", exp), ("2", pat2)]
            val constructedPat = CONSTRUCTED_PAT (vid, record)
          in
            OK (tokens, constructedPat)
          end
      | (ERR, _) => ERR
      | (_, ERR) => ERR
      | _ => raise Fail "142"
    end

  and typedPatLoop (tokens, exp) =
    case typedPattern (tokens, exp) of
      OK (tokens, exp) => typedPatLoop (tokens, exp)
    | ERR => OK (tokens, exp)

  and helpPat tokens =
    let
      val result = ERR
      val result = ifErr (layeredPat, tokens, result)
      val result = ifErr (constructedPattern, tokens, result)
    in
      ifErr (atpat, tokens, result)
    end

  and pat tokens =
    case helpPat tokens of
      OK (tokens, curPat) =>
        firstIfOK
          (infixedValueConstruction (tokens, curPat), OK (tokens, curPat))
    | ERR => ERR

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
