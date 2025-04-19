structure Type =
struct
  open ParseType

  structure L = Lexer

  fun ifErr (f, tokens, result) =
    case result of
      ERR => f tokens
    | OK _ => result

  fun firstIfOK (first, sec) =
    case first of
      OK _ => first
    | ERR => sec

  fun tyvar tokens =
    case tokens of
      L.TYPE_ID {isEqType, id} :: tl =>
        OK (tl, TY_VAR {isEq = isEqType, id = id})
    | _ => ERR

  fun tyrow (tokens, acc) =
    case tokens of
      L.ID fieldName :: L.COLON :: tl =>
        let
          val tyval = ty tl
        in
          case tyval of
            OK (tokens, tyval) =>
              let
                val acc = (fieldName, tyval) :: acc
              in
                case tokens of
                  L.COMMA :: tl => tyrow (tl, acc)
                | L.R_BRACE :: tl =>
                    let val acc = List.rev acc
                    in OK (tl, RECORD_TYPE acc)
                    end
                | _ => ERR
              end
          | ERR => ERR
        end
    | L.INT num :: L.COLON :: tl =>
        if num > 0 then
          (* is valid lab *)
          let
            val tyval = ty tl
          in
            case tyval of
              OK (tokens, tyval) =>
                let
                  val acc = (Int.toString num, tyval) :: acc
                in
                  case tokens of
                    L.COMMA :: tl => tyrow (tl, acc)
                  | L.R_BRACE :: tl =>
                      let val acc = List.rev acc
                      in OK (tl, RECORD_TYPE acc)
                      end
                  | _ => ERR
                end
            | ERR => ERR
          end
        else
          raise Fail "34: label must be: 1, 2, 3, ..."
    | _ => ERR

  and startTyrow tokens =
    case tokens of
      L.L_BRACE :: tl => tyrow (tl, [])
    | _ => ERR

  and parenTy tokens =
    case tokens of
      L.L_PAREN :: tl =>
        let
          val tyval = ty tl
        in
          case tyval of
            OK (tokens, tyval) =>
              let in
                case tokens of
                  L.R_PAREN :: tl => OK (tl, tyval)
                | L.COMMA :: tl => tyseqLongtycon (tl, tyval)
                | _ => ERR
              end
          | _ => ERR
        end
    | _ => ERR

  and flattenTupleTypes (new, acc) =
    case new of
      TUPLE_TYPE lst => (List.rev lst) @ acc
    | _ => new :: acc

  and flattenFunTypes (new, acc) =
    case new of
      FUN_TY lst => (List.rev lst) @ acc
    | _ => new :: acc

  (* note about tupleTy and funTy:
   * the function call `case ty tl of ...` will indirectly call funTy/tupleTy
   * so the `newTy` we receive could possibly be another function type or
   * another tuple type.
   *
   * This will produce a nested AST: the tuple 'a * b * c' will produce
   * `TUPLE_TYPE [a, TUPLE_TYPE [b, c]]`
   * instead of
   * `TUPLE_TYPE [a, b, c]`
   * when we want the second.
   *
   * We get around this by 'flattening' the tuple/function type, which produces
   * the second AST.
   *
   * There is one exception: a tuple `a * (b * c)` is meant to be a nested tuple
   * and we don't flatten it in this case.
   * However, the function `a -> (b -> c)` is always the same as `a -> b -> c`
   * so we always flatten the function type.
   * *)
  and funTy (tokens, typ) =
    case tokens of
      L.DASH_ARROW :: tl =>
        let in
          case ty tl of
            OK (tokens, newTy) =>
              let val acc = flattenFunTypes (newTy, [typ])
              in OK (tokens, FUN_TY acc)
              end
          | ERR => raise Fail "160"
        end
    | _ => ERR

  and tupleTy (tokens, typ) =
    case tokens of
      L.ID "*" :: L.L_PAREN :: tl =>
        let in
          case ty tl of
            OK (tokens, newTy) =>
              let in
                case tokens of
                  L.R_PAREN :: tl => OK (tl, TUPLE_TYPE [typ, newTy])
                | _ => raise Fail "type.sml 113: expected ( to be followed by )"
              end
          | ERR => ERR
        end
    | L.ID "*" :: tl =>
        let in
          case ty tl of
            OK (tokens, newTy) =>
              let
                val acc = flattenTupleTypes (newTy, [typ])
                val result = TUPLE_TYPE (List.rev acc)
              in
                OK (tokens, result)
              end
          | ERR => raise Fail "type.sml 138"
        end
    | _ => ERR

  and loopLongTycon (tokens, acc, tyvars) =
    case tokens of
      L.DOT :: L.ID id :: tl =>
        if id = "*" then raise Fail "type.sml 167: * disallowed in tycon"
        else loopLongTycon (tl, acc ^ "." ^ id, tyvars)
    | _ =>
        let val result = TY_CON {tyseq = tyvars, con = acc}
        in OK (tokens, result)
        end

  and startLongTycon (tokens, tyvars) =
    case tokens of
      L.ID id :: tl =>
        if id = "*" then raise Fail "type.sml 181: * disallowed in tycon"
        else loopLongTycon (tl, id, tyvars)
    | _ => ERR

  and loopTyseqLongtycon (tokens, acc) =
    case ty tokens of
      OK (tokens, newTy) =>
        let
          val acc = newTy :: acc
        in
          case tokens of
            L.COMMA :: tl => loopTyseqLongtycon (tl, acc)
          | L.R_PAREN :: tl => startLongTycon (tl, List.rev acc)
          | _ => raise Fail "type.sml 165"
        end
    | ERR => startLongTycon (tokens, List.rev acc)

  and tyseqLongtycon (tokens, typ) =
    case ty tokens of
      OK (tokens, newTy) =>
        let in
          case tokens of
            L.COMMA :: tl => loopTyseqLongtycon (tl, [newTy, typ])
          | L.R_PAREN :: tl => startLongTycon (tl, [typ, newTy])
          | hd :: _ => raise Fail (Token.toString hd)
          | _ => raise Fail "type.sml 177"
        end
    | ERR => ERR

  and ty tokens =
    let
      val result = ERR
      val result = ifErr (tyvar, tokens, result)
      val result = ifErr (startTyrow, tokens, result)
      val result = ifErr (parenTy, tokens, result)
      val result =
        case result of
          OK _ => result
        | ERR => startLongTycon (tokens, [])
    in
      case result of
        OK (tokens, typ) => afterTy (tokens, typ)
      | ERR => ERR
    end

  and afterTy (tokens, typ) =
    case tupleTy (tokens, typ) of
      OK (tokens, typ) => OK (tokens, typ)
    | ERR =>
        let in
          case funTy (tokens, typ) of
            OK (tokens, typ) => OK (tokens, typ)
          | ERR =>
              let in
                case startLongTycon (tokens, [typ]) of
                  OK (tokens, typ) => OK (tokens, typ)
                | ERR => OK (tokens, typ)
              end
        end
end

fun parse str =
  let val tokens = Lexer.getTokens str
  in Type.ty tokens
  end
