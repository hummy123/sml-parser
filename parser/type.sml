structure Type =
struct
  open ParseType

  structure L = Lexer

  fun tyvar tokens =
    case tokens of
      L.TYPE_ID {isEqType, id} :: tl =>
        OK (tl, TY_VAR {isEq = isEqType, id = id})
    | _ => ERR

  fun tyrow (tokens, acc) =
    case tokens of
      L.ID fieldName :: L.COLON :: tl =>
        Combo.next (ty tl, fn (tokens, tyval) =>
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
          end)
    | L.INT num :: L.COLON :: tl =>
        if num > 0 then
          Combo.next (ty tl, fn (tokens, tyval) =>
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
            end)
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
        Combo.next (ty tl, fn (tokens, tyval) =>
          case tokens of
            L.R_PAREN :: tl => OK (tl, tyval)
          | L.COMMA :: tl => tyseqLongtycon (tl, tyval)
          | _ => ERR)
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
        Combo.next (ty tl, fn (tokens, newTy) =>
          let val acc = flattenFunTypes (newTy, [typ])
          in OK (tokens, FUN_TY acc)
          end)
    | _ => ERR

  and tupleTy (tokens, typ) =
    case tokens of
      L.ID "*" :: L.L_PAREN :: tl =>
        Combo.next (ty tl, fn (tokens, newTy) =>
          case tokens of
            L.R_PAREN :: tl => OK (tl, TUPLE_TYPE [typ, newTy])
          | _ => raise Fail "type.sml 113: expected ( to be followed by )")
    | L.ID "*" :: tl =>
        Combo.next (ty tl, fn (tokens, newTy) =>
          let
            val acc = flattenTupleTypes (newTy, [typ])
            val result = TUPLE_TYPE (List.rev acc)
          in
            OK (tokens, result)
          end)
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
    Combo.next (ty tokens, fn (tokens, newTy) =>
      case tokens of
        L.COMMA :: tl => loopTyseqLongtycon (tl, [newTy, typ])
      | L.R_PAREN :: tl => startLongTycon (tl, [typ, newTy])
      | hd :: _ => raise Fail (Token.toString hd)
      | _ => raise Fail "type.sml 177")

  and ty tokens =
    let
      val result =
        case Combo.choice ([tyvar, startTyrow, parenTy], tokens) of
          (result as OK _) => result
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

  fun multiTyVarSeq (tokens, acc) =
    case tokens of
      L.COMMA :: tl =>
        Combo.next (tyvar tl, fn (tokens, newTy) =>
          multiTyVarSeq (tokens, newTy :: acc))
    | L.R_PAREN :: tl => OK (tl, List.rev acc)
    | _ => ERR

  fun tyVarSeq tokens : type_grm list result =
    case tokens of
      L.L_PAREN :: tl =>
        Combo.next (tyvar tl, fn (tokens, newTy) =>
          multiTyVarSeq (tokens, [newTy]))
    | _ =>
        (case tyvar tokens of
           OK (tokens, newTy) => OK (tokens, [newTy])
         | ERR => ERR)
end
