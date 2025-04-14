structure Type =
struct
  structure L = Lexer

  datatype type_grm =
    TY_VAR of {isEq: bool, id: string}
  | RECORD_TYPE of (string * type_grm) list
  | TY_CON of {tyseq: type_grm list, con: string}

  datatype result = OK of L.token list * type_grm | ERR

  fun ifErr (f, tokens, result) =
    case result of
      ERR => f tokens
    | OK _ => result

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

  and startTyrow tokens = tyrow (tokens, [])

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
                | _ => ERR
              end
          | _ => ERR
        end
    | _ => ERR

  and ty tokens =
    let
      val result = ERR
      val result = ifErr (tyvar, tokens, result)
      val result = ifErr (startTyrow, tokens, result)
      val result = ifErr (parenTy, tokens, result)

    (* todo:
     * - type construction
     * - tuple type
     * - function type expression
     * *)
    in
      result
    end
end
