structure Type =
struct
  open ParseType

  (* grammar for types:
   *
   * tyrow       ::= lab : typ (, tyrow)   record row
   *
   * atomic_type ::= tyvar
   *                                  type variable
   *                 {tyrow}
   *                                  record
   *                 (base_type)
   *                                  parenthesised type
   *                 longid
   *                                  constructor (not generic)
   *                 (base_type1, ...base_typen) longid 
   *                                  constructor (multiple generics)
   *
   * typ         ::= atomic_type
   *                                  atomic type
   *                 typ1 -> typn ...
   *                                  function type
   *                 typ1 * typn ...
   *                                  tuple type
   *
   * base_type   ::= typ
   *                 typ longid
   *                                  constructor of one generic
   * *)

  structure L = Lexer

  fun tyvar tokens =
    case tokens of
      L.TYPE_ID {isEqType, id} :: tl =>
        OK (tl, TY_VAR {isEq = isEqType, id = id})
    | _ => ERR

  fun longid tokens =
    case tokens of
      L.LONG_ID strList :: tl => OK (tl, TY_CON {tyseq = [], con = strList})
    | _ => ERR

  fun lab tokens =
    case tokens of
      L.ID "=" :: _ => raise Fail "type.sml 46: expected identifier but got ="
    | L.ID fieldName :: tl => OK (fieldName, tl)
    | L.INT num :: tl =>
        if num > 0 then OK (Int.toString num, tl)
        else raise Fail "type.sml 53: label must be 1, 2, 3, ..."
    | _ => ERR

  fun tyrow (tokens, acc) =
    Combo.next (lab tokens, fn (tokens, fieldName) =>
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

  and startTyrow tokens =
    case tokens of
      L.L_BRACE :: tl => tyrow (tl, [])
    | _ => ERR

  and multiTy (tokens, acc) =
    Combo.next (baseTy tokens, fn (tokens, newTy) =>
      case tokens of
        L.COMMA :: tl => multiTy (tl, newTy :: acc)
      | L.R_PAREN :: tl => OK (tl, List.rev acc)
      | _ => ERR)

  and parenTy tokens =
    case tokens of
      L.L_PAREN :: tl =>
        Combo.next (baseTy tl, fn (tokens, tyval) =>
          case tokens of
            L.R_PAREN :: tl => OK (tl, tyval)
          | L.COMMA :: tl =>
              (* multi-argument constructor *)
              Combo.next (multiTy (tl, [tyval]), fn (tokens, typeList) =>
                case tokens of
                  ID constructor :: tl =>
                    OK (tl, TY_CON {tyseq = typeList, con = [constructor]})
                | LONG_ID constructor :: tl =>
                    OK (tl, TY_CON {tyseq = typeList, con = constructor})
                | _ => ERR)
          | _ => ERR)
    | _ => ERR

  and atType tokens =
    Combo.choice ([tyvar, startTyrow, parenTy, longid])

  and baseTy tokens = raise Fail ""

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
