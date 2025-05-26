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

  (* ATOMIC TYPE *)
  fun tyvar tokens =
    case tokens of
      L.TYPE_ID {isEqType, id} :: tl =>
        OK (tl, TY_VAR {isEq = isEqType, id = id})
    | _ => ERR

  fun emptyConstructor (tokens, env) =
    case tokens of
      L.ID id :: tl =>
        if ParseEnv.isConstructor (id, env) then
          OK (tl, TY_CON {tyseq = [], con = [id]})
        else
          ERR
    | L.LONG_ID strList :: tl =>
        raise Fail "type.sml 47: don't know how to handle long id yet"
    | _ => ERR

  fun lab tokens =
    case tokens of
      L.ID "=" :: _ => raise Fail "type.sml 46: expected identifier but got ="
    | L.ID fieldName :: tl => OK (tl, fieldName)
    | L.INT num :: tl =>
        if num > 0 then OK (tl, Int.toString num)
        else raise Fail "type.sml 53: label must be 1, 2, 3, ..."
    | _ => ERR

  fun tyrow (tokens, acc, env) =
    Combo.next (lab tokens, fn (tokens, fieldName) =>
      case tokens of
        L.COLON :: tl =>
          Combo.next (baseTy (tl, env), fn (tokens, tyval) =>
            let
              val acc = (fieldName, tyval) :: acc
            in
              case tokens of
                L.COMMA :: tl => tyrow (tl, acc, env)
              | L.R_BRACE :: tl =>
                  let val acc = List.rev acc
                  in OK (tl, RECORD_TYPE acc)
                  end
              | _ => ERR
            end)
      | _ => ERR)

  and startTyrow (tokens, env) =
    case tokens of
      L.L_BRACE :: tl => tyrow (tl, [], env)
    | _ => ERR

  and multiTy (tokens, acc, env) =
    Combo.next (baseTy (tokens, env), fn (tokens, newTy) =>
      let
        val acc = newTy :: acc
      in
        case tokens of
          L.COMMA :: tl => multiTy (tl, acc, env)
        | L.R_PAREN :: tl => OK (tl, List.rev acc)
        | _ => ERR
      end)

  and parenTy (tokens, env) =
    case tokens of
      L.L_PAREN :: tl =>
        Combo.next (baseTy (tl, env), fn (tokens, tyval) =>
          case tokens of
            L.R_PAREN :: tl => OK (tl, tyval)
          | L.COMMA :: tl =>
              (* multi-argument constructor *)
              Combo.next (multiTy (tl, [tyval], env), fn (tokens, typeList) =>
                case tokens of
                  L.ID constructor :: tl =>
                    OK (tl, TY_CON {tyseq = typeList, con = [constructor]})
                | L.LONG_ID constructor :: tl =>
                    OK (tl, TY_CON {tyseq = typeList, con = constructor})
                | _ => ERR)
          | _ => ERR)
    | _ => ERR

  and atType (tokens, env) =
    case tyvar tokens of
      (result as OK _) => result
    | ERR =>
        Combo.choiceData ([startTyrow, emptyConstructor, parenTy], tokens, env)

  (* TYP *)
  and funTy (tokens, inTy, env) =
    case tokens of
      L.DASH_ARROW :: tl =>
        Combo.next (baseTy (tl, env), fn (tokens, nextTy) =>
          let
            (* We want a function type like `a -> b -> c` 
             * to be a list of [a, b, c],
             * but because of the recursion, we have a tree like
             * [a, [b, c]] instead.
             * So we just flatten the result to be a list instead,
             * if the returned type is also a function type. *)
            val result =
              case nextTy of
                FUN_TY list => FUN_TY (inTy :: list)
              | nextTy => FUN_TY [inTy, nextTy]
          in
            OK (tokens, result)
          end)
    | _ => ERR

  and tupleTy (tokens, inTy, env) =
    case tokens of
      L.ID "*" :: L.L_PAREN :: tl =>
        (* If the type definition has a left-paren, 
         * we expect a right-paren after it *)
        Combo.next (baseTy (tl, env), fn (tokens, nextTy) =>
          case tokens of
            L.R_PAREN :: tl => OK (tl, TUPLE_TYPE [inTy, nextTy])
          | _ => ERR)
    | L.ID "*" :: tl =>
        Combo.next (baseTy (tl, env), fn (tokens, nextTy) =>
          let
            val result =
              case nextTy of
                TUPLE_TYPE list => TUPLE_TYPE (inTy :: list)
              | nextTy => TUPLE_TYPE [inTy, nextTy]
          in
            OK (tokens, result)
          end)
    | _ => ERR

  and typ (tokens, env) =
    case atType (tokens, env) of
      (atomic as OK (tokens, newTy)) =>
        let in
          case Combo.choiceData2 ([funTy, tupleTy], tokens, newTy, env) of
            (result as OK _) => result
          | ERR => atomic
        end
    | ERR => ERR

  (* BASE TYPE *)
  and typedConstructor (tokens, newTy, env) =
    case tokens of
      L.ID id :: tl =>
        if ParseEnv.isConstructor (id, env) then
          OK (tl, TY_CON {tyseq = [newTy], con = [id]})
        else
          ERR
    | L.LONG_ID idList :: tl =>
        raise Fail "type.sml 176: don't know how to parse LONG_ID in type yet"
    | _ => ERR

  and loopTypeConstructors (tokens, newTy, env) =
    case typedConstructor (tokens, newTy, env) of
      OK (tokens, newTy) => loopTypeConstructors (tokens, newTy, env)
    | ERR => OK (tokens, newTy)

  and baseTy (tokens, env) =
    case typ (tokens, env) of
      (result as OK (tokens, newTy)) =>
        loopTypeConstructors (tokens, newTy, env)
    | ERR => ERR

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

fun parse str =
  let
    open ParseType
    val tokens = Lexer.lex str
    val env =
      { infixMap = ParseEnv.StringMap.empty
      , constructor = ParseEnv.StringSet.empty
      }
    val env = ParseEnv.addConstructor ("int", env)
    val env = ParseEnv.addConstructor ("char", env)
    val env = ParseEnv.addConstructor ("list", env)
  in
    Type.baseTy (tokens, env)
  end
