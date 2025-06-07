structure Pat =
struct
  open ParseType

  (* grammar for patterns:
   *
   * patrow      ::= lab = pat (, patrow)
   *                                  pattern
   *                 id (: base_type) (as pat) (, patrow)
   *                                  variable
   *                 ...
   *                                  wildcard
   *
   * atomic_pat  ::= -
   *                                  wildcard
   *                 scon
   *                                  special constant
   *                 (op) id
   *                                  variable
   *                 (op) longbid (pat)
   *                                  construction
   *                 { patrow }
   *                                  record pattern
   *                 ()
   *                                  unit pattern
   *                 (pat1, ...patn)
   *                                  tuple pattern
   *                 (pat)
   *                                  parenthesised pattern
   *                 [pat1, ...patn]
   *                                  list pattern
   *                #[pat1, ...patn]
   *                                  vector pattern
   *
   * base_pat    ::= atomic_pat
   *                                  atomic pattern
   *                 (op) longvid atpat
   *                                  constructed pattern
   *                 (op) vid (: base_type) as pat
   *                                  layered pattern
   *
   * pat         ::= base_pat
   *                                  base pattern
   *                 pat1 vid pat2
   *                                  infix constructor pattern
   *                 pat : base_type
   *                                  typed pattern
   * *)

  structure L = Lexer

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

  fun wildcard tokens =
    case tokens of
      L.WILDCARD :: tl => OK (tl, WILDCARD_PAT)
    | _ => ERR

  fun makeConstruction (id, tl, env) =
    case pat (tl, env) of
      OK (tl, newPat) => OK (tl, CONSTRUCTED_PAT (id, SOME newPat))
    | ERR => OK (tl, CONSTRUCTED_PAT (id, NONE))

  and construction (tokens, env) =
    case tokens of
      L.OP :: L.ID id :: tl =>
        if ParseEnv.isConstructor (id, env) then makeConstruction (id, tl, env)
        else ERR
    | L.ID id :: tl =>
        if ParseEnv.isInfix (id, env) then
          ERR
        else if ParseEnv.isConstructor (id, env) then
          makeConstruction (id, tl, env)
        else
          ERR
    | L.OP :: L.LONG_ID _ :: tl =>
        raise Fail "pat.sml 94: don't know how to parse LONG_ID yet"
    | L.LONG_ID _ :: tl =>
        raise Fail "pat.sml 96: don't know how to parse LONG_ID yet"
    | _ => ERR

  and variable (tokens, env) =
    case tokens of
      L.OP :: L.ID id :: tl => OK (tl, ID_PAT id)
    | L.ID id :: tl =>
        if ParseEnv.isInfix (id, env) then ERR else OK (tl, ID_PAT id)
    | _ => ERR

  and nextPatrow (tokens, env, acc) =
    case tokens of
      L.R_BRACE :: tl => OK (tl, RECORD_PAT (List.rev acc))
    | L.COMMA :: tl => patrow (tl, env, acc)
    | _ => raise Fail "pat.sml 115: expected , or }"

  and patrowID (tokens, fieldName, env, acc) =
    case pat (tokens, env) of
      OK (tokens, newPat) =>
        let
          val acc =
            { fieldName = fieldName
            , fieldPat = SOME newPat
            , asPat = NONE
            , typ = NONE
            } :: acc
        in
          nextPatrow (tokens, env, acc)
        end
    | ERR => raise Fail "pat.sml 114: expected pattern"

  and patrow (tokens, env, acc) =
    case tokens of
      L.ID fieldName :: L.ID "=" :: tl =>
        if fieldName = "=" then
          raise Fail "expected field name in patrow but got ="
        else
          patrowID (tl, fieldName, env, acc)
    | L.INT num :: L.ID "=" :: tl =>
        if num <= 0 then
          raise Fail "pat.sml 128: field name must be 1, 2, 3, ..."
        else
          patrowID (tl, Int.toString num, env, acc)
    | L.ID fieldName :: tl =>
        let
          val (tl, typ) =
            case Type.baseTy (tl, env) of
              OK (tl, typ) => (tl, SOME typ)
            | ERR => (tl, NONE)
        in
          case tl of
            L.AS :: tl =>
              Combo.next (pat (tl, env), fn (tokens, newPat) =>
                let
                  val acc =
                    { fieldName = fieldName
                    , fieldPat = NONE
                    , asPat = SOME newPat
                    , typ = typ
                    } :: acc
                in
                  nextPatrow (tokens, env, acc)
                end)
          | _ =>
              let
                val acc =
                  { fieldName = fieldName
                  , fieldPat = NONE
                  , asPat = NONE
                  , typ = typ
                  } :: acc
              in
                nextPatrow (tl, env, acc)
              end
        end
    | L.TRIPLE_DOT :: L.R_BRACE :: tl => OK (tl, RECORD_PAT (List.rev acc))
    | L.TRIPLE_DOT :: tl => raise Fail "expected } after ... in patrow"
    | _ => raise Fail "pat.sml 174: expected label in patrow"

  and record (tokens, env) =
    case tokens of
      L.L_BRACE :: L.R_BRACE :: tl => OK (tl, UNIT_PAT)
    | L.L_BRACE :: tl => patrow (tl, env, [])
    | _ => ERR

  and pat (tokens, env) =
    raise Fail "pat.sml: pat not implemented yet"
end
