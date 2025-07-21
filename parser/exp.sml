structure Exp =
struct
  open ParseType

  (* grammar for expressions:
   *
   * exprow      ::= lab = exp (, exprow)
   *
   * atomic_exp  ::= scon
   *                                  special constant
   *                 (op) longvid
   *                                  value identifier
   *                 {exprow}
   *                                  record
   *                 # lab
   *                                  record selector
   *                 ()
   *                                  unit value
   *                 (exp)
   *                                  parenthesised expression
   *                 (exp1, ...expn)
   *                                  tuple
   *                 (exp1; ...expn)
   *                                  sequence expression
   *                 [exp1, ...expn]
   *                                  list expression
   *                #[exp1, ...expn]
   *                                  vector expression
   *                 let dec in exp1; ...expn end
   *                                  let expression
   *
   * app_exp     ::= attomic_exp
   *                                  atomic expression
   *                 app_exp atomic_exp
   *                                  multiple atomic expressions
   *
   * infix_exp   ::= app_exp
   *                                  application expression
   *                 infix_exp1 vid infix_exp2
   *                                  infix expression
   *
   * mrule       ::= pat => exp
   *
   * match       ::= mrule (| match)
   *
   * base_exp    ::= infix_exp
   *                                  infix expression or application exp
   *                 raise exp
   *                                  raise expression
   *                 if exp1 then exp2 else exp3
   *                                  if expression
   *                 while exp1 do exp2
   *                                  iteration
   *                 case exp of match
   *                                  case expression
   *                 fn match
   *                                  function
   *
   * loop_exp    ::= exp : ty (loop_exp)
   *                                  typed expression
   *                 exp1 andalso exp2 (loop_exp)
   *                                  conjunction
   *                 exp1 orelse exp2 (loop_exp)
   *                                  disjunction
   *                 exp handle match (loop_exp)
   *                                  handle expression
   *                 loop_exp exp
   *                                  loop expression
   *
   * exp         ::= base_exp
   *                                  base expression
   *                 loop_exp
   *                                  loop expression
   * *)

  structure L = Lexer

  (* EXPRESSIONS *)

  fun scon (tokens, _) =
    case tokens of
      L.INT num :: tl => OK (tl, INT_EXP num)
    | L.BOOL b :: tl => OK (tl, BOOL_EXP b)
    | L.STRING s :: tl => OK (tl, STRING_EXP s)
    | _ => ERR

  and nextExprow (tokens, env, acc, fieldName) =
    case exp (tokens, env) of
      OK (tl, newExp) =>
        let
          val acc = (fieldName, newExp) :: acc
        in
          case tl of
            L.COMMA :: tl => loopExprow (tl, env, acc)
          | L.R_BRACE :: tl => OK (tl, RECORD_EXP (List.rev acc))
          | _ => raise Fail "exp.sml 19: expected , or }"
        end
    | ERR => ERR

  and loopExprow (tokens, env, acc) =
    case tokens of
      L.ID fieldName :: L.ID "=" :: tl =>
        if fieldName = "=" then raise Fail "exp.sml 9: expected label but got ="
        else nextExprow (tl, env, acc, fieldName)
    | L.ID fieldName :: tl =>
        if fieldName = "=" then
          raise Fail "exp.sml 24: expected label but got ="
        else
          raise Fail "exp.sml 26: expected = after label in loopExprow"
    | L.INT num :: L.ID "=" :: tl =>
        if num < 1 then
          raise Fail "exp.sml 32: integer label must be greater than 1"
        else
          nextExprow (tl, env, acc, Int.toString num)
    | L.INT num :: tl =>
        if num < 1 then
          raise Fail "exp.sml 35: integer label must be greater than 1"
        else
          raise Fail "exp.sml 37: expected = after label"
    | _ => ERR

  and exprow (tokens, env) = loopExprow (tokens, env, [])

  and tupleExp (tokens, env, acc, counter) =
    case exp (tokens, env) of
      OK (L.COMMA :: tl, newExp) =>
        let val acc = (Int.toString counter, newExp) :: acc
        in tupleExp (tl, env, acc, counter + 1)
        end
    | OK (L.R_BRACE :: tl, newExp) =>
        let
          val acc = (Int.toString counter, newExp) :: acc
          val acc = RECORD_EXP (List.rev acc)
        in
          OK (tl, acc)
        end
    | ERR => ERR

  and sequenceExp (tokens, env, acc) =
    case exp (tokens, env) of
      OK (L.SEMI_COLON :: tl, newExp) => sequenceExp (tl, env, newExp :: acc)
    | OK (L.R_PAREN :: tl, newExp) =>
        let
          val acc = newExp :: acc
          val acc = SEQ_EXP (List.rev acc)
        in
          OK (tl, acc)
        end
    | OK _ =>
        raise Fail
          "exp.sml 149: expected ; or ) after expression in sequenceExp"
    | ERR => raise Fail "exp.sml 150: expected expression"

  and parenExp (token, env) =
    case tokens of
      L.L_PAREN :: L.R_PAREN :: tl => OK (tl, UNIT_EXP)
    | L.L_PAREN :: tl =>
        (case exp (tl, env) of
           OK (L.R_PAREN :: tl, newExp) => OK (tl, GROUP_EXP newExp)
         | OK (L.COMMA :: tl, newExp) => tupleExp (tl, env, acc, 2)
         | OK (L.SEMI_COLON :: tl, newExp) => sequenceExp (tl, env, [newExp])
         | OK _ => raise Fail "exp.sml 147: expected , or } after expression"
         | ERR => raise Fail "exp.sml 148: expected expression after (")
    | ERR => ERR

  and exp (tokens, env) =
    raise Fail "exp.sml 124: exp not implemented"
end

fun main () =
  let
    val expr = "val ('a, 'b) a : int = 3"
    val tokens = Lexer.getTokens expr
  in
    case Exp.startValBind (tokens, StringMap.empty) of
      ParseType.OK (tokens, _) =>
        print ("ok: " ^ Int.toString (List.length tokens) ^ "\n")
    | _ => print "err\n"
  end

val () = main ()
