type id = string

datatype binop = PLUS | MINUS | TIMES | DIV

datatype stm =
  COMPUND_STM of stm * stm
| ASSIGN_STM of id * exp
| PRINT_STM of exp list

and exp =
  ID_EXP of id
| NUM_EXP of int
| OP_EXP of exp * binop * exp
| ESEQ_EXP of stm * exp

val prog =
  COMPUND_STM
    ( ASSIGN_STM ("a", OP_EXP (NUM_EXP 5, PLUS, NUM_EXP 3))
    , COMPUND_STM
        ( ASSIGN_STM ("b", ESEQ_EXP
            ( PRINT_STM [ID_EXP "a", OP_EXP (ID_EXP "a", MINUS, NUM_EXP 1)]
            , OP_EXP (NUM_EXP 10, TIMES, ID_EXP "a")
            ))
        , PRINT_STM [ID_EXP "b"]
        )
    )

fun maxStm (stm, maxSoFar) =
  case stm of
    COMPUND_STM (s1, s2) =>
      let
        val cnt1 = maxStm (s1, maxSoFar)
        val cnt2 = maxStm (s2, maxSoFar)
      in
        Int.max (cnt1, cnt2)
      end
  | ASSIGN_STM (id, exp) => maxExp (exp, maxSoFar)
  | PRINT_STM exps =>
      let val maxSoFar = Int.max (maxSoFar, List.length exps)
      in maxPrint (exps, maxSoFar)
      end

and maxExp (exp, maxSoFar) =
  case exp of
    ESEQ_EXP (stm, exp) =>
      Int.max (maxStm (stm, maxSoFar), maxExp (exp, maxSoFar))
  | ID_EXP _ => maxSoFar
  | NUM_EXP _ => maxSoFar
  | OP_EXP (e1, _, e2) => Int.max (maxExp (e1, maxSoFar), maxExp (e2, maxSoFar))

and maxPrint (lst, maxSoFar) =
  case lst of
    hd :: tl => maxPrint (tl, Int.max (maxSoFar, maxExp (hd, maxSoFar)))
  | [] => maxSoFar

fun maxArgs prog = maxStm (prog, 0)

fun helpUpdate (newId, newVal, lst, acc, hasUpdated) =
  case lst of
    (oldId, oldVal) :: tl =>
      let
        val (acc, hasUpdated) =
          if newId = oldId then ((newId, newVal) :: acc, true)
          else ((oldId, oldVal) :: acc, hasUpdated)
      in
        helpUpdate (newId, newVal, tl, acc, hasUpdated)
      end
  | [] => if hasUpdated then acc else (newId, newVal) :: acc

fun update (newId, newVal, lst) =
  helpUpdate (newId, newVal, lst, [], false)

fun lookup (id, lst) =
  case lst of
    (oldID, oldVal) :: tl => if oldID = id then oldVal else lookup (id, tl)
  | [] => raise Empty

fun interpOp (v1, binop, v2) =
  case binop of
    PLUS => v1 + v2
  | MINUS => v1 - v2
  | DIV => v1 div v2
  | TIMES => v1 * v2

fun interpStm (stm, table) =
  case stm of
    COMPUND_STM (s1, s2) =>
      let val table = interpStm (s1, table)
      in interpStm (s2, table)
      end
  | ASSIGN_STM (id, exp) =>
      let val (value, table) = interpExp (exp, table)
      in update (id, value, table)
      end
  | PRINT_STM exps => interpPrint (table, exps)

and interpExp (exp, table) =
  case exp of
    NUM_EXP n => (n, table)
  | ID_EXP id => (lookup (id, table), table)
  | OP_EXP (exp1, binop, exp2) =>
      let
        val (val1, table) = interpExp (exp1, table)
        val (val2, table) = interpExp (exp2, table)
      in
        (interpOp (val1, binop, val2), table)
      end
  | ESEQ_EXP (stm, exp) =>
      let val table = interpStm (stm, table)
      in interpExp (exp, table)
      end

and interpPrint (table, lst) =
  case lst of
    hd :: tl =>
      let
        val (value, table) = interpExp (hd, table)
        val () = print (Int.toString value ^ "\n")
      in
        interpPrint (table, tl)
      end
  | [] => table
