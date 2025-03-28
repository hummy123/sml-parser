structure Yard =
struct
  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | BOOL_LITERAL of bool

  datatype opt =
  (* highest precedence *)
    TIMES
  | DIV

  | PLUS
  | MINUS

  | LEQ
  | GEQ
  | LE
  | GE

  (* lowest precedence *)
  | ANDALSO
  | ORELSE

  datatype unary = NEGATE_INT

  datatype exp =
    LITERAL of literal
  | BINARY of exp * opt * exp
  | UNARY of unary * exp
  | GROUP of exp
  | VAL_ID of string
  | FUNCTION_CALL of string * exp list
  | LET_EXPR of dec list * exp
  | SEQ_EXP of exp list
  | IF_THEN_ELSE of exp * exp * exp
  | RECORD_EXP of {fieldName: string, fieldValue: exp} list
  | SELECT_FIELD of string * exp
  | EMPTY

  and dec =
    TYPE_DEC of string * {fieldName: string, fieldValue: string} list
  | VAL_DEC of string * exp
  | FUN_DEC of string * string list * exp

  structure L = Lexer

  structure StringMap =
    MakeGapMap
      (struct
         type key = string

         fun l (a: string, b: string) = a < b
         fun eq (a: string, b: string) = a = b
         fun g (a: string, b: string) = a > b

         val maxNodeSize = 9
       end)

  fun shouldPush (newName, fnStack, fixMap) =
    case fnStack of
      L.ID hdName :: _ =>
        let in
          case (StringMap.get (hdName, fixMap), StringMap.get (newName, fixMap)) of
            (SOME hd, SOME new) => new > hd
          | _ => raise Fail "65"
        end
    | [] => true
    | _ => raise Fail "67"

  fun reduce (fnStack, valStack) =
    case fnStack of
      L.ID funName :: ftl =>
        (* assume ID is function taking two values for now *)
        let in
          case valStack of
            r :: l :: vtl =>
              let val result = FUNCTION_CALL (funName, [l, r]) :: vtl
              in reduce (ftl, result)
              end
          | _ => raise Fail "584"
        end
    | [L.EOF] => (fnStack, valStack)
    | [] => (fnStack, valStack)
    | _ => raise Fail "586"

  fun yard (tokens, fnStack, valStack, fixMap) =
    case tokens of
      L.INT num :: tl =>
        let val valStack = LITERAL (INT_LITERAL num) :: valStack
        in yard (tl, fnStack, valStack, fixMap)
        end
    | L.STRING s :: tl =>
        let val valStack = LITERAL (STRING_LITERAL s) :: valStack
        in yard (tl, fnStack, valStack, fixMap)
        end
    | L.BOOL b :: tl =>
        let val valStack = LITERAL (BOOL_LITERAL b) :: valStack
        in yard (tl, fnStack, valStack, fixMap)
        end
    | L.ID name :: tl =>
        if shouldPush (name, fnStack, fixMap) then
          let val fnStack = L.ID name :: fnStack
          in yard (tl, fnStack, valStack, fixMap)
          end
        else
          let
            val (fnStack, valStack) = reduce (fnStack, valStack)
            val fnStack = L.ID name :: fnStack
          in
            yard (tl, fnStack, valStack, fixMap)
          end
    | _ =>
        let val (_, valStack) = reduce (fnStack, valStack)
        in (tokens, valStack)
        end

  fun helpParse (tokens, fixMap) =
    case tokens of
      L.INFIX :: L.INT power :: L.ID name :: tl =>
        let val fixMap = StringMap.add (name, power, fixMap)
        in helpParse (tl, fixMap)
        end
    | L.IF :: tl =>
        let
          val (tokens, predicateVals) = yard (tl, [], [], fixMap)
        in
          case tokens of
            L.THEN :: tl =>
              let
                val (tokens, thenVals) = yard (tl, [], [], fixMap)
              in
                case tokens of
                  L.ELSE :: tl =>
                    let
                      val (tokens, elseVals) = yard (tl, [], [], fixMap)
                      val result =
                        IF_THEN_ELSE
                          ( List.hd predicateVals
                          , List.hd thenVals
                          , List.hd elseVals
                          )
                    in
                      (tokens, [result])
                    end
                | _ => raise Fail "146: expected 'else'"
              end
          | _ => raise Fail "124: expected 'then'"
        end
    | _ => yard (tokens, [], [], fixMap)

  fun parse tokens = helpParse (tokens, StringMap.empty)
end

fun yard str =
  let
    val tokens = Lexer.getTokens str
    val (_, r) = Yard.parse tokens
  in
    r
  end

val result = yard
  "\
  \infix 1 + \
  \infix 1 - \
  \infix 3 * \
  \infix 3 / \
  \if true then 1 else 2"
