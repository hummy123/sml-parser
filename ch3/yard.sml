structure Yard =
struct
  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | BOOL_LITERAL of bool

  datatype exp =
    LITERAL of literal
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
    if newName = "andalso" orelse newName = "orelse" then
      false
    else
      case fnStack of
        L.ID hdName :: _ =>
          let in
            case
              (StringMap.get (hdName, fixMap), StringMap.get (newName, fixMap))
            of
              (SOME hd, SOME new) => new > hd
            | _ => raise Fail "65"
          end
      | [] => true
      | _ => raise Fail "67"

  fun helpReduce (fnStack, valStack) =
    case fnStack of
      L.ID funName :: ftl =>
        (* assume ID is function taking two values for now *)
        let in
          case valStack of
            r :: l :: vtl =>
              let val result = FUNCTION_CALL (funName, [l, r]) :: vtl
              in helpReduce (ftl, result)
              end
          | [hd] =>
              let val result = FUNCTION_CALL (funName, [hd]) :: valStack
              in (ftl, result)
              end
          | [] => raise Fail "85"
        end
    | [L.EOF] => (fnStack, valStack)
    | [] => (fnStack, valStack)
    | _ => raise Fail "586"

  fun reduce (fnStack, valStack) =
    case (fnStack, valStack) of
      ([L.ID hd], []) => ([], [VAL_ID hd])
    | _ => helpReduce (fnStack, valStack)

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

  fun parseIf (tokens, fixMap) =
    case tokens of
      L.IF :: tl =>
        let
          val () = print "IF\n"
          val (tokens, predicateVals) = yard (tl, [], [], fixMap)
        in
          case tokens of
            L.THEN :: tl =>
              let
                val () = print "THEN\n"
                val (tokens, thenVals) = yard (tl, [], [], fixMap)
              in
                case tokens of
                  L.ELSE :: tl =>
                    let
                      val () = print "ELSE\n"
                      val (tokens, elseVals) = yard (tl, [], [], fixMap)
                      val result =
                        IF_THEN_ELSE
                          ( List.hd predicateVals
                          , List.hd thenVals
                          , List.hd elseVals
                          )
                      val () = print "ELSE DONE\n"
                    in
                      (tokens, result)
                    end
                | _ => raise Fail "146: expected 'else'"
              end
          | hd :: _ =>
              raise Fail ("124: expected 'then' but got " ^ L.tokenToString hd)
        end
    | _ =>
        let
          val (tokens, exp) = yard (tokens, [], [], fixMap)
        in
          case exp of
            hd :: _ => (tokens, hd)
          | _ => parseLet (tokens, fixMap)
        end

  and parseLetDecs (tokens, fixMap, acc) =
    case tokens of
      L.VAL :: L.ID valName :: L.EQUALS :: tl =>
        let
          val (tokens, exp) = parseIf (tl, fixMap)
          val acc = VAL_DEC (valName, exp) :: acc
        in
          parseLetDecs (tokens, fixMap, acc)
        end
    | L.EOF :: tl => (tokens, List.rev acc)
    | _ => (tokens, List.rev acc)

  and parseLet (tokens, fixMap) =
    case tokens of
      L.LET :: tl =>
        let
          val (tokens, decs) = parseLetDecs (tl, fixMap, [])
        in
          case tokens of
            L.IN :: tl =>
              let
                val (tokens, exp) = parseIf (tl, fixMap)
              in
                case tokens of
                  L.END :: tl =>
                    let val result = LET_EXPR (decs, exp)
                    in (tl, result)
                    end
                | _ => raise Fail "166"
              end
          | hd :: _ => raise Fail ("168 " ^ L.tokenToString hd)
        end
    | _ => parseInfix (tokens, fixMap)

  and parseInfix (tokens, fixMap) =
    case tokens of
      L.INFIX :: L.INT power :: L.ID name :: tl =>
        let val fixMap = StringMap.add (name, power, fixMap)
        in parseInfix (tl, fixMap)
        end
    | _ => parseIf (tokens, fixMap)

  fun helpParse (tokens, fixMap) = parseIf (tokens, fixMap)

  fun parse tokens = helpParse (tokens, StringMap.empty)
end

fun yard str =
  let
    val tokens = Lexer.getTokens str
    val (_, r) = Yard.parse tokens
  in
    r
  end

val result = yard "infix 3 + infix 1 andalso infix 1 orelse 1 + 1 andalso 2 + 2 orelse 3 + 3"
