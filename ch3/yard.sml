structure Yard =
struct
  datatype literal = INT_LITERAL of int | STRING_LITERAL of string

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
  | IF_THEN_ELSE of exp * exp * exp
  | RECORD_EXP of {fieldName: string, fieldValue: exp} list
  | SELECT_FIELD of string * exp
  | EMPTY

  and dec =
    TYPE_DEC of string * {fieldName: string, fieldValue: string} list
  | VAL_DEC of string * exp
  | FUN_DEC of string * string list * exp

  structure L = Lexer

  local
    (*
      fun toIntLiteral num = LITERAL (INT_LITERAL num)
        | toIntLiteral _ = raise Empty
    
      fun loopRpn (yardList, ast) =
        case yardList of
          L.INT a :: L.INT b :: tl => 
        | opt :: tl =>
            let in
              case newNumStack of
                L.INT b :: L.INT a :: tl =>
    
      fun rpnToParseTree lst =
        case lst of
          [L.INT num] => toIntLiteral num
        | L.INT a :: L.INT b :: 
          L.INT num1 :: tl => 
          let
            val num1 = toIntLiteral num
          in
            *)

    fun whenPlusOrMinusToken (numStack, opStack) =
      case opStack of
        L.ASTERISK :: tl => whenPlusOrMinusToken (L.ASTERISK :: numStack, tl)
      | L.SLASH :: tl => whenPlusOrMinusToken (L.SLASH :: numStack, tl)
      | L.PLUS :: tl => (L.PLUS :: numStack, tl)
      | L.MINUS :: tl => (L.MINUS :: numStack, tl)
      | _ => (numStack, opStack)

    fun popToLParen (numStack, opStack) =
      case opStack of
        L.L_PAREN :: tl => (numStack, tl)
      | hd :: tl => popToLParen (hd :: numStack, tl)
      | [] => (numStack, opStack)

    fun finishYard (numStack, opStack) =
      case opStack of
        hd :: tl => finishYard (hd :: numStack, tl)
      | [] => List.rev numStack

    fun loop (tokens, numStack, opStack) =
      case tokens of
        L.INT num :: tl => loop (tl, L.INT num :: numStack, opStack)
      | L.ASTERISK :: tl =>
          let val opStack = L.ASTERISK :: opStack
          in loop (tl, numStack, opStack)
          end
      | L.SLASH :: tl =>
          let val opStack = L.SLASH :: opStack
          in loop (tl, numStack, opStack)
          end
      | L.PLUS :: tl =>
          let
            val (numStack, opStack) = whenPlusOrMinusToken (numStack, opStack)
            val opStack = L.PLUS :: opStack
          in
            loop (tl, numStack, opStack)
          end
      | L.MINUS :: tl =>
          let
            val (numStack, opStack) = whenPlusOrMinusToken (numStack, opStack)
            val opStack = L.MINUS :: opStack
          in
            loop (tl, numStack, opStack)
          end
      | L.L_PAREN :: tl => loop (tl, numStack, L.L_PAREN :: opStack)
      | L.R_PAREN :: tl =>
          let val (numStack, opStack) = popToLParen (numStack, opStack)
          in loop (tl, numStack, opStack)
          end
      | L.EOF :: _ => (finishYard (numStack, opStack), [])
      | [] => (finishYard (numStack, opStack), [])
      | _ :: _ => (finishYard (numStack, opStack), tokens)
  in
    fun yard tokens = loop (tokens, [], [])
  end
end

(* for repl *)
fun yard str =
  let
    val tokens = Lexer.getTokens str
    val (result, remaining) = Yard.yard tokens
  in
    result
  end
