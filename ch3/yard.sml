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
    fun pushTimesDivToOpStack opStack =
      case opStack of
        [] => true
      | L.PLUS :: _ => true
      | L.MINUS :: _ => true
      | _ :: _ => false

    fun pushPlusMinusToOpStack opStack =
      case opStack of
        L.ASTERISK :: _ => true
      | L.SLASH :: _ => true
      | [] => true
      | _ :: _ => false

    fun moveOpsToNumStack (numStack, opStack) =
      case opStack of
        L.L_PAREN :: [] => numStack
      | hd :: tl => moveOpsToNumStack (hd :: numStack, tl)
      | [] => numStack

    fun popWithinContext opStack =
      case opStack of
        L.L_PAREN :: _ => opStack
      | hd :: tl => popWithinContext tl
      | [] => opStack

    fun popUntilLParen (numStack, opStack) =
      case opStack of
        Lexer.L_PAREN :: tl => (numStack, tl)
      | hd :: tl => popUntilLParen (hd :: numStack, tl)
      | [] => (numStack, opStack)

    fun loopTimesDiv (token, numStack, opStack, tl, fLoop) =
      if pushTimesDivToOpStack opStack then
        fLoop (tl, numStack, token :: opStack)
      else
        let val numStack = moveOpsToNumStack (numStack, opStack)
        in fLoop (tl, numStack, token :: (popWithinContext opStack))
        end

    and loopPlusMinus (token, numStack, opStack, tl, fLoop) =
      if pushPlusMinusToOpStack opStack then
        fLoop (tl, numStack, token :: opStack)
      else
        let val numStack = moveOpsToNumStack (numStack, opStack)
        in fLoop (tl, numStack, token :: (popWithinContext opStack))
        end

    and loopParens (tokens, numStack, opStack) =
      case tokens of
        L.INT num :: tl => loopParens (tl, L.INT num :: numStack, opStack)
      | L.ASTERISK :: tl =>
          loopTimesDiv (L.ASTERISK, numStack, opStack, tl, loopParens)
      | L.SLASH :: tl =>
          loopTimesDiv (L.SLASH, numStack, opStack, tl, loopParens)
      | L.PLUS :: tl =>
          loopPlusMinus (L.PLUS, numStack, opStack, tl, loopParens)
      | L.MINUS :: tl =>
          loopPlusMinus (L.MINUS, numStack, opStack, tl, loopParens)
      | L.R_PAREN :: tl =>
          let val (numStack, opStack) = popUntilLParen (numStack, opStack)
          in loopParens (tl, numStack, opStack)
          end
      | L.EOF :: _ =>
          let val t = moveOpsToNumStack (numStack, opStack)
          in List.rev t
          end
      | [] => raise Size
      | _ => raise Size

    and loop (tokens, numStack, opStack) =
      case tokens of
        L.INT num :: tl => loop (tl, L.INT num :: numStack, opStack)
      | L.ASTERISK :: tl =>
          loopTimesDiv (L.ASTERISK, numStack, opStack, tl, loop)
      | L.SLASH :: tl => loopTimesDiv (L.SLASH, numStack, opStack, tl, loop)
      | L.PLUS :: tl => loopPlusMinus (L.PLUS, numStack, opStack, tl, loop)
      | L.MINUS :: tl => loopPlusMinus (L.MINUS, numStack, opStack, tl, loop)
      | L.L_PAREN :: tl => loopParens (tl, numStack, L.L_PAREN :: opStack)
      | L.EOF :: _ =>
          let val t = moveOpsToNumStack (numStack, opStack)
          in List.rev t
          end
      | [] => raise Size
      | _ => raise Size
  in
    fun yard tokens = loop (tokens, [], [])
  end
end
