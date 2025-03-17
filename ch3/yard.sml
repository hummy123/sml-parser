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
    fun getStringLiteral s =
      LITERAL (STRING_LITERAL s)
    fun getIntLiteral i =
      LITERAL (INT_LITERAL i)

    fun optPower (TIMES) = 5
      | optPower (DIV) = 5
      | optPower (MINUS) = 3
      | optPower (PLUS) = 3
      | optPower (_) = raise Empty

    fun reduce (opt, fnStack, valStack) =
      let
        val newPower = optPower opt
      in
        case fnStack of
          L.PLUS :: fntl =>
            let
              val topPower = optPower PLUS
            in
              if topPower > newPower then
                case valStack of
                  b :: a :: valtl =>
                    let val result = BINARY (a, PLUS, b)
                    in reduce (opt, fntl, result :: valtl)
                    end
                | _ => (print "reduce valStack case\n"; raise Size)
              else
                (fnStack, valStack)
            end
        | L.MINUS :: fntl =>
            let
              val topPower = optPower MINUS
            in
              if topPower > newPower then
                case valStack of
                  b :: a :: valtl =>
                    let val result = BINARY (a, MINUS, b)
                    in reduce (opt, fntl, result :: valtl)
                    end
                | _ => (print "reduce valStack case\n"; raise Size)
              else
                (fnStack, valStack)
            end
        | L.ASTERISK :: fntl =>
            let
              val topPower = optPower TIMES
            in
              if topPower > newPower then
                case valStack of
                  b :: a :: valtl =>
                    let val result = BINARY (a, TIMES, b)
                    in reduce (opt, fntl, result :: valtl)
                    end
                | _ => (print "reduce valStack case\n"; raise Size)
              else
                (fnStack, valStack)
            end
        | L.SLASH :: fntl =>
            let
              val topPower = optPower DIV
            in
              if topPower > newPower then
                case valStack of
                  b :: a :: valtl =>
                    let val result = BINARY (a, DIV, b)
                    in reduce (opt, fntl, result :: valtl)
                    end
                | _ => (print "reduce valStack case\n"; raise Size)
              else
                (fnStack, valStack)
            end
        | L.TILDE :: fntl =>
            let in
              case valStack of
                hd :: valtl =>
                  let val result = UNARY (NEGATE_INT, hd)
                  in reduce (opt, fntl, result :: valtl)
                  end
              | _ => (print "reduce valStack case\n"; raise Size)
            end
        | L.L_PAREN :: fntl => (fnStack, valStack)
        | hd :: tl =>
            ( print ("reduce fnStack case [" ^ L.tokenToString hd ^ "]\n")
            ; raise Size
            )
        | [] => (fnStack, valStack)
      end

    fun reduceParens (fnStack, valStack) =
      case fnStack of
        L.PLUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, PLUS, b)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens plus"
          end
      | L.L_PAREN :: fntl =>
          let in
            case valStack of
              hd :: valtl =>
                let val result = GROUP hd :: valtl
                in (fntl, result)
                end
          end
      | [L.EOF] => (fnStack, valStack)
      | [] => (fnStack, valStack)
      | _ => raise Fail "reduceParens wildcard"

    fun reduceUntilEmpty (fnStack, valStack) =
      case fnStack of
        L.PLUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, PLUS, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => (print "unexpected case in yard.sml 91\n"; raise Size)
          end
      | L.MINUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, MINUS, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => (print "unexpected case in yard.sml 99\n"; raise Size)
          end
      | L.ASTERISK :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, TIMES, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => (print "unexpected case in yard.sml 107\n"; raise Size)
          end
      | L.SLASH :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, DIV, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => (print "unexpected case in yard.sml 115\n"; raise Size)
          end
      | L.TILDE :: fntl =>
          let in
            case valStack of
              hd :: valtl =>
                let val result = UNARY (NEGATE_INT, hd) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => (print "unexpected case in yard.sml 169\n"; raise Size)
          end
      | [L.EOF] => valStack
      | [] => valStack
      | hd :: tl => (print "unexpected token in reduceUntilEmpty\n"; raise Size)

    fun binary (tokens, fnStack, valStack) =
      case tokens of
        L.PLUS :: tl =>
          let
            val (fnStack, valStack) = reduce (PLUS, fnStack, valStack)
            val fnStack = L.PLUS :: fnStack
          in
            unary (tl, fnStack, valStack)
          end
      | L.MINUS :: tl =>
          let
            val (fnStack, valStack) = reduce (PLUS, fnStack, valStack)
            val fnStack = L.MINUS :: fnStack
          in
            unary (tl, fnStack, valStack)
          end
      | L.ASTERISK :: tl =>
          let
            val (fnStack, valStack) = reduce (TIMES, fnStack, valStack)
            val fnStack = L.ASTERISK :: fnStack
          in
            unary (tl, fnStack, valStack)
          end
      | L.SLASH :: tl =>
          let
            val (fnStack, valStack) = reduce (DIV, fnStack, valStack)
            val fnStack = L.SLASH :: fnStack
          in
            unary (tl, fnStack, valStack)
          end
      | L.R_PAREN :: tl =>
          let val (fnStack, valStack) = reduceParens (fnStack, valStack)
          in binary (tl, fnStack, valStack)
          end
      | [] => (fnStack, valStack)
      | [L.EOF] => (fnStack, valStack)
      | _ => (print "unexpected binary\n"; raise Size)

    and unary (tokens, fnStack, valStack) =
      case tokens of
      (* constantds, identifiers/variables *)
        L.STRING str :: tl =>
          let
            val lit = getStringLiteral str
            val valStack = lit :: valStack
          in
            binary (tl, fnStack, valStack)
          end
      | L.INT num :: tl =>
          let
            val lit = getIntLiteral num
            val valStack = lit :: valStack
          in
            binary (tl, fnStack, valStack)
          end
      | L.ID valName :: tl =>
          let
            val id = VAL_ID valName
            val valStack = id :: valStack
          in
            binary (tl, fnStack, valStack)
          end
      (* L_PAREN for grouping *)
      (*
      | L.L_PAREN :: tl =>
          let
            val fnStack = L.PAREN :: fnStack
          in
            unary (tl, fnStack, valStack)
          end
          *)
      (* operators *)
      | L.TILDE :: tl =>
          let val fnStack = L.TILDE :: fnStack
          in unary (tl, fnStack, valStack)
          end
      | L.L_PAREN :: tl =>
          let val fnStack = L.L_PAREN :: fnStack
          in unary (tl, fnStack, valStack)
          end
      | [] => (fnStack, valStack)
      | [L.EOF] => (fnStack, valStack)
      | _ => (print "unexpected token during unary stage"; raise Size)
  in
    fun dblE tokens =
      let val (fnStack, valStack) = unary (tokens, [], [])
      in reduceUntilEmpty (fnStack, valStack)
      end
  end
end

fun dbl str =
  let
    val tokens = Lexer.getTokens str
    val r = Yard.dblE tokens
  in
    r
  end
