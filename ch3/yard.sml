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
                | _ => raise Fail "reduce valStack case\n"
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
                | _ => raise Fail "reduce valStack case\n"
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
                | _ => raise Fail "reduce valStack case\n"
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
                | _ => raise Fail "reduce valStack case\n"
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
              | _ => raise Fail "reduce valStack case\n"
            end
        | L.L_PAREN :: fntl => (fnStack, valStack)
        | hd :: tl =>
            raise Fail ("reduce fnStack case [" ^ L.tokenToString hd ^ "]\n")
        | [] => (fnStack, valStack)
      end

    fun reduceParens (fnStack, valStack) =
      case fnStack of
        L.L_PAREN :: fntl =>
          let in
            case valStack of
              hd :: valtl =>
                let val result = GROUP hd :: valtl
                in (fntl, result)
                end
            | _ => raise Fail "reduceParens L_PAREN"
          end
      | L.PLUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, PLUS, b)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens plus"
          end
      | L.MINUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, MINUS, b)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens minus"
          end
      | L.ASTERISK :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, TIMES, b)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens times"
          end
      | L.SLASH :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, DIV, b)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens div"
          end
      | L.TILDE :: fntl =>
          let in
            case valStack of
              hd :: valtl =>
                let val result = UNARY (NEGATE_INT, hd)
                in reduceParens (fntl, result :: valtl)
                end
            | _ => raise Fail "reduceParens tilde"
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
            | _ => raise Fail "unexpected case in yard.sml 91\n"
          end
      | L.MINUS :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, MINUS, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => raise Fail "unexpected case in yard.sml 99\n"
          end
      | L.ASTERISK :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, TIMES, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => raise Fail "unexpected case in yard.sml 107\n"
          end
      | L.SLASH :: fntl =>
          let in
            case valStack of
              b :: a :: valtl =>
                let val result = BINARY (a, DIV, b) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => raise Fail "unexpected case in yard.sml 115\n"
          end
      | L.TILDE :: fntl =>
          let in
            case valStack of
              hd :: valtl =>
                let val result = UNARY (NEGATE_INT, hd) :: valtl
                in reduceUntilEmpty (fntl, result)
                end
            | _ => raise Fail "unexpected case in yard.sml 169\n"
          end
      | [L.EOF] => valStack
      | [] => valStack
      | hd :: tl => raise Fail "unexpected token in reduceUntilEmpty\n"

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
      | [] => (fnStack, valStack, tokens)
      | [L.EOF] => (fnStack, valStack, tokens)
      | hd :: _ =>
          raise Fail ("unexpected binary [ " ^ L.tokenToString hd ^ " ]")

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
      (* operators *)
      | L.TILDE :: tl =>
          let val fnStack = L.TILDE :: fnStack
          in unary (tl, fnStack, valStack)
          end
      | [] => (fnStack, valStack, tokens)
      | [L.EOF] => (fnStack, valStack, tokens)

      (* parse record *)
      | L.L_BRACE :: tl =>
          let
            val (tokens, record) = parseRecord (tl, [])
            val valStack = record :: valStack
          in
            unary (tokens, fnStack, valStack)
          end
      (* parse tuple or group-expression *)
      | L.L_PAREN :: tl =>
          let
            val (tokens, tuple) = parseTuple (tl, [], 1)
            val valStack = tuple :: valStack
          in
            unary (tokens, fnStack, valStack)
          end
      | L.LET :: tl =>
          let
            val (tokens, letExp) = parseLetExp tl
            val valStack = letExp :: valStack
          in
            unary (tokens, fnStack, valStack)
          end
      | hd :: _ =>
          raise Fail ("unexpected unary [ " ^ L.tokenToString hd ^ " ]")

    and splitRecordTokens (expTokens, tokens, braceLevel) =
      case tokens of
        L.COMMA :: tl => (List.rev expTokens, tokens)

      (* we might have nested records, where record is inside another record
       * so we keep track of how many brace pairs we have seen 
       * as we want to parse the whole record *)
      | L.L_BRACE :: tl =>
          splitRecordTokens (L.L_BRACE :: expTokens, tl, braceLevel + 1)
      | L.R_BRACE :: tl =>
          if braceLevel - 1 = 0 then (List.rev expTokens, tokens)
          else splitRecordTokens (L.R_BRACE :: expTokens, tl, braceLevel - 1)

      | hd :: tl => splitRecordTokens (hd :: expTokens, tl, braceLevel)
      | [] => (List.rev expTokens, tokens)

    and splitTupleTokens (expTokens, tokens, parenLevel) =
      case tokens of
        L.COMMA :: tl => (List.rev expTokens, tokens)
      | L.L_PAREN :: tl =>
          splitTupleTokens (L.L_PAREN :: expTokens, tl, parenLevel + 1)
      | L.R_PAREN :: tl =>
          if parenLevel - 1 = 0 then (List.rev expTokens, tokens)
          else splitTupleTokens (L.R_PAREN :: expTokens, tl, parenLevel - 1)
      | hd :: tl => splitTupleTokens (hd :: expTokens, tl, parenLevel)
      | [] => (List.rev expTokens, tokens)

    and parseRecord (tokens, record) =
      case tokens of
        L.ID fieldName :: L.EQUALS :: tl =>
          let
            val (expTokens, tokens) = splitRecordTokens ([], tl, 1)
            val (fnStack, valStack, _) = unary (expTokens, [], [])
            val expresion = reduceUntilEmpty (fnStack, valStack)
            val fieldValue = List.hd expresion
            val record =
              {fieldName = fieldName, fieldValue = fieldValue} :: record
          in
            case tokens of
              L.R_BRACE :: tl => (tl, RECORD_EXP record)
            | L.COMMA :: tl => parseRecord (tl, record)
            | _ => raise Fail "yard.sml 340"
          end
      | _ => raise Fail "yard.sml 342"

    and parseTuple (tokens, record, fieldNum) =
      let
        val (expTokens, tokens) = splitTupleTokens ([], tokens, 1)
        val (fnStack, valStack, _) = unary (expTokens, [], [])
        val expresion = reduceUntilEmpty (fnStack, valStack)
      in
        case tokens of
          L.R_PAREN :: tl =>
            if fieldNum = 1 then
              (tl, GROUP (List.hd expresion))
            else
              let
                val record =
                  { fieldName = Int.toString fieldNum
                  , fieldValue = List.hd expresion
                  } :: record
              in
                (tl, RECORD_EXP record)
              end
        | L.COMMA :: tl =>
            let
              val record =
                { fieldName = Int.toString fieldNum
                , fieldValue = List.hd expresion
                } :: record
            in
              parseTuple (tl, record, fieldNum + 1)
            end
        | _ => raise Fail "yard.sml line 403"
      end

    (* between "in ... end" in "let ... in ... end" *)
    and splitLetExpTokens (expTokens, tokens, letLevel) =
      case tokens of
        L.END :: tl =>
          if letLevel - 1 = 0 then (List.rev expTokens, tokens)
          else splitLetExpTokens (L.END :: expTokens, tl, letLevel - 1)
      | L.LET :: tl => splitLetExpTokens (L.LET :: expTokens, tl, letLevel + 1)
      | hd :: tl => splitLetExpTokens (hd :: expTokens, tl, letLevel)
      | [] => raise Fail "splitLetExpTokens empty"

    and advanceLetEndings tokens =
      case tokens of
        L.END :: tl => advanceLetEndings tl
      | _ => tokens

    and splitValTokens (expTokens, tokens, openLevel) =
      case tokens of
      (* slight trickiness: we are looking for another 'dec' or 'L.in' 
      * so we know when to terminate the splitting.
      * However, it's possible for the expression associated with this val
      * to be something like:
      * 'val a = let val b = ...'
      * in which case we don't want to terminate after seing the first
      * declaration token, as the second 'val' is contained in this 'val'.
      * So, we keep track of an 'openLevel' counter, 
      * increment it when we see a 'let' and decrement it when we see an 'end'.
      * If 'openLevel' is 0, then we can terminate; otherwise, we have to keep
      * parsing through.
      * Question: do we need to consider other types of token
      * s except 'let', 'in' and 'end" ?
      * *)
        L.LET :: tl => splitValTokens (L.LET :: expTokens, tl, openLevel + 1)
      | L.END :: tl => splitValTokens (L.END :: expTokens, tl, openLevel - 1)

      (* tokens we may want to terminate on *)
      | L.VAL :: tl =>
          if openLevel = 0 then (List.rev expTokens, tokens)
          else splitValTokens (L.VAL :: expTokens, tl, openLevel)
      | L.IN :: tl =>
          if openLevel = 0 then (List.rev expTokens, tokens)
          else splitValTokens (L.IN :: expTokens, tl, openLevel)
      | L.TYPE :: tl =>
          if openLevel = 0 then (List.rev expTokens, tokens)
          else splitValTokens (L.TYPE :: expTokens, tl, openLevel)
      | L.FUN :: tl =>
          if openLevel = 0 then (List.rev expTokens, tokens)
          else splitValTokens (L.FUN :: expTokens, tl, openLevel)

      | hd :: tl => splitValTokens (hd :: expTokens, tl, openLevel)
      | [] => raise Fail "yard.sml 462 empty"

    and getDecList (tokens, decs) =
      case tokens of
        L.IN :: tl => (List.rev decs, tokens)
      | L.VAL :: L.ID valName :: L.EQUALS :: tl =>
          let
            val (expTokens, tokens) = splitValTokens ([], tl, 0)
            val (fnStack, valStack, _) = unary (expTokens, [], [])
            val exp = reduceUntilEmpty (fnStack, valStack)
            val valDec = VAL_DEC (valName, List.hd exp)
            val decs = valDec :: decs
          in
            getDecList (tokens, decs)
          end
      (* todo: parse other kinds of decs as well, including types and functions. *)
      | _ => raise Fail "yard.sml 475"

    and parseLetExp tokens =
      let
        val (letDecs, tokens) = getDecList (tokens, [])
      in
        case tokens of
          L.IN :: tl =>
            let
              val (expTokens, tokens) = splitLetExpTokens ([], tl, 1)
              val (fnStack, valStack, _) = unary (expTokens, [], [])
              val letExp = reduceUntilEmpty (fnStack, valStack)
              val result = LET_EXPR (letDecs, List.hd letExp)
              val tokens = advanceLetEndings tokens
            in
              (tokens, result)
            end
        | _ => raise Fail "parseLetExp did not encounter 'in'"
      end
  in
    fun dblE tokens =
      let val (fnStack, valStack, remainingTokens) = unary (tokens, [], [])
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
