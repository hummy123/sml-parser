structure ParseTree =
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

  fun advanceLParen next =
    case next of
      L.R_PAREN :: tl => tl
    | hd :: _ =>
        ( print "expected L_PAREN to be followed by R_PAREN\n"
        ; print ("but got: " ^ Lexer.tokenToString hd ^ "\n")
        ; raise Size
        )
    | [] => (print "expected L_PAREN to be followed by R_PAREN\n"; raise Size)

  fun advanceEqual next =
    case next of
      L.EQUALS :: tl => tl
    | _ => (print "264: unexpected token while expecting = \n"; raise Size)

  fun advanceThen next =
    case next of
      L.THEN :: tl => tl
    | _ => (print "60: unexpected token while expecting then"; raise Size)

  fun advanceElse next =
    case next of
      L.ELSE :: tl => tl
    | _ => (print "60: unexpected token while expecting else"; raise Size)

  fun advanceIfComma next =
    case next of
      L.COMMA :: tl => tl
    | _ => next

  fun advanceToLetResult next =
    case next of
      L.IN :: tl => advanceToLetResult tl
    | L.END :: tl => advanceToLetResult tl
    | _ => next

  fun primary (expr, next) =
    case next of
      L.INT num :: tl =>
        let
          val literal = INT_LITERAL num
          val literal = LITERAL literal
        in
          comparison (literal, tl)
        end
    | L.STRING str :: tl =>
        let
          val literal = STRING_LITERAL str
          val literal = LITERAL literal
        in
          comparison (literal, tl)
        end
    (* function call *)
    | L.ID _ :: L.L_PAREN :: _ => comparison (EMPTY, next)
    | L.ID name :: tl => comparison (VAL_ID name, tl)
    | L.L_PAREN :: tl => parseGroup tl
    | L.TILDE :: _ => comparison (EMPTY, next)
    | L.LET :: tl => parseLet tl
    | L.IF :: tl => parseIfExp tl
    | L.L_BRACE :: tl => parseRecord (tl, [])
    | L.HASH :: L.ID fieldName :: tl =>
        let
          val (expr, next) = comparison (EMPTY, tl)
          val result = SELECT_FIELD (fieldName, expr)
        in
          comparison (result, next)
        end
    | _ => (expr, next)

  and finishCall (funName, expr, next, args) =
    let
      val (expr, next) = primary (expr, next)
      val args = expr :: args
    in
      case next of
        L.COMMA :: tl => finishCall (funName, expr, tl, args)
      | L.R_PAREN :: tl =>
          let
            val args = List.rev args
            val result = FUNCTION_CALL (funName, args)
          in
            (result, tl)
          end
      | _ =>
          ( print ("functionCall " ^ funName ^ " did not terminate\n")
          ; raise Size
          )
    end

  and functionCall (expr, next) =
    case next of
      L.ID funName :: L.L_PAREN :: L.R_PAREN :: tl =>
        (* function call with no arguments *)
        let val result = FUNCTION_CALL (funName, [])
        in (result, tl)
        end
    | L.ID funName :: L.L_PAREN :: tl => finishCall (funName, expr, tl, [])
    | _ => primary (expr, next)

  and unary (expr, next) =
    case next of
      L.TILDE :: tl =>
        let
          val (expr, next) = unary (expr, tl)
          val result = UNARY (NEGATE_INT, expr)
        in
          (result, next)
        end
    | _ => functionCall (expr, next)

  and factor (expr, next) =
    let
      val (expr, next) = unary (expr, next)
    in
      case next of
        L.ASTERISK :: tl =>
          let
            val (rightExpr, next) = unary (expr, tl)
            val result = BINARY (expr, TIMES, rightExpr)
          in
            factor (result, next)
          end
      | L.SLASH :: tl =>
          let
            val (rightExpr, next) = unary (expr, tl)
            val result = BINARY (expr, DIV, rightExpr)
          in
            factor (result, next)
          end
      | _ => (expr, next)
    end

  and term (expr, next) =
    let
      val (expr, next) = factor (expr, next)
    in
      case next of
        L.MINUS :: tl =>
          let
            val (rightExpr, next) = factor (expr, tl)
            val result = BINARY (expr, MINUS, rightExpr)
          in
            term (result, next)
          end
      | L.PLUS :: tl =>
          let
            val (rightExpr, next) = factor (expr, tl)
            val result = BINARY (expr, PLUS, rightExpr)
          in
            term (result, next)
          end
      | _ => (expr, next)
    end

  and comparison (expr, next) =
    let
      val (expr, next) = term (expr, next)
    in
      case next of
        L.AMPERSAND :: tl =>
          let
            val (rightExpr, next) = term (expr, tl)
            val result = BINARY (expr, ANDALSO, rightExpr)
          in
            comparison (result, next)
          end
      | L.PIPE :: tl =>
          let
            val (rightExpr, next) = term (expr, tl)
            val result = BINARY (expr, ORELSE, rightExpr)
          in
            comparison (result, next)
          end
      | _ => (expr, next)
    end

  and parseRecord (next, acc) =
    case next of
      L.ID fieldName :: L.EQUALS :: tl =>
        let
          val (expr, next) = expression tl
          val acc = {fieldName = fieldName, fieldValue = expr} :: acc
          val next = advanceIfComma next
        in
          parseRecord (next, acc)
        end
    | L.R_BRACE :: tl =>
        let
          val record = List.rev acc
          val record = RECORD_EXP record
        in
          (record, tl)
        end
    | _ => (print "258 unexpected token while parsing record\n"; raise Size)

  and parseIfExp next =
    let
      val (predicate, next) = expression next
      val next = advanceThen next

      val (ifExp, next) = expression next
      val next = advanceElse next

      val (elseExp, next) = expression next
      val result = IF_THEN_ELSE (predicate, ifExp, elseExp)
    in
      (result, next)
    end

  and parseLet next =
    let
      val (decs, next) = getDecs (next, [])
      val next = advanceToLetResult next
      val (expr, next) = comparison (EMPTY, next)
      val result = LET_EXPR (decs, expr)
    in
      (result, next)
    end

  and parseGroup next =
    let
      val (expr, next) = expression next
      val next = advanceLParen next
    in
      comparison (GROUP expr, next)
    end

  (* start of parsing loop *)
  and expression next =
    case next of
      L.INT num :: tl =>
        let
          val literal = INT_LITERAL num
          val literal = LITERAL literal
        in
          comparison (literal, tl)
        end
    | L.STRING str :: tl =>
        let
          val literal = STRING_LITERAL str
          val literal = LITERAL literal
        in
          comparison (literal, tl)
        end
    (* function call *)
    | L.ID _ :: L.L_PAREN :: _ => comparison (EMPTY, next)
    | L.ID name :: tl => comparison (VAL_ID name, tl)
    | L.L_PAREN :: tl => parseGroup tl
    | L.TILDE :: _ => comparison (EMPTY, next)
    | L.LET :: tl => comparison (parseLet tl)
    | L.IF :: tl => comparison (parseIfExp tl)
    | L.L_BRACE :: tl => comparison (parseRecord (tl, []))
    | L.HASH :: L.ID fieldName :: tl =>
        let
          val (expr, next) = comparison (EMPTY, tl)
          val result = SELECT_FIELD (fieldName, expr)
        in
          comparison (result, next)
        end
    | _ => (print "243 unexpected token\n"; raise Size)

  and getTypeDec (next, typeName, fieldAcc) =
    case next of
      L.ID fieldName :: L.COLON :: L.ID fieldValue :: tl =>
        let
          val fieldAcc =
            {fieldName = fieldName, fieldValue = fieldValue} :: fieldAcc

          val tl = advanceIfComma tl
        in
          getTypeDec (tl, typeName, fieldAcc)
        end
    | L.R_BRACE :: tl =>
        let val result = TYPE_DEC (typeName, List.rev fieldAcc)
        in (result, tl)
        end
    | _ => (print "262: unexpected token while parsing type\n"; raise Size)

  and finishFunDec (next, args, funName) =
    case next of
      L.ID argName :: L.COMMA :: tl =>
        let val args = argName :: args
        in finishFunDec (tl, args, funName)
        end
    | L.ID argName :: L.R_PAREN :: tl =>
        let
          val args = argName :: args
          val args = List.rev args
          val next = advanceEqual tl
          val (expr, next) = expression next
          val result = FUN_DEC (funName, args, expr)
        in
          (result, next)
        end
    | L.R_PAREN :: tl =>
        let
          val next = advanceEqual tl
          val (expr, next) = expression next
          val result = FUN_DEC (funName, [], expr)
        in
          (result, next)
        end
    | _ => (print "278: unexpected token while parsing fun dec\n"; raise Size)

  and getDecs (next, acc) =
    case next of
      L.VAL :: L.ID valName :: L.EQUALS :: tl =>
        let
          val (expr, next) = expression tl
          val value = VAL_DEC (valName, expr)
          val valList = value :: acc
        in
          getDecs (next, valList)
        end
    | L.TYPE :: L.ID typeName :: L.EQUALS :: L.L_BRACE :: tl =>
        let
          val (typeDec, next) = getTypeDec (tl, typeName, [])
          val acc = typeDec :: acc
        in
          getDecs (next, acc)
        end
    | L.FUN :: L.ID funName :: L.L_PAREN :: tl =>
        let
          val (funDec, next) = finishFunDec (tl, [], funName)
          val acc = funDec :: acc
        in
          getDecs (next, acc)
        end
    | _ => (List.rev acc, next)

  fun parse lst =
    let val (tree, _) = expression lst
    in tree
    end
end
