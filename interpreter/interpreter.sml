structure Interpreter =
struct
  structure PT = ParseTree
  structure RV = RuntimeValue

  structure SymbolTable =
    MakeGapMap
      (struct
         type key = string
         type value = RuntimeValue.runtime_value

         fun l (a: string, b: string) = a < b
         fun eq (a: string, b: string) = a = b
         fun g (a: string, b: string) = a > b

         val maxNodeSize = 9
       end)

  local
    fun boolToInt b =
      if b then 1 else 0

    fun intToBool i =
      case i of
        RV.INT_VALUE i => i <> 0
      | _ => (print "called intToBool when runtime type is not int"; raise Size)

    fun literalToRuntimeValue lit =
      case lit of
        PT.INT_LITERAL num => RV.INT_VALUE num
      | PT.STRING_LITERAL str => RV.STRING_VALUE str

    fun times (RV.INT_VALUE l, RV.INT_VALUE r) = l * r
      | times (_, _) =
          (print "times type error\n"; raise Size)

    fun divValue (RV.INT_VALUE l, RV.INT_VALUE r) = l div r
      | divValue (_, _) =
          (print "divValue type error\n"; raise Size)

    fun plus (RV.INT_VALUE l, RV.INT_VALUE r) = l + r
      | plus (_, _) =
          (print "plus type error\n"; raise Size)

    fun minus (RV.INT_VALUE l, RV.INT_VALUE r) = l - r
      | minus (_, _) =
          (print "minus type error\n"; raise Size)

    fun le (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l < r)
      | le (_, _) =
          (print "le type error\n"; raise Size)

    fun leq (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l <= r)
      | leq (_, _) =
          (print "leq type error\n"; raise Size)

    fun ge (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l > r)
      | ge (_, _) =
          (print "ge type error\n"; raise Size)

    fun geq (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l >= r)
      | geq (_, _) =
          (print "geq type error\n"; raise Size)

    fun andExp (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l = 1 andalso r = 1)
      | andExp (_, _) =
          (print "andExp type error\n"; raise Size)

    fun orExp (RV.INT_VALUE l, RV.INT_VALUE r) =
          boolToInt (l = 1 orelse r = 1)
      | orExp (_, _) =
          (print "orExp type error\n"; raise Size)

    fun helpBinaryOpt (l, opt, r) =
      case opt of
        PT.TIMES => times (l, r)
      | PT.DIV => divValue (l, r)
      | PT.PLUS => plus (l, r)
      | PT.MINUS => minus (l, r)

      | PT.LEQ => leq (l, r)
      | PT.GEQ => geq (l, r)
      | PT.LE => le (l, r)
      | PT.GE => ge (l, r)

      | PT.ANDALSO => andExp (l, r)
      | PT.ORELSE => orExp (l, r)

    fun binaryOpt (l, opt, r) =
      RV.INT_VALUE (helpBinaryOpt (l, opt, r))

    fun negateInt value =
      case value of
        RV.INT_VALUE num => RV.INT_VALUE (~num)
      | _ => (print "negateInt type error\n"; raise Size)

    fun getSymbolValue (id, table) =
      case SymbolTable.get (id, table) of
        SOME value => value
      | NONE =>
          ( print "SymbolTable does not have value with requested ID"
          ; raise Size
          )

    fun helpFindFieldName (id, record) =
      case record of
        {fieldName, fieldValue} :: tl =>
          if fieldName = id then fieldValue else helpFindFieldName (id, tl)
      | [] => (print "field name not found"; raise Size)

    fun findFieldName (id, value) =
      case value of
        RV.RECORD_VALUE record => helpFindFieldName (id, record)
      | _ =>
          ( print "called findFieldName but did not pass record type\n"
          ; raise Size
          )
  in
    fun helpRecordExpToValue (record, acc, table) =
      case record of
        {fieldName, fieldValue} :: tl =>
          let
            val fieldValue = loopExp (fieldValue, table)
            val field = {fieldName = fieldName, fieldValue = fieldValue}
            val acc = field :: acc
          in
            helpRecordExpToValue (tl, acc, table)
          end
      | [] => let val acc = List.rev acc in RV.RECORD_VALUE acc end

    and recordExpToValue (record, table) =
      helpRecordExpToValue (record, [], table)

    and loopDecs (decs, table) =
      case decs of
        PT.VAL_DEC (id, exp) :: tl =>
          let
            val expValue = loopExp (exp, table)
            val table = SymbolTable.add (id, expValue, table)
          in
            loopDecs (tl, table)
          end
      | PT.TYPE_DEC _ :: _ => (print "TYPE_DEC not yet supported"; raise Size)
      | PT.FUN_DEC _ :: _ => (print "FUN_DEC not yet supported"; raise Size)
      | [] => table

    and loopExp (exp, table) =
      case exp of
        PT.LITERAL lit => literalToRuntimeValue lit
      | PT.BINARY (l, opt, r) =>
          let
            val l = loopExp (l, table)
            val r = loopExp (r, table)
          in
            binaryOpt (l, opt, r)
          end
      | PT.UNARY (unaryOpt, exp) =>
          let val expValue = loopExp (exp, table)
          in negateInt expValue
          end
      | PT.GROUP exp => loopExp (exp, table)
      | PT.VAL_ID id => getSymbolValue (id, table)
      | PT.FUNCTION_CALL (_, _) =>
          (print "function call not yet implemented"; raise Size)
      | PT.LET_EXPR (decs, exp) =>
          let val table = loopDecs (decs, table)
          in loopExp (exp, table)
          end
      | PT.IF_THEN_ELSE (predicate, thenExp, elseExp) =>
          let
            val isTrue = loopExp (predicate, table)
            val isTrue = intToBool isTrue
          in
            if isTrue then loopExp (thenExp, table)
            else loopExp (elseExp, table)
          end
      | PT.RECORD_EXP record => recordExpToValue (record, table)
      | PT.SELECT_FIELD (id, exp) =>
          let val value = loopExp (exp, table)
          in findFieldName (id, value)
          end
      | PT.EMPTY => (print "received EMPTY on loopExp\n"; raise Size)
  end
end
