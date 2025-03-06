structure ParseTree =
struct
  structure FieldMap =
    MakeGapMap
      (struct
         type key = string
         type value = string

         fun l (s1: string, s2) = s1 < s2
         fun eq (s1: string, s2) = s1 = s2
         fun g (s1: string, s2) = s1 > s2

         val maxNodeSize = 8
       end)

  datatype literal = INT_LITERAL of int | STRING_LITERAL of string

  datatype comparison =
    LESS_THAN
  | LESS_THAN_EQUAL
  | GREATER_THAN
  | GREATER_THAN_EQUAL

  datatype equality = EQUALS | NOT_EQUALS

  datatype term = PLUS | MINUS

  datatype factor = DIV | TIMES

  datatype unary = NEGATE_INT

  datatype expr =
    LITERAL of literal
  | COMPARISON of comparison
  | EQUALITY of equality

  datatype operator = TERM of term | FACTOR of factor

  datatype parse_tree =
    BINARY of expr * operator * expr
  | UNARY of operator * expr
  | LITERAL of literal

  datatype ty_env_value =
    TYPE_ALIAS of string
  | TYPE_DEC of FieldMap.t
  | ARRAY_DEC of string

  structure TyEnv =
    MakeGapMap
      (struct
         (* phrase number? *)
         type key = string
         type value = ty_env_value

         fun l (s1: string, s2) = s1 < s2
         fun eq (s1: string, s2) = s1 = s2
         fun g (s1: string, s2) = s1 > s2

         val maxNodeSize = 8
       end)

  structure L = Lexer

  (* loop over multiple equality expressions *)
  fun helpEquality (prev, next, acc) =
    case next of
      L.EQUALS :: tl => startEqualityLoop (prev, tl, L.EQUALS, EQUALS, acc)
    | L.NOT_EQUALS :: tl =>
        startEqualityLoop (prev, tl, L.NOT_EQUALS, NOT_EQUALS, acc)
    | _ => (acc, prev, next)

  and startEqualityLoop (prev, next, lexEq, astEq, acc) =
    let
      val prev = leqEq :: prev
      val (rightExpr, prev, next) = comparison (prev, next)
      val acc = BINARY (acc, astEq, rightExpr)
    in
      helpEquality (prev, next, acc)
    end

  and equality (prev, next) =
    let val (leftExpr, prev, next) = comparison (prev, next)
    in helpEquality (prev, next, leftExpr)
    end

  and helpComparison (prev, next, acc) =
    case next of
      L.GREATER_THAN :: tl =>
        startComparisonLoop (prev, tl, L.GREATER_THAN, GREATER_THAN, acc)
    | L.GREATER_THAN_OR_EQUAL :: tl =>
        startComparisonLoop
          (prev, tl, L.GREATER_THAN_OR_EQUAL, GREATER_THAN_EQUAL, acc)
    | L.LESS_THAN :: tl =>
        startComparisonLoop (prev, tl, L.LESS_THAN, LESS_THAN, acc)
    | L.LESS_THAN_OR_EQUAL :: tl =>
        startComparisonLoop
          (prev, tl, L.LESS_THAN_OR_EQUAL, LESS_THAN_EQUAL, acc)
    | _ => (acc, prev, next)

  and startComparisonLoop (prev, next, lexCmp, astCmp, acc) =
    let
      val prev = lexCmp :: prev
      val (rightExpr, prev, next) = term (prev, next)
      val acc = BINARY (acc, astCmp, rightExpr)
    in
      helpComparison (prev, next, acc)
    end

  and comparison (prev, next) =
    let val (leftExpr, prev, next) = term (prev, next)
    in helpComparison (prev, next, leftExpr)
    end

  and helpTerm (prev, next, acc) =
    case next of
      L.MINUS :: tl => startTermLoop (prev, tl, L.MINUS, MINUS, acc)
    | L.PLUS :: tl => startTermLoop (prev, tl, L.PLUS, PLUS, acc)
    | _ => (acc, prev, next)

  and startTermLoop (prev, next, lexTerm, astTerm, acc) =
    let
      val prev = lexTerm :: prev
      val (rightExpr, prev, next) = factor (prev, next)
      val acc = BINARY (acc, astTerm, rightExpr)
    in
      helpTerm (prev, next, acc)
    end

  and term (prev, next) =
    let val (leftExpr, prev, next) = factor (prev, next)
    in helpTerm (prev, next, leftExpr)
    end

  and helpFactor (prev, next, acc) =
    case next of
      L.SLASH :: tl => startFactorLoop (prev, tl, L.SLASH, DIV, acc)
    | L.ASTERISK :: tl => startFactorLoop (prev, tl, L.ASTERISK, TIMES, acc)
    | _ => (acc, prev, next)

  and startFactorLoop (prev, next, lexFct, astFct, acc) =
    let
      val prev = lexFct :: prev
      val (rightExpr, prev, next) = unary (prev, next)
      val acc = BINARY (acc, astFct, rightExpr)
    in
      helpFactor (prev, next, acc)
    end

  and factor (prev, next) =
    let val (leftExpr, prev, next) = unary (prev, next)
    in helpFactor (prev, next, leftExpr)
    end

  fun ty (prev, next, fieldMap, typeName, tyEnv) =
    case next of
      L.ID fieldName :: L.COLON :: L.ID typeID :: tl =>
        (* add field to map *)
        let
          val fieldMap = FieldMap.add (fieldName, typeID, fieldMap)
        in
          (case tl of
             L.COMMA :: tl =>
               (* continue to parse type *)
               let
                 val prev =
                   L.COMMA :: L.ID typeID :: L.COLON :: L.ID fieldName :: prev
                 val fieldMap = FieldMap.add (fieldName, typeID, fieldMap)
               in
                 ty (prev, tl, fieldMap, typeName, tyEnv)
               end
           | L.R_BRACE :: tl =>
               (* terminate type *)
               let
                 val prev =
                   L.R_BRACE :: L.ID typeID :: L.COLON :: L.fieldName :: prev
               in
                 (prev, tl, TyEnv.add (typeName, TYPE_DEC fieldMap, tyEnv))
               end
           | hd :: _ =>
               let val _ = print "expected , or } but found something else\n"
               in raise Size
               end
           | [] =>
               let val _ = print "expected , or } but reached end of file\n"
               in raise Size
               end)
        end
    | _ => (print "53 unexpected"; raise Size)

  fun typ (prev, next, tyEnv) =
    case next of
      L.ID typeName :: L.EQUALS :: L.L_BRACE :: tl =>
        let val prev = L.L_BRACE :: L.EQUALS :: L.ID typeName :: prev
        in ty (prev, tl, FieldMap.empty, typeName, tyEnv)
        end
    | L.ID typeAlias :: L.EQUALS :: L.ID origTypeName :: tl =>
        let
          val prev = L.ID origTypeName :: L.EQUALS :: L.ID typeAlias :: prev
          val tyEnv = TyEnv.add (typeAlias, TYPE_ALIAS origTypeName, tyEnv)
        in
          (prev, tl, tyEnv)
        end
    | L.ID typeAlias :: L.EQUALS :: L.ARRAY :: L.OF :: L.ID origTypeName :: tl =>
        let
          val prev =
            L.ID origTypeName :: L.OF :: L.ARRAY :: L.EQUALS :: L.ID typeAlias
            :: prev
          val tyEnv = TyEnv.add (typeAlias, ARRAY_DEC origTypeName, tyEnv)
        in
          (prev, tl, tyEnv)
        end
    | _ => (print "85 unexpected"; raise Size)

  fun helpTok (prev, next, tyEnv) =
    case next of
      L.TYPE :: tl =>
        let
          val (prev, tl, tyEnv) = typ (tl, tyEnv)
          val prev = L.TYPE :: prev
        in
          helpTok (prev, tl, tyEnv)
        end
    | L.EQUALS :: tl => equality (L.EQUALS :: prev, tl, L.EQUALS)
    | [L.EOF] => tyEnv
    | _ => (print "90 unexpected"; raise Size)

  fun tok lst = helpTok (lst, TyEnv.empty)
end
