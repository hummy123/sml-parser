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

  datatype operator =
    TERM of term
  | FACTOR of factor
  | COMPARISON of comparison
  | EQUALITY of equality

  datatype expr =
    LITERAL of literal
  | UNARY of unary * expr
  | BINARY of expr * operator * expr

  fun literalToString lit =
    case lit of
      INT_LITERAL num => String.concat ["INT_LITERAL(", Int.toString num, ")"]
    | STRING_LITERAL str => String.concat ["STRING_LITERAL(", str, ")"]

  fun comparisonToString cmp =
    case cmp of
      LESS_THAN => "<"
    | LESS_THAN_EQUAL => "<="
    | GREATER_THAN => ">"
    | GREATER_THAN_EQUAL => ">="

  fun equalityToString eq =
    case eq of
      EQUALS => "="
    | NOT_EQUALS => "<>"

  fun termToString term =
    case term of
      PLUS => "+"
    | MINUS => "-"

  fun factorToString fct =
    case fct of
      TIMES => "*"
    | DIV => "/"

  fun operatorToString opt =
    case opt of
      TERM term => termToString term
    | FACTOR fct => factorToString fct
    | COMPARISON cmp => comparisonToString cmp
    | EQUALITY eq => equalityToString eq

  fun unaryToString unary =
    case unary of NEGATE_INT => "~"

  fun exprToString (exp: expr) =
    case exp of
      LITERAL l => literalToString l
    | BINARY (l, opt, r) =>
        String.concat
          [ " ( "
          , exprToString l
          , " "
          , operatorToString opt
          , " "
          , exprToString r
          , " ) "
          ]
    | UNARY (unary, expr) =>
        String.concat
          [" ( ", unaryToString unary, " ", exprToString expr, " ) "]

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
      val prev = lexEq :: prev
      val (rightExpr, prev, next) = comparison (prev, next)
      val opt = EQUALITY astEq
      val acc = BINARY (acc, opt, rightExpr)
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
    | L.LESS_OR_EQUAL :: tl =>
        startComparisonLoop (prev, tl, L.LESS_OR_EQUAL, LESS_THAN_EQUAL, acc)
    | _ => (acc, prev, next)

  and startComparisonLoop (prev, next, lexCmp, astCmp, acc) =
    let
      val prev = lexCmp :: prev
      val (rightExpr, prev, next) = term (prev, next)
      val opt = COMPARISON astCmp
      val acc = BINARY (acc, opt, rightExpr)
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
      val opt = TERM astTerm
      val acc = BINARY (acc, opt, rightExpr)
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
      val opt = FACTOR astFct
      val acc = BINARY (acc, opt, rightExpr)
    in
      helpFactor (prev, next, acc)
    end

  and factor (prev, next) =
    let val (leftExpr, prev, next) = unary (prev, next)
    in helpFactor (prev, next, leftExpr)
    end

  and unary (prev, next) =
    case next of
      L.MINUS :: tl =>
        let
          val prev = L.MINUS :: prev
          val (right, prev, next) = unary (prev, tl)
          val result = UNARY (NEGATE_INT, right)
        in
          (result, prev, next)
        end
    | _ => primary (prev, next)

  and advancePastRParen (expr, prev, next) =
    case next of
      L.R_PAREN :: tl =>
        let val prev = L.R_PAREN :: prev
        in (expr, prev, tl)
        end
    | _ => (print "expected rParen but got something else\n"; raise Size)

  and primary (prev, next) =
    case next of
      L.INT num :: tl =>
        let
          val prev = L.INT num :: prev
          val result = INT_LITERAL num
          val result = LITERAL result
        in
          (result, prev, tl)
        end
    | L.STRING str :: tl =>
        let
          val prev = L.STRING str :: prev
          val result = STRING_LITERAL str
          val result = LITERAL result
        in
          (result, prev, tl)
        end
    | L.L_PAREN :: tl =>
        let
          val prev = L.L_PAREN :: prev
          val (expr, prev, next) = equality (prev, tl)
        in
          advancePastRParen (expr, prev, next)
        end
    | [] => (print "empty on primary\n"; raise Size)
    | _ => (print "unmatched on primary\n"; raise Size)

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
                   L.R_BRACE :: L.ID typeID :: L.COLON :: L.ID fieldName :: prev
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

  fun parse lst =
    let val (tree, _, _) = equality ([], lst)
    in tree
    end
end

fun ioToString (io, str) =
  case TextIO.inputLine io of
    SOME tl => ioToString (io, str ^ tl)
  | NONE => str

fun main () =
  let
    val io = TextIO.openIn "../ch2/sample.tiger"
    val str = ioToString (io, "")
    val _ = TextIO.closeIn io
    val tokens = Lexer.getTokens str

    val _ = print "before parse\n"
    val parseTree = ParseTree.parse tokens
    val str = ParseTree.exprToString parseTree
    val _ = print "after parse\n"
  in
    print str
  end

val _ = main ()
