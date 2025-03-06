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

  datatype ty_env_value =
    TYPE_ALIAS of string
  | TYPE_DEC of FieldMap.t
  | ARRAY_DEC of string

  structure TyEnv =
    MakeGapMap
      (struct
         (* key = user-given name to type.
          * There may be multiple types of the same name, 
          * one declared after another.
          * In that case, the old type is shadowed. *)
         type key = string
         type value = ty_env_value

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

  structure L = Lexer

  (* loop over multiple equality expressions *)
  fun helpEquality (next, acc) =
    case next of
      L.EQUALS :: tl => startEqualityLoop (tl, L.EQUALS, EQUALS, acc)
    | L.NOT_EQUALS :: tl =>
        startEqualityLoop (tl, L.NOT_EQUALS, NOT_EQUALS, acc)
    | _ => (acc, next)

  and startEqualityLoop (next, lexEq, astEq, acc) =
    let
      val (rightExpr, next) = comparison next
      val opt = EQUALITY astEq
      val acc = BINARY (acc, opt, rightExpr)
    in
      helpEquality (next, acc)
    end

  and equality next =
    let val (leftExpr, next) = comparison next
    in helpEquality (next, leftExpr)
    end

  and helpComparison (next, acc) =
    case next of
      L.GREATER_THAN :: tl =>
        startComparisonLoop (tl, L.GREATER_THAN, GREATER_THAN, acc)
    | L.GREATER_THAN_OR_EQUAL :: tl =>
        startComparisonLoop
          (tl, L.GREATER_THAN_OR_EQUAL, GREATER_THAN_EQUAL, acc)
    | L.LESS_THAN :: tl => startComparisonLoop (tl, L.LESS_THAN, LESS_THAN, acc)
    | L.LESS_OR_EQUAL :: tl =>
        startComparisonLoop (tl, L.LESS_OR_EQUAL, LESS_THAN_EQUAL, acc)
    | _ => (acc, next)

  and startComparisonLoop (next, lexCmp, astCmp, acc) =
    let
      val (rightExpr, next) = term next
      val opt = COMPARISON astCmp
      val acc = BINARY (acc, opt, rightExpr)
    in
      helpComparison (next, acc)
    end

  and comparison next =
    let val (leftExpr, next) = term next
    in helpComparison (next, leftExpr)
    end

  and helpTerm (next, acc) =
    case next of
      L.MINUS :: tl => startTermLoop (tl, L.MINUS, MINUS, acc)
    | L.PLUS :: tl => startTermLoop (tl, L.PLUS, PLUS, acc)
    | _ => (acc, next)

  and startTermLoop (next, lexTerm, astTerm, acc) =
    let
      val (rightExpr, next) = factor next
      val opt = TERM astTerm
      val acc = BINARY (acc, opt, rightExpr)
    in
      helpTerm (next, acc)
    end

  and term next =
    let val (leftExpr, next) = factor next
    in helpTerm (next, leftExpr)
    end

  and helpFactor (next, acc) =
    case next of
      L.SLASH :: tl => startFactorLoop (tl, L.SLASH, DIV, acc)
    | L.ASTERISK :: tl => startFactorLoop (tl, L.ASTERISK, TIMES, acc)
    | _ => (acc, next)

  and startFactorLoop (next, lexFct, astFct, acc) =
    let
      val (rightExpr, next) = unary next
      val opt = FACTOR astFct
      val acc = BINARY (acc, opt, rightExpr)
    in
      helpFactor (next, acc)
    end

  and factor next =
    let val (leftExpr, next) = unary next
    in helpFactor (next, leftExpr)
    end

  and unary next =
    case next of
      L.MINUS :: tl =>
        let
          val (right, next) = unary tl
          val result = UNARY (NEGATE_INT, right)
        in
          (result, next)
        end
    | _ => primary next

  and advancePastRParen (expr, next) =
    case next of
      L.R_PAREN :: tl => (expr, tl)
    | _ => (print "expected rParen but got something else\n"; raise Size)

  and primary next =
    case next of
      L.INT num :: tl =>
        let
          val result = INT_LITERAL num
          val result = LITERAL result
        in
          (result, tl)
        end
    | L.STRING str :: tl =>
        let
          val result = STRING_LITERAL str
          val result = LITERAL result
        in
          (result, tl)
        end
    | L.L_PAREN :: tl =>
        let val (expr, next) = equality tl
        in advancePastRParen (expr, next)
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
    let val (tree, _) = equality lst
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
