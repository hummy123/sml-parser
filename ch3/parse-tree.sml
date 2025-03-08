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

  datatype exp = LITERAL of literal | BINARY of exp * opt * exp | GROUP of exp

  structure L = Lexer

  fun primary (expr, next) =
    case next of
      L.INT num :: tl =>
        let val literal = INT_LITERAL num
        in (LITERAL literal, tl)
        end
    | L.STRING str :: tl =>
        let val literal = STRING_LITERAL str
        in (LITERAL literal, tl)
        end
    | L.L_PAREN :: tl =>
        let
          val (expr, next) = primary (expr, tl)
          val next =
            case next of
              L.R_PAREN :: tl => tl
            | _ =>
                ( print "expected L_PAREN to be followed by R_PAREN\n"
                ; raise Size
                )
        in
          (GROUP expr, next)
        end
    | _ => (expr, next)

  fun factor (expr, next) =
    let
      val (expr, next) = primary (expr, next)
    in
      case next of
        L.ASTERISK :: tl =>
          let
            val (rightExpr, next) = primary (expr, tl)
            val result = BINARY (expr, TIMES, rightExpr)
          in
            factor (result, next)
          end
      | L.SLASH :: tl =>
          let
            val (rightExpr, next) = primary (expr, tl)
            val result = BINARY (expr, DIV, rightExpr)
          in
            factor (result, next)
          end
      | _ => (expr, next)
    end

  fun term (expr, next) =
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

  fun parseIf (expr, next) =
    let
      val (expr, next) = term (expr, next)
    in
      case next of
        L.AMPERSAND :: tl =>
          let
            val (rightExpr, next) = term (expr, tl)
            val result = BINARY (expr, ANDALSO, rightExpr)
          in
            parseIf (result, next)
          end
      | L.PIPE :: tl =>
          let
            val (rightExpr, next) = term (expr, tl)
            val result = BINARY (expr, ORELSE, rightExpr)
          in
            parseIf (result, next)
          end
      | _ => (expr, next)
    end

  (* start of parsing loop *)
  fun expression next =
    case next of
      L.INT num :: tl =>
        let
          val literal = INT_LITERAL num
          val literal = LITERAL literal
        in
          parseIf (literal, tl)
        end
    | L.STRING str :: tl =>
        let
          val literal = STRING_LITERAL str
          val literal = LITERAL literal
        in
          parseIf (literal, tl)
        end
    | L.L_PAREN :: tl =>
        let
          val (expr, next) = expression tl
          val next =
            case next of
              L.R_PAREN :: tl => tl
            | _ =>
                ( print "expected L_PAREN to be followed by R_PAREN\n"
                ; raise Size
                )
        in
          (GROUP expr, next)
        end
    | _ => raise Size

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
    let val (tree, _) = expression lst
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
    val _ = print "after parse\n"
  in
    parseTree
  end

val _ = main ()
