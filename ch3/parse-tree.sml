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

  (*
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
  *)

  datatype literal =
    INT_LITERAL of int
  | STRING_LITERAL of string

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
  | EMPTY

  datatype val_dec = ONE_VAL of string * exp

  datatype dec = TYPE_DEC of string | VAL_DECS of val_dec list | NO_DEC

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
          val (expr, next) = comparison (expr, tl)
          val next = advanceLParen next
        in
          (GROUP expr, next)
        end
    | L.ID name :: tl => (VAL_ID name, tl)
    | _ => (expr, next)

  and finishCall (funName, expr, next, args) =
    let
      val (expr, next) = comparison (expr, next)
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

  (* start of parsing loop *)
  fun expression next =
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
    | L.L_PAREN :: tl =>
        let
          val (expr, next) = expression tl
          val next = advanceLParen next
        in
          comparison (GROUP expr, next)
        end
    | L.TILDE :: _ => comparison (EMPTY, next)
    | _ => raise Size

  fun getValDecs (valList, next) =
    case next of
      L.VAL :: L.ID valName :: L.EQUALS :: tl =>
        let
          val (expr, next) = expression tl
          val valList = ONE_VAL (valName, expr) :: valList
        in
          getValDecs (valList, next)
        end
    | _ => List.rev valList

  fun dec next =
    case next of
      L.VAL :: L.ID valName :: L.EQUALS :: tl =>
        let
          val (expr, next) = expression tl
          val valList = [ONE_VAL (valName, expr)]
          val valList = getValDecs (valList, next)
          val result = VAL_DECS valList
        in
          (result, next)
        end
    | _ => (NO_DEC, next)

  (*
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
  *)

  fun parse lst =
    let val (tree, _) = dec lst
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

    val parseTree = ParseTree.parse tokens
  in
    parseTree
  end

val _ = main ()
