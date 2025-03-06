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

  datatype expr =
    LITERAL of literal
  | COMPARISON of comparison
  | EQUALITY of equality

  and literal =
    INT_LITERAL of int
  | STRING_LITERAL of string
  | WITH_PARENS of expr

  and comparison =
    LESS_THAN
  | LESS_THAN_EQUAL
  | GREATER_THAN
  | GREATER_THAN_EQUAL

  and equality =
    EQUALS
  | NOT_EQUALS

  datatype ty_env_value =
    TYPE_ALIAS of string
  | TYPE_DEC of FieldMap.t
  | ARRAY_DEC of string

  datatype term = PLUS | MINUS

  datatype factor = DIV | TIMES

  datatype unary = NEGATE

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

  datatype parse_tree = TYPE_ID of string

  structure L = Lexer

  fun ty (tl, fieldMap, typeName, tyEnv) =
    case tl of
      L.ID fieldName :: L.COLON :: L.ID typeID :: tl =>
        (* add field to map *)
        let
          val fieldMap = FieldMap.add (fieldName, typeID, fieldMap)
        in
          (case tl of
             L.COMMA :: tl =>
               (* continue to parse type *)
               let val fieldMap = FieldMap.add (fieldName, typeID, fieldMap)
               in ty (tl, fieldMap, typeName, tyEnv)
               end
           | L.R_BRACE :: tl =>
               (* terminate type *)
               (tl, TyEnv.add (typeName, TYPE_DEC fieldMap, tyEnv))
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
    | [EOF] => tyEnv
    | _ => (print "90 unexpected"; raise Size)

  fun tok lst = helpTok (lst, TyEnv.empty)
end
