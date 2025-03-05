structure ParseTree =
struct
  structure FieldMap = MakeGapMap (struct 
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

  structure TyEnv = MakeGapMap (struct
    (* phrase number? *)
    type key = string
    type value = ty_env_value

    fun l (s1: string, s2) = s1 < s2
    fun eq (s1: string, s2) = s1 = s2
    fun g (s1: string, s2) = s1 > s2

    val maxNodeSize = 8
  end)

  datatype parse_tree = 
    TYPE_ID of string

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
            let
              val fieldMap = FieldMap.add (fieldName, typeID, fieldMap)
            in
              ty (tl, fieldMap, typeName, tyEnv)
            end
        | L.R_BRACE :: tl => 
          (* terminate type *)
            (tl, TyEnv.add (typeName, TYPE_DEC fieldMap, tyEnv))
        | hd :: _ => 
            let
              val _ = print "expected , or } but found something else\n"
            in
              raise Size
            end
        | [] =>
            let
              val _ = print "expected , or } but reached end of file\n"
            in
              raise Size
            end)
        end
    | _ => (print "53 unexpected"; raise Size)

  fun typ (tl, tyEnv) =
    case tl of
      L.ID typeName :: L.EQUALS :: L.L_BRACE :: tl => 
          ty (tl, FieldMap.empty, typeName, tyEnv)
    | L.ID typeAlias :: L.EQUALS :: L.ID origTypeName :: tl =>
        let
          val tyEnv =
            TyEnv.add (typeAlias, TYPE_ALIAS origTypeName, tyEnv)
        in
          (tl, tyEnv)
        end
    | L.ID typeAlias :: L.ARRAY :: L.OF :: L.ID origTypeName :: tl => 
        let
          val tyEnv = TyEnv.add (typeAlias, ARRAY_DEC origTypeName, tyEnv)
        in
          (tl, tyEnv)
        end
    | _ => (print "85 unexpected"; raise Size)

  fun helpTok (lst, tyEnv) =
    case lst of
      L.TYPE :: tl => 
      let
        val (tl, tyEnv) = typ (tl, tyEnv)
      in
        helpTok (tl, tyEnv)
      end
    | [EOF] =>
        tyEnv
    | _ => (print "90 unexpected"; raise Size)

  fun tok lst = helpTok (lst, TyEnv.empty)
end
