structure ParseTree =
struct
  datatype var_type =
    INT of int
  | STRING of string
  | COMPOUND of (string * var_type) list

  datatype parse_tree = 
    TYPE_ID of string

  open Lexer

  fun ty tl =
    case tl of
      ID fieldName :: COLON :: ID typeID :: tl =>
        (* add field to map *)
        (case tl of
          COMMA =>
            (* continue to parse type *)
            ty tl
        | R_BRACE => 
          (* terminate type *)
          )

  fun typ tl =
    case tl of
      ID typeID :: EQUALS :: L_BRACE :: tl => ty tl
    | ID typeAlias :: EQUALS :: ID origTypeID :: tl =>
    | ARRAY :: OF :: typeID =>

  fun tok lst =
    case lst of
      TYPE :: tl =>

end
