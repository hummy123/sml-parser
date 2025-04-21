structure ParseType =
struct
  datatype type_grm =
    TY_VAR of {isEq: bool, id: string}
  | RECORD_TYPE of (string * type_grm) list
  | TY_CON of {tyseq: type_grm list, con: string}
  | TUPLE_TYPE of type_grm list
  | FUN_TY of type_grm list

  datatype pat =
    INT_PAT of int
  | BOOL_PAT of bool
  | STRING_PAT of string
  | RECORD_PAT of (string * pat) list
  | ID_PAT of string
  | UNIT_PAT
  | LIST_PAT of pat list
  | VECTOR_PAT of pat vector
  | WILDCARD_PAT
  | CONSTRUCTED_PAT of string * pat
  | TYPE_ANNOTATED of pat * type_grm
  | AS_PAT of pat * pat

  datatype 'a result = OK of Token.t list * 'a | ERR

  datatype exp =
    INT_EXP of int
  | BOOL_EXP of bool
  | STRING_EXP of string

  | EXP_VAL_ID of string
  | RECORD_SELECTOR of string
  | UNIT_EXP
end
