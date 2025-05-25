structure ParseType =
struct
  datatype type_grm =
    TY_VAR of {isEq: bool, id: string}
  | RECORD_TYPE of (string * type_grm) list
  | TY_CON of {tyseq: type_grm list, con: string list}
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

  datatype exp =
    INT_EXP of int
  | BOOL_EXP of bool
  | STRING_EXP of string

  | RECORD_EXP of (string * exp) list
  | LIST_EXP of exp list
  | VECTOR_EXP of exp vector

  | EXP_VAL_ID of string
  | RECORD_SELECTOR of string
  | UNIT_EXP

  | APP_EXP of exp list
  | GROUP_EXP of exp

  | ANDALSO_EXP of exp * exp
  | ORELSE_EXP of exp * exp

  | TYPED_EXP of exp * type_grm

  | RAISE_EXP of exp
  | HANDLE_EXP of (pat * exp) list
  | WHILE_EXP of exp * exp

  | IF_EXP of exp * exp * exp
  | CASE_EXP of exp * (pat * exp) list
  | FN_EXP of (pat * exp) list

  and dec =
    VAL of type_grm list * pat * exp
  | VAL_REC of type_grm list * pat * exp
  | SEQ_DEC of dec list

  datatype 'a result = OK of Token.t list * 'a | ERR
end
