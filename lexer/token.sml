structure Token =
struct
  datatype t =
    INT of int
  | ID of string
  | STRING of string
  | BOOL of bool
  | TYPE_ID of {isEqType: bool, id: string}

  (* reserved words *)
  | LET
  | IN
  | END
  | FUN
  | VAL
  | TYPE
  | ARRAY
  | IF
  | THEN
  | ELSE
  | CASE
  | OF
  | INFIX
  | AS
  | OP
  | ANDALSO
  | ORELSE
  | RAISE
  | HANDLE
  | WHILE
  | DO

  (* punctuation *)
  | L_PAREN
  | R_PAREN
  | L_BRACKET
  | R_BRACKET
  | L_BRACE
  | R_BRACE
  | COMMA
  | COLON
  | SEMI_COLON
  | TRIPLE_DOT
  | WILDCARD
  | PIPE
  | EQUAL_ARROW
  | DASH_ARROW
  | HASH

  (* singleton dot is not in grammar; maybe remove? *)
  | DOT
  | EOF

  fun toString tok =
    case tok of
      INT num => "INT(" ^ Int.toString num ^ ")"
    | ID id => "ID(" ^ id ^ ")"
    | STRING str => "STRING(" ^ str ^ ")"
    | BOOL b => "BOOL(" ^ Bool.toString b ^ ")"
    | TYPE_ID {isEqType, id} =>
        "TYPE_ID{isEqType: " ^ Bool.toString isEqType ^ ", id: " ^ id ^ ")"

    (* reserved words *)
    | LET => "let"
    | IN => "in"
    | END => "end"
    | FUN => "fun"
    | VAL => "val"
    | TYPE => "type"
    | ARRAY => "array"
    | IF => "if"
    | THEN => "then"
    | ELSE => "else"
    | CASE => "case"
    | OF => "of"
    | INFIX => "infix"
    | AS => "as"
    | OP => "op"
    | ANDALSO => "andalso"
    | ORELSE => "orelse"
    | RAISE => "raise"
    | HANDLE => "handle"
    | WHILE => "while"
    | DO => "do"

    (* punctuation *)
    | L_PAREN => "("
    | R_PAREN => ")"
    | L_BRACKET => "["
    | R_BRACKET => "]"
    | L_BRACE => "{"
    | R_BRACE => "}"
    | COMMA => ","
    | COLON => ":"
    | SEMI_COLON => ";"
    | TRIPLE_DOT => "..."
    | WILDCARD => "_"
    | PIPE => "|"
    | EQUAL_ARROW => "=>"
    | DASH_ARROW => "->"
    | HASH => "#"

    | DOT => "."
    | EOF => "EOF"
end
