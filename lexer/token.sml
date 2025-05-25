structure Token =
struct
  datatype t =
    INT of int
  | STRING of string
  | BOOL of bool
  | ID of string
  | TYPE_ID of {isEqType: bool, id: string}
  | LONG_ID of string list

  (* reserved words *)
  | LET
  | IN
  | END
  | FUN
  | VAL
  | REC
  | AND
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
  | FN

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

  | EOF

  fun toString tok =
    case tok of
      INT num => "INT(" ^ Int.toString num ^ ")"
    | STRING str => "STRING(" ^ str ^ ")"
    | BOOL b => "BOOL(" ^ Bool.toString b ^ ")"
    | ID id => "ID(" ^ id ^ ")"
    | TYPE_ID {isEqType, id} =>
        "TYPE_ID{isEqType: " ^ Bool.toString isEqType ^ ", id: " ^ id ^ ")"
    | LONG_ID strList =>
        let val str = String.concatWith "," strList
        in String.concat ["LONG_ID(", str, ")"]
        end

    (* reserved words *)
    | LET => "let"
    | IN => "in"
    | END => "end"
    | FUN => "fun"
    | VAL => "val"
    | REC => "rec"
    | AND => "and"
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
    | FN => "fn"

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

    | EOF => "EOF"
end
