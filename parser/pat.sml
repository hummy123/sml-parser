structure Pat =
struct
  open ParseType

  (* grammar for patterns:
   *
   * patrow      ::= lab = pat (, patrow)
   *                                  pattern
   *                 id (: base_type) (as pat) (, patrow)
   *                                  variable
   *                 ...
   *                                  wildcard
   *
   * atomic_pat  ::= -
   *                                  wildcard
   *                 scon
   *                                  special constant
   *                 (op) id
   *                                  variable
   *                 (op) longbid (pat)
   *                                  construction
   *                 { patrow }
   *                                  record pattern
   *                 ()
   *                                  unit pattern
   *                 (pat1, ...patn)
   *                                  tuple pattern
   *                 (pat)
   *                                  parenthesised pattern
   *                 [pat1, ...patn]
   *                                  list pattern
   *                #[pat1, ...patn]
   *                                  vector pattern
   *
   * base_pat    ::= atomic_pat
   *                                  atomic pattern
   *                 (op) longvid atpat
   *                                  constructed pattern
   *                 (op) vid (: base_type) as pat
   *                                  layered pattern
   *
   * pat         ::= base_pat
   *                                  base pattern
   *                 pat1 vid pat2
   *                                  infix constructor pattern
   *                 pat : base_type
   *                                  typed pattern
   * *)

  structure L = Lexer

  fun tryOrDefault (f, exp, tokens) =
    case f (tokens, exp) of
      ERR => (tokens, exp)
    | OK (tokens, exp) => (tokens, exp)

  fun firstIfOK (res1, res2) =
    case res1 of
      OK _ => res1
    | ERR => res2

  fun scon tokens =
    case tokens of
      L.INT num :: tl => OK (tl, INT_PAT num)
    | L.STRING str :: tl => OK (tl, STRING_PAT str)
    | L.BOOL b :: tl => OK (tl, BOOL_PAT b)
    | _ => ERR

  fun wildcard tokens =
    case tokens of
      L.WILDCARD :: tl => OK (tl, WILDCARD_PAT)
    | _ => ERR

  fun variable (tokens, env) = 
    case tokens of
      L.OP :: L.ID id :: tl => OK (tl, ID_PAT id)
    | L.ID id :: tl =>
        if ParseEnv.isInfix (id, env) then
          ERR
        else
          OK (tl, ID_PAT id)
    | _ => ERR
end
