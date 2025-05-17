structure InfixType =
struct
  (* infix operator's associativity direction 
   * and precedence level.
   * Equivalent to the SML declarations:
   * `infix 1 +` for left-associative infix operators with binding power of 1
   * `infixr 1 -` for right-associative infix operators with binding power of 1
   * When a function is declared non-fix, it is deleted from the infixMap.
   * *)
  type t = {isLeft: bool, power: int}
end
