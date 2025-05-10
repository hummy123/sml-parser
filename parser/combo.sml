structure Combo =
struct
  open ParseType

  fun choice (funList, tokens) =
    case funList of
      f :: tl =>
        let in
          case f tokens of
            ERR => choice (tl, tokens)
          | (result as OK _) => result
        end
    | [] => ERR

  fun next (result, f) =
    case result of
      OK (tokens, data) => f (tokens, data)
    | ERR => ERR
end
