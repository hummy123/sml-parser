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

  fun choiceData (funList, tokens, data) =
    case funList of
      f :: tl =>
        let in
          case f (tokens, data) of
            ERR => choiceData (tl, tokens, data)
          | (result as OK _) => result
        end
    | [] => ERR

  fun choiceData2 (funList, tokens, data1, data2) =
    case funList of
      f :: tl =>
        let in
          case f (tokens, data1, data2) of
            ERR => choiceData2 (tl, tokens, data1, data2)
          | (result as OK _) => result
        end
    | [] => ERR

  fun next (result, f) =
    case result of
      OK (tokens, data) => f (tokens, data)
    | ERR => ERR
end
