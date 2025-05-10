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
end
