structure AllDfa =
struct
  type all_dfa =
    { curID: int
    , curInt: int
    , curPunct: int
    , curWild: int
    , curType: int
    , curSpace: int

    , lastID: int
    , lastInt: int
    , lastPunct: int
    , lastWild: int
    , lastType: int
    , lastSpace: int
    }

  val initial =
    { curID = IdDfa.start
    , curInt = IntDfa.start
    , curPunct = PunctDfa.start
    , curWild = WildcardDfa.start
    , curType = TypeIdDfa.start
    , curSpace = SpaceDfa.start

    , lastID = ~1
    , lastInt = ~1
    , lastPunct = ~1
    , lastWild = ~1
    , lastType = ~1
    , lastSpace = ~1
    }

  fun areAllDead (dfa: all_dfa) =
    let
      val {curID, curInt, curPunct, curWild, curType, curSpace, ...} = dfa
    in
      curID = 0 andalso curInt = 0 andalso curPunct = 0 andalso curWild = 0
      andalso curType = 0 andalso curSpace = 0
    end

  fun update (chr, dfa: all_dfa, pos) =
    let
      val
        { curID
        , curInt
        , curPunct
        , curWild
        , curType
        , curSpace

        , lastID
        , lastInt
        , lastPunct
        , lastWild
        , lastType
        , lastSpace
        } = dfa

      val curID = IdDfa.getNewState (chr, curID)
      val curInt = IntDfa.getNewState (chr, curInt)
      val curPunct = PunctDfa.getNewState (chr, curPunct)
      val curWild = WildcardDfa.getNewState (chr, curWild)
      val curType = TypeIdDfa.getNewState (chr, curType)
      val curSpace = SpaceDfa.getNewState (chr, curSpace)

      val lastID = if IdDfa.isFinal curID then pos else lastID
      val lastInt = if IntDfa.isFinal curInt then pos else lastInt
      val lastPunct = if PunctDfa.isFinal curPunct then pos else lastPunct
      val lastWild = if WildcardDfa.isFinal curWild then pos else lastWild
      val lastType = if TypeIdDfa.isFinal curType then pos else lastType
      val lastSpace = if SpaceDfa.isFinal curSpace then pos else lastSpace
    in
      { curID = curID
      , curInt = curInt
      , curPunct = curPunct
      , curWild = curWild
      , curType = curType
      , curSpace = curSpace

      , lastID = lastID
      , lastInt = lastInt
      , lastPunct = lastPunct
      , lastWild = lastWild
      , lastType = lastType
      , lastSpace = lastSpace
      }
    end
end
