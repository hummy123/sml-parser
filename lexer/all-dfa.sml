structure AllDfa =
struct
  type all_dfa =
    { curID: int
    , curInt: int
    , curPunct: int
    , curWild: int
    , curType: int
    , curSpace: int
    , curLongID: int

    , lastID: int
    , lastInt: int
    , lastPunct: int
    , lastWild: int
    , lastType: int
    , lastSpace: int
    , lastLongID: int
    }

  val initial =
    { curID = IdDfa.start
    , curInt = IntDfa.start
    , curPunct = PunctDfa.start
    , curWild = WildcardDfa.start
    , curType = TypeIdDfa.start
    , curSpace = SpaceDfa.start
    , curLongID = LongIdDfa.start

    , lastID = ~1
    , lastInt = ~1
    , lastPunct = ~1
    , lastWild = ~1
    , lastType = ~1
    , lastSpace = ~1
    , lastLongID = ~1
    }

  fun areAllDead (dfa: all_dfa) =
    let
      val {curID, curInt, curPunct, curWild, curType, curSpace, curLongID, ...} =
        dfa
    in
      curID = 0 andalso curInt = 0 andalso curPunct = 0 andalso curWild = 0
      andalso curType = 0 andalso curSpace = 0 andalso curLongID = 0
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
        , curLongID

        , lastID
        , lastInt
        , lastPunct
        , lastWild
        , lastType
        , lastSpace
        , lastLongID
        } = dfa

      val curID = IdDfa.getNewState (chr, curID)
      val curInt = IntDfa.getNewState (chr, curInt)
      val curPunct = PunctDfa.getNewState (chr, curPunct)
      val curWild = WildcardDfa.getNewState (chr, curWild)
      val curType = TypeIdDfa.getNewState (chr, curType)
      val curSpace = SpaceDfa.getNewState (chr, curSpace)
      val curLongID = LongIdDfa.getNewState (chr, curLongID)

      val lastID = if IdDfa.isFinal curID then pos else lastID
      val lastInt = if IntDfa.isFinal curInt then pos else lastInt
      val lastPunct = if PunctDfa.isFinal curPunct then pos else lastPunct
      val lastWild = if WildcardDfa.isFinal curWild then pos else lastWild
      val lastType = if TypeIdDfa.isFinal curType then pos else lastType
      val lastSpace = if SpaceDfa.isFinal curSpace then pos else lastSpace
      val lastLongID = if LongIdDfa.isFinal curLongID then pos else lastLongID
    in
      { curID = curID
      , curInt = curInt
      , curPunct = curPunct
      , curWild = curWild
      , curType = curType
      , curSpace = curSpace
      , curLongID = curLongID

      , lastID = lastID
      , lastInt = lastInt
      , lastPunct = lastPunct
      , lastWild = lastWild
      , lastType = lastType
      , lastSpace = lastSpace
      , lastLongID = lastLongID
      }
    end
end
