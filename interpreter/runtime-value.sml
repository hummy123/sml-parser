structure RuntimeValue =
struct
  datatype runtime_value =
    INT_VALUE of int
  | STRING_VALUE of string
  | RECORD_VALUE of {fieldName: string, fieldValue: runtime_value} list
  | FUN_DEC of
      { argNames: string list
      , body: ParseTree.exp
      , env: runtime_value StringMap.t
      }

  structure MakeString =
  struct
    fun makeSpace _ = #" "

    fun makeSpaces numSpaces = CharVector.tabulate (numSpaces, makeSpace)

    fun helpMakeString (value, spaces) =
      case value of
        INT_VALUE num => Int.toString num
      | STRING_VALUE str => str
      | RECORD_VALUE lst =>
          makeRecordString (lst, spaces + 2, if spaces = 0 then "" else "\n")
      | FUN_DEC _ =>
          (print "incorrectly tried to print FUN_VALUE\n"; raise Size)

    and makeRecordString (lst, spaces, acc) =
      case lst of
        {fieldName, fieldValue} :: tl =>
          let
            val leading = makeSpaces spaces
            val acc = String.concat
              [ acc
              , leading
              , fieldName
              , " = "
              , helpMakeString (fieldValue, spaces)
              , "\n"
              ]
          in
            makeRecordString (tl, spaces, acc)
          end
      | [] => acc

    fun makeString value = helpMakeString (value, 0)
  end

  val toString = MakeString.makeString

  val value = RECORD_VALUE
    [ {fieldName = "name", fieldValue = STRING_VALUE "Thomas"}
    , {fieldName = "age", fieldValue = INT_VALUE 33}
    , { fieldName = "school"
      , fieldValue = RECORD_VALUE
          [ { fieldName = "schoolName"
            , fieldValue = STRING_VALUE "Belvoir College"
            }
          , {fieldName = "lastYear", fieldValue = INT_VALUE 13}
          ]
      }
    ]

  val valueString = MakeString.makeString value
end
