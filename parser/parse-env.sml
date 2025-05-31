structure ParseEnv =
struct
  structure StringMap =
    MakeGapMap
      (struct
         type key = string

         fun l (a: string, b: string) = a < b
         fun eq (a: string, b: string) = a = b
         fun g (a: string, b: string) = a > b

         val maxNodeSize = 8
       end)

  structure StringSet =
    MakeGapSet
      (struct
         type key = string

         fun l (a: string, b: string) = a < b
         fun eq (a: string, b: string) = a = b
         fun g (a: string, b: string) = a > b

         val maxNodeSize = 8
       end)

  type infix_record = {isLeft: bool, power: int}

  type env = {infixMap: infix_record StringMap.t, constructor: StringSet.t}

  fun addInfix (key, record: infix_record, env) =
    let
      val {infixMap, constructor} = env
      val infixMap = StringMap.add (key, record, infixMap)
    in
      {infixMap = infixMap, constructor = constructor}
    end

  fun getInfix (key, {infixMap, constructor}) = StringMap.get (key, infixMap)

  fun isInfix (key, {infixMap, constructor = _}) =
    StringMap.get (key, infixMap) <> NONE

  fun addConstructor (key, {constructor, infixMap}) =
    let val constructor = StringSet.add (key, constructor)
    in {constructor = constructor, infixMap = infixMap}
    end

  fun isConstructor (key, {constructor, infixMap = _}) =
    StringSet.exists (key, constructor)

  fun removeConstructor (key, {constructor, infixMap}) =
    let val constructor = StringSet.remove (key, constructor)
    in {constructor = constructor, infixMap = infixMap}
    end
end
