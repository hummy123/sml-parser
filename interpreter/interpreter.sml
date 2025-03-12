structure Interpreter =
struct
  open RuntimeValue

  structure SymbolTable =
    MakeGapMap
      (struct
         type key = string
         type value = RuntimeValue.runtime_value

         fun l (a: string, b: string) = a < b
         fun eq (a: string, b: string) = a = b
         fun g (a: string, b: string) = a > b

         val maxNodeSize = 9
       end)
end
