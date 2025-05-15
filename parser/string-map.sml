structure StringMap =
  MakeGapMap
    (struct
       type key = string

       fun l (a: string, b: string) = a < b
       fun eq (a: string, b: string) = a = b
       fun g (a: string, b: string) = a > b

       val maxNodeSize = 8
     end)
