structure StringRewrite =
struct
  (* We don't have a string DFA because we want to rewrite the string,
   * converting escape character codes to the character codes they are meant to
   * represent. *)

  fun hexCharToNum chr =
    case Char.toLower chr of
      #"0" => 0
    | #"1" => 1
    | #"2" => 2
    | #"3" => 3
    | #"4" => 4
    | #"5" => 5
    | #"6" => 6
    | #"7" => 7
    | #"8" => 8
    | #"9" => 9
    | #"a" => 10
    | #"b" => 11
    | #"c" => 12
    | #"d" => 13
    | #"e" => 14
    | #"f" => 15
    | _ => raise Fail "expected hex digit"

  fun escapeThreeDigits (c1, tl) =
    (* test if we have three digits \ddd.
     * If we don't, then leave charList alone and just return it. *)
    case tl of
      c2 :: c3 :: tl =>
        if Char.isDigit c2 andalso Char.isDigit c3 then
          let
            val chr =
              hexCharToNum c1 * 100 + hexCharToNum c2 * 10 + hexCharToNum c3
            val chr = Char.chr chr
          in
            chr :: tl
          end
        else
          raise Fail "expected \\ddd where each d is a digit from 0 to 9"
    | _ => raise Fail "expected \\ddd where each d is a digit from 0 to 9"

  fun escapeFrontChars charList =
    case charList of
      #"\\" :: tl =>
        let in
          case tl of
          (* common escape sequences *)
            #"a" :: tl => #"\a" :: tl
          | #"b" :: tl => #"\b" :: tl
          | #"t" :: tl => #"\t" :: tl
          | #"n" :: tl => #"\n" :: tl
          | #"v" :: tl => #"\v" :: tl
          | #"f" :: tl => #"\f" :: tl
          | #"r" :: tl => #"\r" :: tl
          | #"\\" :: tl => #"\\" :: tl
          | #"\"" :: tl => #"\"" :: tl

          (* few different cases we have to handle:
          * 1. \ddd where each 'd' is a decimal digit,
          * 2. \uxxxx where there are four hexadecimal digits
          * 3. \f ___ f\ which contains characters that should be ignored
          *    and edxist only to format the string in source code
          * 4. control characters. *)

          (* \uxxxx where x is one of four hexadecimal digits *)
          | #"u" :: #"0" :: #"0" :: c3 :: c4 :: tl =>
              if Char.isHexDigit c3 andalso Char.isHexDigit c4 then
                let
                  val c3 = hexCharToNum c3 * 16
                  val c4 = hexCharToNum c4
                  val chr = chr (c3 + c4)
                in
                  chr :: tl
                end
              else
                raise Fail
                  "expected \\u escape sequence to be betwen \\u0000 and \\u00FF"
          | #"u" :: tl =>
              raise Fail
                "expected \\u escape sequence to be betwen \\u0000 and \\u00FF"

          | chr :: tl =>
              let
                val chrCode = Char.ord chr
                val ctrlCode = chrCode - 64
              in
                if chrCode >= 64 andalso Char.isCntrl (Char.chr ctrlCode) then
                  (* get control code if there is one *)
                  Char.chr ctrlCode :: tl
                else if Char.isDigit chr then
                  (* get digit as in \ddd if possible *)
                  escapeThreeDigits (chr, tl)
                else
                  charList
              end
          | _ => charList
        end
    | _ => charList

  fun helpSkip (pos, str) =
    if pos = String.size str then
      raise Fail
        "error: expected \\ to close formatting chars but got unclosed string"
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\\" then
          pos
        else if chr = #"\"" then
          raise Fail
            "expected matching \\ to close formatting chars\
            \but string closed before matching \\ was found"
        else if Char.isSpace chr then
          helpSkip (pos - 1, str)
        else
          (* error: found non-formatting char *)
          raise Fail
            ("expected formatting char but found " ^ String.implode [chr])
      end

  fun skipFormattingChars (pos, str, acc) =
    case acc of
      #"\\" :: _ => helpSkip (pos - 1, str)
    | _ => pos

  fun loop (pos, str, acc) =
    if pos < 0 then
      raise Fail "unclosed string"
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\"" then
          (pos, String.implode acc)
        else
          let
            val acc = escapeFrontChars (chr :: acc)
            val pos = skipFormattingChars (pos, str, acc)
          in
            loop (pos - 1, str, acc)
          end
      end

  fun rewrite (pos, str) = loop (pos, str, [])
end
