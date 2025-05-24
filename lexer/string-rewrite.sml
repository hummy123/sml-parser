structure StringRewrite =
struct
  (* We don't have a string DFA because we want to rewrite the string,
   * converting escape character codes to the character codes they are meant to
   * represent. *)

  type zipper = {left: char list, right: char list}

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

  fun helpSkip old =
    case old of
      #"\\" :: tl => tl
    | [] =>
        raise Fail
          "expected formatting sequence \\ ... \\ but no closing \\ found"
    | hd :: tl => helpSkip tl

  fun skipFormattingChars old =
    case old of
      #"\\" :: tl => helpSkip tl
    | _ => old

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

          | #"^" :: chr :: tl =>
              (* control code *)
              let
                val chrCode = Char.ord chr
                val cntrlCode = chrCode - 64
                val isCntrl =
                  chrCode >= 64 andalso chrCode <= 95
                  andalso Char.isCntrl (Char.chr cntrlCode)
              in
                if isCntrl then Char.chr cntrlCode :: tl
                else raise Fail "expected control code like ^H"
              end
          | chr :: tl =>
              if Char.isDigit chr then escapeThreeDigits (chr, tl) else charList
          | [] => charList
        end
    | _ => charList

  fun moveOldHeadToNewHead (new, oldHd :: oldTl) = (oldHd :: new, oldTl)
    | moveOldHeadToNewHead (new, []) = (new, [])

  fun rewriteEscapeChars (new, old) =
    case old of
      #"\\" :: tl =>
        let
          val old = escapeFrontChars old
          val old = skipFormattingChars old
          val (new, old) = moveOldHeadToNewHead (new, old)
        in
          rewriteEscapeChars (new, old)
        end
    | hd :: tl => rewriteEscapeChars (hd :: new, tl)
    | [] => String.implode (List.rev new)

  fun loop (pos, str, acc) =
    if pos < 0 then
      raise Fail "unclosed string"
    else
      let
        val chr = String.sub (str, pos)
      in
        case chr of
          #"\"" => let val str = rewriteEscapeChars ([], acc) in (pos, str) end
        | _ => loop (pos - 1, str, chr :: acc)
      end

  fun rewrite (pos, str) = loop (pos, str, [])
end
