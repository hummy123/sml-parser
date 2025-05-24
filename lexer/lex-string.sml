structure LexString =
struct
  (* We don't have a string DFA because we want to rewrite the string,
   * converting escape character codes to the character codes they are meant to
   * represent. *)

  fun skipFormattingChars (pos, str) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        if chr = #"\\" then
          pos
        else if chr = #"\"" then
          (* error: string closed while searching 
           * for end of format chars *)
          raise Size
        else if Char.isSpace chr then
          skipFormattingChars (pos + 1, str)
        else
          (* error: found non-formatting char *)
          raise Size
      end

  fun getEscapeString (pos, str, parsedString) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        case chr of
        (* common escape sequences *)
          #"a" => (pos, #"\a" :: parsedString)
        | #"b" => (pos, #"\b" :: parsedString)
        | #"t" => (pos, #"\t" :: parsedString)
        | #"n" => (pos, #"\n" :: parsedString)
        | #"v" => (pos, #"\v" :: parsedString)
        | #"f" => (pos, #"\f" :: parsedString)
        | #"r" => (pos, #"\r" :: parsedString)
        | #"\\" => (pos, #"\\" :: parsedString)
        | #"\"" => (pos, #"\"" :: parsedString)

        (* few different cases we have to handle:
        * 1. \ddd where there are three decimal digits,
        * 2. \uxxxx where there are four hexadecimal digits
        * 3. \f ___ f\ which contains characters that should be ignored
        *    and edxist only to format the string in source code
        * 4. control characters. *)
        | #"u" =>
            (* hex char *)
            let
              val endPos = pos + 4
              val chr1 = String.sub (str, pos + 1)
              val chr2 = String.sub (str, pos + 2)
              val chr3 = String.sub (str, pos + 3)
              val chr4 = String.sub (str, pos + 4)
            in
              if chr1 <> #"0" orelse chr2 <> #"0" then
                (* invalid hex constants: can only go up to 00FF *)
                raise Size
              else if Char.isHexDigit chr3 andalso Char.isHexDigit chr4 then
                let
                  val chr3 = Char.ord chr3 * 16
                  val chr4 = Char.ord chr4
                  val chr = Char.chr (chr3 + chr4)
                in
                  (endPos, chr :: parsedString)
                end
              else
                (* invalid hex digit *)
                raise Size
            end
        | _ =>
            let
              val chrCode = Char.ord chr
              val ctrlCode = chrCode - 64
            in
              if chrCode >= 64 andalso Char.isCntrl (Char.chr ctrlCode) then
                (* control code *)
                (pos, Char.chr ctrlCode :: parsedString)
              else if Char.isDigit chr then
                (* \ddd *)
                let
                  val str = String.substring (str, pos, 3)
                in
                  case Int.fromString str of
                    SOME x => (pos, Char.chr x :: parsedString)
                  | NONE => raise Size
                end
              else
                (* assuming we have formatting chars next *)
                (skipFormattingChars (pos + 1, str), parsedString)
            end
      end

  fun getString (pos, str, parsedString) =
    if pos = String.size str then
      (* error: unclosed string *)
      raise Size
    else
      let
        val chr = String.sub (str, pos)
      in
        case chr of
          #"\"" =>
            (* close string and return *)
            (pos, String.implode (List.rev parsedString))
        | #"\\" =>
            let
              (* either we have an escape sequence,
               * or we need to skip formatting strings *)
              val (newPos, parsedString) =
                getEscapeString (pos + 1, str, parsedString)
            in
              getString (newPos + 1, str, parsedString)
            end
        | _ => getString (pos + 1, str, chr :: parsedString)
      end
end
