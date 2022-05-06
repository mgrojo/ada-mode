-- Used to leave a syntax error.
procedure Ada_Mode.Incremental_String_Quote_01
is
begin
   --EMACSCMD:(progn (end-of-line 3)(forward-char -1)(wisi-replay-kbd-macro "(\"foo \"\"bar\"\" \")")(length (wisi-parser-local-parse-errors wisi-parser-local)))
   --EMACSRESULT: 0
   Put_Line;
   --EMACSCMD:(progn (end-of-line 0)(forward-char -1)(delete-char -16))
end Ada_Mode.Incremental_String_Quote_01;
