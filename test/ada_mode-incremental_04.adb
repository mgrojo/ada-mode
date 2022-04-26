--  Used to get "tree out of byte-order" error.
--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
procedure Ada_Mode.Incremental_04
is begin
   --EMACSCMD:(progn (forward-line 3)(forward-char 6)(delete-char 26)(wisi-parse-incremental-none))
   --EMACSCMD:(progn (forward-line 2)(forward-char 6)(delete-char 1)(wisi-parse-incremental-none))
   Parse_Text
     (Label       => "1",
      Initial     => ";; comment_1",
      Incr_Errors => 1);
   --EMACSCMD:(length (wisi-parser-local-parse-errors wisi-parser-local))
   --EMACSRESULT:0
   --EMACSCMD:(progn (forward-line -4)(forward-char 6)(insert "Label       => \"1\",\n"))
   --EMACSCMD:(progn (forward-line -5)(insert "      I"))

end Ada_Mode.Incremental_04;
