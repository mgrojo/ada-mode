-- Used to get "error during resume" with incremental parse.
--
-- This is extracted from ada_mode-interactive_2.adb, extended.
procedure Ada_Mode.Interactive_03
is

   --EMACSCMD:(progn (end-of-line 6)(kill-line 4)(newline-and-indent))
   --EMACSCMD:(progn (forward-line 5)(execute-kbd-macro "is begin\nreturn 1.0;\nend Function_Access_1;\n")(current-indentation))
   --EMACSRESULT:3
   function Function_Access_1
     (A_Param : in Float)
     return Standard.Float
   is begin
      return 1.0;
   end Function_Access_1;

   -- There should be no syntax errors at this point. Note that in
   -- incremental parse, there are still virtual tokens in the tree.
   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT: 0
begin
   null;

   -- Insert text at end of buffer; no errors.
   --EMACSCMD:(progn (forward-line 3)(kill-line 4)(wisi-indent-statement))
   --EMACSCMD:(progn (forward-line 2)(execute-kbd-macro "procedure Foo is begin\nnull;\nend Foo;\n")(wisi-indent-statement))
end Ada_Mode.Interactive_03;
procedure Foo is begin
   null;
end Foo;
