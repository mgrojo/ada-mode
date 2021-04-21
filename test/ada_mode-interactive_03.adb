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

   -- There should be no syntax errors and no virtual terminals at this point
   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT: 0
   --EMACSCMD:(if wisi-incremental-parse-enable (length (wisi-parse-tree-query wisi--parser 'virtuals 1)) 0)
   --EMACSRESULT: 0
begin
   null;

   -- Insert text at end of buffer; no errors.
   --EMACSCMD:(progn (forward-line 3)(execute-kbd-macro "procedure Foo is begin\nnull;\nend Foo;\n"))
   --EMACSCMD:(progn (forward-line 2)(kill-line 4))
end Ada_Mode.Interactive_03;
