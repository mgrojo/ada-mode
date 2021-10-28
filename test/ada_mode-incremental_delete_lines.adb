--  Test incremental parser with deleted lines; also passes with partial parse.

procedure Ada_Mode.Incremental_Delete_Lines is

   -- First edit file to pre insert/delete line state
   --EMACSCMD:(progn (end-of-line 1)(search-forward "function Func_1")(end-of-line 1)(kill-line 2)(insert "\n is begin\nreturn Float (A);\nend Func_1;\n")(wisi-indent-statement))
   --EMACSCMD:(progn (end-of-line 1)(search-forward "function Func_2")(end-of-line)(kill-line 4)(insert "\n is (-1);\n")(wisi-indent-statement))

   -- Kill parser to reset state to before insert/delete line.
   --EMACSCMD:(progn (wisi-process-parse-soft-kill wisi--parser)(wisi-reset-parser))

   -- Restore text, test incremental parse/indent
   --EMACSCMD:(progn (end-of-line 2)(kill-line 4)(insert "\n is (Float (A));\n")(wisi-indent-statement))
   function Func_1 (A : in Integer) return Float
     is (Float (A));

   --EMACSCMD:(progn (end-of-line 2)(kill-line 2)(insert "\n is begin\nreturn -1;\nend Func_2;\n")(wisi-indent-statement))
   function Func_2 return Integer
   is begin
      return -1;
   end Func_2;

begin
   null;
end Ada_Mode.Incremental_Delete_Lines;
