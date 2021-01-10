--  Tests for incremental parser

package body Ada_Mode.Incremental_Parse is

   -- Edit a comment, invoke incremental parse
   --EMACSCMD:(progn (end-of-line 0)(delete-char -5)(insert "parse")(wisi-indent-statement))

   --EMACSCMD:(progn (end-of-line 2)(kill-line 2)(insert "\n is (Float (A));\n")(wisi-indent-statement))
   function Func_1 (A : in Integer) return Float
     is (Float (A));

   -- Deliberately kill parser to emulate it crashing for some reason.
   -- Then request a partial parse, which causes a "file_not_found"
   -- error; error will be handled by requesting a full parse.
   --EMACSCMD:(progn (wisi-kill-parser)(wisi-reset-parser))

   --EMACSCMD:(progn (end-of-line 2)(kill-line 2)(insert "\n is (-A);\n")(wisi-indent-statement))
   function Func_2 (A : in Integer) return Integer
     is (-A);

end Ada_Mode.Incremental_Parse;
-- Local Variables:
-- wisi-incremental-parse-enable: t
-- End:
