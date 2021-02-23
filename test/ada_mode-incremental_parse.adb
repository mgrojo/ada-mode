--  Initial test for incremental parser

procedure Ada_Mode.Incremental_Parse
is

   -- Edit a comment twice, invoke incremental parse
   --EMACSCMD:(progn (beginning-of-line 0)(forward-word 3)(delete-char 6)(insert " twice")(forward-word 2)(delete-char 6)(insert " parse")(wisi-indent-statement))

   --EMACSCMD:(progn (end-of-line 2)(kill-line 2)(insert "\n is (Float (A));\n")(wisi-indent-statement))
   function Func_1 (A : in Integer) return Float
     is (Float (A));

   -- Deliberately kill parser to emulate it crashing for some reason.
   -- Then request a partial parse, which causes a "file_not_found"
   -- error; error will be handled by requesting a full parse.
   --EMACSCMD:(progn (wisi-process-parse-soft-kill wisi--parser)(wisi-reset-parser))

   --EMACSCMD:(progn (end-of-line 3)(kill-line 2)(insert "\n is (-1);\n"))
   --EMACSCMD:(progn (end-of-line 3)(wisi-indent-statement))
   function Func_2 return Integer
     is (-1);

begin
   null;
end Ada_Mode.Incremental_Parse;
-- Local Variables:
-- wisi-incremental-parse-enable: t
-- End:
