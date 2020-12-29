--  Tests for incremental parser

package body Ada_Mode.Incremental_Parse is

   --EMACSCMD:(progn (end-of-line 4)(kill-line 2)(insert ";"))
   --EMACSCMD:(progn (font-lock-ensure (point-min) (point-max)) (length (wisi-parser-parse-errors wisi--parser)))
   --EMACSRESULT: 0
   function Func_1 (A : in Integer) return Float
     is (Float (A));

   --EMACSCMD:(progn (end-of-line 0)(delete-char -1)(insert "\n     is (Float (A));\n"))

end Ada_Mode.Incremental_Parse;
-- Local Variables:
-- wisi-incremental-parse-enable: t
-- End:
