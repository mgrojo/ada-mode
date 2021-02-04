--  Test for incremental parser with error recovery.

package body Ada_Mode.Recover_Incremental is

   --EMACSCMD:(progn (end-of-line 4)(delete-char -5)(wisi-indent-statement))
   --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(current-column))
   --EMACSRESULT: 5
   function Func_1 (A : in Integer) return Float
     is (Float (A));
   --EMACSCMD:(progn (end-of-line -1)(insert "Float"))

end Ada_Mode.Recover_Incremental;
-- Local Variables:
-- wisi-incremental-parse-enable: t
-- End:
