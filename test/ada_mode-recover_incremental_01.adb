--  Test for incremental parser with error recovery.
--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
procedure Ada_Mode.Recover_Incremental_01 is

   --EMACSCMD:(progn (end-of-line 4)(delete-char -5)(wisi-indent-statement))
   --EMACSCMD:(progn (forward-line 3)(back-to-indentation)(current-column))
   --EMACSRESULT: 5
   function Func_1 (A : in Integer) return Float
     is (Float (A));
   --EMACSCMD:(progn (end-of-line -1)(insert "Float"))

begin
   null;
end Ada_Mode.Recover_Incremental_01;
