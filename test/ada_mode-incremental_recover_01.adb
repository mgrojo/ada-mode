--  Used to get "error in resume"

procedure Ada_Mode.Interactive_1
is
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff.
   procedure Proc_2 (A : in Integer)
   is begin

   end Proc_2;

   A : constant integer := 1;
begin
   null;
end Ada_Mode.Interactive_1;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-incremental-parse-enable: t
-- End:
