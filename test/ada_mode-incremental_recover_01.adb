--  Used to get "error in resume", due to current_shared_token being a
-- nonterm. Now fixed.

procedure Ada_Mode.Incremental_Recover_01
is
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff.
   procedure Proc_2 (A : in Integer)
   is begin

   end Proc_2;

   A : constant Integer := 1;
begin
   null;
end Ada_Mode.Incremental_Recover_01;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- wisi-incremental-parse-enable: t
-- End:
