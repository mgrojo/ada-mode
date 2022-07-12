-- Used to get "error in resume", due to current_shared_token being a
-- nonterm. Now fixed.
--EMACS_SKIP_UNLESS: wisi-incremental-parse-enable

procedure Ada_Mode.Incremental_Recover_01
is
   --EMACSCMD:(progn (end-of-line 3)(kill-line 4)(insert ";")(ada-make-subprogram-body))
   -- result verified by diff.
   procedure Proc_2 (A : in Integer)
   is begin
      null;
   end Proc_2;

   A : constant Integer := 1;
begin
   null;
end Ada_Mode.Incremental_Recover_01;
