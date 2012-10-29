separate (Ada_Mode)
procedure Separate_Procedure is
   -- no comment before "separate"
   --EMACSCMD:(progn (forward-line -3)(ada-find-other-file t)(looking-at "procedure Separate_Procedure is"))
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Procedure;
