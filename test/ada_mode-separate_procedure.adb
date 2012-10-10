separate (Ada_Mode)
procedure Separate_Procedure is
   --  Broken in an early version of Ada Mode 5.0.
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Procedure;
