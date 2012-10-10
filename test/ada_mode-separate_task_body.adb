separate (Ada_Mode)
task body Separate_Task_Body is
   --  Broken in an early version of Ada Mode 5.0.
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Task_Body;
