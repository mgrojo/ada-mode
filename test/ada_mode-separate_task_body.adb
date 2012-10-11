separate (Ada_Mode)
task body Separate_Task_Body is
   -- no comment before "separate"
begin
   begin  -- should be indented.
      null;
   end;
end Separate_Task_Body;
