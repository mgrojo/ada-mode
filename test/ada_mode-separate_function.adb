separate (Ada_Mode)
function Separate_Function return Integer is
   -- no comment before "separate"
begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Separate_Function;
