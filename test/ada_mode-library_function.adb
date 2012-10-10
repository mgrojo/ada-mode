function Ada_Mode.Library_Function return Integer is
   --  Broken in an early version of Ada Mode 5.0.
begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Ada_Mode.Library_Function;
