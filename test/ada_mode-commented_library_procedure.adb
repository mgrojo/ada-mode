--  Broken in an early version of Ada Mode 5.0.
procedure Ada_Mode.Commented_Library_Procedure is
   --  Broken in a slightly later version of Ada Mode 5.0 if the
   --  leading comment is omitted.
begin
   begin  -- should be indented.
      null;
   end;
end Ada_Mode.Commented_Library_Procedure;
