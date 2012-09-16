with Text_Io; use Text_Io;
procedure Adacore_9907_010 is
   function F (X : Integer) return String
   is
   begin
      return "world";
   end;
begin
   Put_Line ("hello " &
               "world" &
               " in ada");
   Put_Line ("hello "
               & "world"
               & " in ada");
   Put_Line ("hello " &
               F(1) &
               " in ada");
   Put_Line ("hello "
               & F(1)
               & " in ada");
end;
