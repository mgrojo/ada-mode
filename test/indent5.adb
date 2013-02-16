--EMACSCMD:(setq skip-recase-test t)
with Ada.Text_Io;
procedure Indent5 is

   package Top is
      type Truc is
         record
            I : Integer;
         end record;
   end Top;

   subtype My_Int is Integer range 0 .. 31;
begin
   Ada.Text_IO.Put_Line ("hello");
end Indent5;
