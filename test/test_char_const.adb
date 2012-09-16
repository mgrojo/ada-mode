
-- Indentation problem with parenthesis inside character constants

with Ada.Text_Io; use Ada.Text_Io;
function Test_Char_Const (Rr : in Integer) return Float is
   F : Character;
   G : String (1 .. 2) := "hh";
begin
   pragma Debug (Put_Line (G));
   F := '(';
   G := "rr";
   return 1.0;
exception
   when others =>
      pragma Debug (Put_Line (G));
      null;
      return 1.0;
end Test_Char_Const;
