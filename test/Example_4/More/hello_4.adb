with Hello_Pkg;
with Ada.Text_IO; use Ada.Text_IO;
procedure Hello_4
is begin
   Hello_Pkg.Say_Hello;
   Put_Line ("From hello_4");
end Hello_4;
