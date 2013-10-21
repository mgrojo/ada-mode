with Hello_Pkg;
with Ada.Text_IO; use Ada.Text_IO;
procedure Hello_3
is begin
   Hello_Pkg.Say_Hello;
   Put_Line ("From hello_3");
end Hello_3;
