--
--  Indentation problem in ada-mode 3.2
--  Indentation problem after the instanciation of a new package
--

with Ada.Text_Io;
procedure Indent2 is
   package B is
     new Ada.Text_Io.Integer_Io (Num => Integer);  --  The problem appears because of =>

   procedure F (G : in out Integer); -- <<<<<< wrongly indented

   procedure F (G : in out Integer) is begin null; end F;
begin
   null;
end Indent2;
