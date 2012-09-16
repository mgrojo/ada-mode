--  Check the indentation of comments



--  7724-003
--  A comment was incorrectly indented when a string on the previous string
--  contains some dashes

with Ada.Text_Io; use Ada.Text_Io;
procedure Comments is

   procedure Foo is
   begin
      Put_Line ("-----");
      --  This comment was aligned on the first '-' above
   end Foo;
begin
   null;
end Comments;
