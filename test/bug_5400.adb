procedure Bug_5400 is
   procedure Label_Test is

      procedure Test1 is null;

      procedure Test2 is null;

   begin
      null;
   end Label_Test;

   A : Integer; -- <tab> here used to cause "matching defun has different name" due to null procedures
   pragma Unreferenced (A);
begin
   null;
end Bug_5400;
