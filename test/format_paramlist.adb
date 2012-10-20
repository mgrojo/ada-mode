
--  Test ada-format-paramlist
--  FIXME: automate the test.
--  Spaces in the type name were incorrectly parsed

package body Format_Paramlist is

   procedure X (Y : in Z 'Class    := Default_Z;
                B    :  access Integer     ;
                A : out Integer)
   is begin
      null;
   end;

   procedure Toto (D : in Integer;
                   C : in Integer) is
   begin
      null;
   end Toto;

   function F (D : in Z 'Class;
               C : in out  Z 'Class) return Integer is
   begin
      return 0;
   end F;
end Format_Paramlist;
