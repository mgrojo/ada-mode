--  The following code is not indented correctly
--  (Works if we remove the 'package' statement)

package body Other_2 is

   procedure Q is
   begin
      declare
      begin
         null;
      end;

      if True then
         begin   --  This line used to be aligned on the first column
            null;
         exception
            when Constraint_Error => null;
         end;
      end if;
   end Q;

end Other_2;
