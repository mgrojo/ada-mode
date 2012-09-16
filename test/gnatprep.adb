
--  Controls the way gnatprep statements are highlighted.
--  We also test other usage of the '#' sign, especially in
--  based numbers or in strings.

procedure Gnatprep is

begin
   if A = 1 then
#if Gnat_Compiler
      A := 1;
#elsif Other_Compiler
      A := 2;
#else
      B := 3;
#end if;
      A := 3;

   end if;

   A := "in a # string, should work too";
   A := 2;

   declare
      Control_Flags : Integer;
      for Control_Flags use at (BASE_ADDRESS + 16#3fd#);
      --  The color was incorrect above with adamode3.3.

      --  Don't change the hex literal to mixed case
      Other_Flags : constant Interfaces.Unsigned_16 := 16#ffff#;
   begin
   end;

end Gnatprep;
