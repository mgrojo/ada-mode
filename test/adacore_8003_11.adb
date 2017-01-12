
--  Indentation of the 'end' keyword
--  See 8003-011

package body Adacore_8003_11 is
   package body Communication is
   begin
      null;
   end Communication;

   package Internal_Types is
      function Foo return Integer;
   end Internal_Types;

   package body Internal_Types is
      function Foo return Integer
      is begin
         return 0;
      end Foo;

   end Internal_Types;

   --  'is separate' used to cause problems; tested elsewhere.
   --  procedure Aot is separate;

end Adacore_8003_11;   --  Indented same level as package body Communication
