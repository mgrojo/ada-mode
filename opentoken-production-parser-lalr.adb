with Ada.Strings.Fixed;
package body OpenToken.Production.Parser.LALR is

   function State_Image (Item : in State_Index) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (State_Index'Image (Item), Both);
   end State_Image;

end OpenToken.Production.Parser.LALR;
