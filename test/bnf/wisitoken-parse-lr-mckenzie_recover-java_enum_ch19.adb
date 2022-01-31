--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2019 - 2022 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Java_Enum_Ch19_Actions;
package body WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19 is

   use all type Java_Enum_Ch19_Actions.Token_Enum_ID; -- token names

   Descriptor : WisiToken.Descriptor renames Java_Enum_Ch19_Actions.Descriptor;

   procedure Matching_Begin_Tokens
     (Super                   :         in out Base.Supervisor;
      Shared_Parser           :         in out Parser.Parser;
      Tokens                  :         in     Token_ID_Array_1_3;
      Config                  : aliased in     Configuration;
      Matching_Begin_Tokens   :            out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :            out Boolean)
   is
      pragma Unreferenced (Super, Shared_Parser);
      use Java_Enum_Ch19_Actions;
      use Token_ID_Arrays;
   begin
      Forbid_Minimal_Complete := False;

      if Config.Stack.Depth = 1 and Tokens (1) = Descriptor.EOI_ID then
         --  Empty input buffer
         Matching_Begin_Tokens := To_Vector (+IDENTIFIER_ID);

      else
         case To_Token_Enum (Tokens (1)) is
         when RIGHT_CURLY_BRACKET_ID =>
            Matching_Begin_Tokens := To_Vector (+LEFT_CURLY_BRACKET_ID);
         when others =>
            Matching_Begin_Tokens := Empty_Vector;
         end case;
      end if;
   end Matching_Begin_Tokens;

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set
   is
      pragma Unreferenced (Descriptor);
   begin
      return (String_Literal_ID .. String_Literal_ID => True);
   end String_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.Java_Enum_Ch19;
