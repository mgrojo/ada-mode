--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

pragma License (GPL);

package body WisiToken.Gen_Token_Enum is

   function Token_Enum_Image return Token_ID_Array_String
   is
      Result : Token_ID_Array_String (Token_ID'First .. +Last_Nonterminal);
   begin
      for I in Token_Enum_ID loop
         Result (+I) := new String'(Token_Enum_ID'Image (I));
      end loop;
      return Result;
   end Token_Enum_Image;

   function To_Syntax (Item : in Enum_Syntax) return WisiToken.Lexer.Regexp.Syntax
   is
      Result : WisiToken.Lexer.Regexp.Syntax (Token_ID'First .. +Last_Terminal);
   begin
      for I in Result'Range loop
         Result (I) := Item (-I);
      end loop;
      return Result;
   end To_Syntax;

   function "&" (Left, Right : in Token_Enum_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List do
         Result.Append (+Left);
         Result.Append (+Right);
      end return;
   end "&";

   function "&"
     (Left  : in Token_ID_Lists.List;
      Right : in Token_Enum_ID)
     return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List := Left do
         Result.Append (+Right);
      end return;
   end "&";

   function "+"
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Syntax_Trees.Semantic_Action)
     return WisiToken.Production.Right_Hand_Side
   is begin
      return WisiToken.Production."+" (+Left, Right);
   end "+";

   function "<="
     (Left  : in Token_Enum_ID;
      Right : in WisiToken.Production.Right_Hand_Side)
     return WisiToken.Production.Instance
   is begin
      return WisiToken.Production."<=" (+Left, Right);
   end "<=";

   function To_Nonterminal_Array_Token_Set
     (Item : in Nonterminal_Array_Token_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Nonterminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Token_Set;

   function To_Nonterminal_Array_Terminal_Set
     (Item : in Nonterminal_Array_Terminal_Set)
     return WisiToken.Token_Array_Token_Set
   is
      Result : Token_Array_Token_Set :=
        (LR1_Descriptor.First_Nonterminal .. LR1_Descriptor.Last_Nonterminal =>
           (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False));
   begin
      for I in Item'Range (1) loop
         for J in Item'Range (2) loop
            Result (+I, +J) := Item (I, J);
         end loop;
      end loop;
      return Result;
   end To_Nonterminal_Array_Terminal_Set;

   function "+" (Item : in Token_Array) return WisiToken.Token_ID_Set
   is
      Result : Token_ID_Set := (LR1_Descriptor.First_Terminal .. LR1_Descriptor.Last_Terminal => False);
   begin
      for I in Item'Range loop
         Result (+Item (I)) := True;
      end loop;
      return Result;
   end "+";

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID_Set
   is begin
      return +Token_Array'(1 => Item);
   end "+";

end WisiToken.Gen_Token_Enum;
