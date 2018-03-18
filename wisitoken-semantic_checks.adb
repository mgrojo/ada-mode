--  Abstract :
--
--  See spec.
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

pragma License (Modified_GPL);

package body WisiToken.Semantic_Checks is

   function Image (Item : in Check_Status) return String
   is begin
      case Item.Label is
      when Ok =>
         return Check_Status_Label'Image (Item.Label);
      when Error =>
         return '(' & Check_Status_Label'Image (Item.Label) & ", " &
           Semantic_Checks.Error_Code'Image (Item.Code) & ')';
      end case;
   end Image;

   function Match_Names
     (Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in Recover_Token_Array;
      Start_Index  : in Positive_Index_Type;
      End_Index    : in Positive_Index_Type;
      End_Optional : in Boolean)
     return Check_Status
   is
      use all type Recover_Token_Arrays.Vector;
   begin
      if Tokens (Start_Index).Virtual or Tokens (End_Index).Virtual then
         return (Label => Ok);

      elsif End_Optional then
         if Tokens (End_Index).Name = Null_Buffer_Region then
            return (Label => Ok);
         elsif Tokens (Start_Index).Name = Null_Buffer_Region then
            return (Error, Extra_Name_Error, Tokens (Start_Index) & Tokens (End_Index));
         else
            if Lexer.Buffer_Text (Tokens (Start_Index).Name) =
              Lexer.Buffer_Text (Tokens (End_Index).Name)
            then
               return (Label => Ok);
            else
               return (Error, Match_Names_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;
         end if;

      else
         if Tokens (Start_Index).Name = Null_Buffer_Region then
            if Tokens (End_Index).Name = Null_Buffer_Region then
               return (Label => Ok);
            else
               return (Error, Extra_Name_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;

         elsif Tokens (End_Index).Name = Null_Buffer_Region then
            return (Error, Missing_Name_Error, Tokens (Start_Index) & Tokens (End_Index));

         else
            if Lexer.Buffer_Text (Tokens (Start_Index).Name) =
              Lexer.Buffer_Text (Tokens (End_Index).Name)
            then
               return (Label => Ok);
            else
               return (Error, Match_Names_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;
         end if;
      end if;
   end Match_Names;

   function Propagate_Name
     (Nonterm    : in out Recover_Token;
      Tokens     : in     Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Check_Status
   is begin
      if Tokens (Name_Index).Name = Null_Buffer_Region then
         Nonterm.Name := Tokens (Name_Index).Byte_Region;
      else
         Nonterm.Name := Tokens (Name_Index).Name;
      end if;
      return (Label => Ok);
   end Propagate_Name;

   function Merge_Names
     (Nonterm     : in out Recover_Token;
      Tokens      : in     Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Check_Status
   is
      First_Name : Buffer_Region renames Tokens (First_Index).Name;
      Last_Name  : Buffer_Region renames Tokens (Last_Index).Name;
   begin
      Nonterm.Name :=
        First_Name and
          (if Last_Name = Null_Buffer_Region
           then Tokens (Last_Index).Byte_Region
           else Last_Name);
      return (Label => Ok);
   end Merge_Names;

end WisiToken.Semantic_Checks;
