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

   function Image (Item : in Error_Label_Set) return String
   is
      use Ada.Strings.Unbounded;
      Result     : Unbounded_String := +"(";
      Need_Comma : Boolean          := False;
   begin
      for I in Item'Range loop
         if Item (I) then
            Result := Result &
              (if Need_Comma then ", " else "") &
              Error_Label'Image (I);
            Need_Comma := True;
         end if;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   function Image (Item : in Check_Status) return String
   is begin
      case Item.Label is
      when Ok =>
         return Check_Status_Label'Image (Item.Label);
      when Error =>
         return '(' & Check_Status_Label'Image (Item.Label) & ", " &
           Semantic_Checks.Error_Label'Image (Item.Code) & ')';
      end case;
   end Image;

   function Match_Names
     (Syntax_Tree  : in WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Lexer        : in WisiToken.Lexer.Handle;
      Tokens       : in WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      Start_Index  : in Ada.Containers.Count_Type;
      End_Index    : in Ada.Containers.Count_Type;
      End_Optional : in Boolean)
     return Check_Status
   is
      use all type Syntax_Trees.Valid_Node_Index_Arrays.Vector;
   begin
      if Syntax_Tree.Virtual (Tokens (Start_Index)) or Syntax_Tree.Virtual (Tokens (End_Index)) then
         return (Label => Ok);

      elsif End_Optional then
         if Syntax_Tree.Name_Region (Tokens (End_Index)) = Null_Buffer_Region then
            return (Label => Ok);
         elsif Syntax_Tree.Name_Region (Tokens (Start_Index)) = Null_Buffer_Region then
            return (Error, Extra_Name_Error, Tokens (Start_Index) & Tokens (End_Index));
         else
            if Lexer.Buffer_Text (Syntax_Tree.Name_Region (Tokens (Start_Index))) =
              Lexer.Buffer_Text (Syntax_Tree.Name_Region (Tokens (End_Index)))
            then
               return (Label => Ok);
            else
               return (Error, Match_Names_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;
         end if;

      else
         if Syntax_Tree.Name_Region (Tokens (Start_Index)) = Null_Buffer_Region then
            if Syntax_Tree.Name_Region (Tokens (End_Index)) = Null_Buffer_Region then
               return (Label => Ok);
            else
               return (Error, Extra_Name_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;

         elsif Syntax_Tree.Name_Region (Tokens (End_Index)) = Null_Buffer_Region then
            return (Error, Missing_Name_Error, Tokens (Start_Index) & Tokens (End_Index));

         else
            if Lexer.Buffer_Text (Syntax_Tree.Name_Region (Tokens (Start_Index))) =
              Lexer.Buffer_Text (Syntax_Tree.Name_Region (Tokens (End_Index)))
            then
               return (Label => Ok);
            else
               return (Error, Match_Names_Error, Tokens (Start_Index) & Tokens (End_Index));
            end if;
         end if;
      end if;
   end Match_Names;

   function Propagate_Name
     (Syntax_Tree : in out WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Nonterm     : in     WisiToken.Syntax_Trees.Valid_Node_Index;
      Tokens      : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      Name_Index  : in     Ada.Containers.Count_Type)
     return Check_Status
   is begin
      Syntax_Tree.Set_Name_Region (Nonterm, Syntax_Tree.Name_Region (Tokens (Name_Index)));
      return (Label => Ok);
   end Propagate_Name;

   function Merge_Names
     (Syntax_Tree : in out WisiToken.Syntax_Trees.Abstract_Tree'Class;
      Nonterm     : in     WisiToken.Syntax_Trees.Valid_Node_Index;
      Tokens      : in     WisiToken.Syntax_Trees.Valid_Node_Index_Array;
      First_Index : in     Ada.Containers.Count_Type;
      Last_Index  : in     Ada.Containers.Count_Type)
     return Check_Status
   is
      First_Name_Region : Buffer_Region renames Syntax_Tree.Name_Region (Tokens (First_Index));
      Last_Name_Region  : Buffer_Region renames Syntax_Tree.Name_Region (Tokens (Last_Index));
   begin
      Syntax_Tree.Set_Name_Region
        (Nonterm,
         First_Name_Region and
           (if Last_Name_Region = Null_Buffer_Region
            then Syntax_Tree.Byte_Region (Tokens (Last_Index))
            else Last_Name_Region));
      return (Label => Ok);
   end Merge_Names;

end WisiToken.Semantic_Checks;
