--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Characters.Handling;
package body WisiToken.In_Parse_Actions is

   function Image (Item : in Status; Tree : in Syntax_Trees.Tree) return String
   is begin
      case Item.Label is
      when Ok =>
         return Status_Label'Image (Item.Label);
      when Error =>
         return '(' & Status_Label'Image (Item.Label) & ", " &
           Syntax_Trees.Image (Tree, Item.Begin_Name) & ',' &
           Syntax_Trees.Image (Tree, Item.End_Name) & ')';
      end case;
   end Image;

   function Match_Names
     (Lexer        : access constant WisiToken.Lexer.Instance'Class;
      Descriptor   : in     WisiToken.Descriptor;
      Tokens       : in     Syntax_Trees.Recover_Token_Array;
      Start_Index  : in     Positive_Index_Type;
      End_Index    : in     Positive_Index_Type;
      End_Optional : in     Boolean)
     return Status
   is
      use Syntax_Trees;
   begin
      if Contains_Virtual_Terminal (Tokens (Start_Index)) or Contains_Virtual_Terminal (Tokens (End_Index)) then
         return (Label => Ok);
      end if;

      declare
         Start_Name_Region : constant Buffer_Region := Syntax_Trees.Name (Tokens (Start_Index));
         End_Name_Region   : constant Buffer_Region := Syntax_Trees.Name (Tokens (End_Index));

         function Equal return Boolean
         is
            use Ada.Characters.Handling;
         begin
            if Descriptor.Case_Insensitive then
               return To_Lower (Lexer.Buffer_Text (Start_Name_Region)) =
                 To_Lower (Lexer.Buffer_Text (End_Name_Region));
            else
               return Lexer.Buffer_Text (Start_Name_Region) = Lexer.Buffer_Text (End_Name_Region);
            end if;
         end Equal;
      begin

         if End_Optional then
            if End_Name_Region = Null_Buffer_Region then
               return (Label => Ok);
            elsif Start_Name_Region = Null_Buffer_Region then
               return (Extra_Name_Error, Tokens (Start_Index), Tokens (End_Index));
            else
               if Equal then
                  return (Label => Ok);
               else
                  return (Match_Names_Error, Tokens (Start_Index), Tokens (End_Index));
               end if;
            end if;

         else
            if Start_Name_Region = Null_Buffer_Region then
               if End_Name_Region = Null_Buffer_Region then
                  return (Label => Ok);
               else
                  return (Extra_Name_Error, Tokens (Start_Index), Tokens (End_Index));
               end if;

            elsif End_Name_Region = Null_Buffer_Region then
               return (Missing_Name_Error, Tokens (Start_Index), Tokens (End_Index));

            else
               if Equal then
                  return (Label => Ok);
               else
                  return (Match_Names_Error, Tokens (Start_Index), Tokens (End_Index));
               end if;
            end if;
         end if;
      end;
   end Match_Names;

   function Propagate_Name
     (Nonterm    : in out Syntax_Trees.Recover_Token;
      Tokens     : in     Syntax_Trees.Recover_Token_Array;
      Name_Index : in     Positive_Index_Type)
     return Status
   is begin
      Syntax_Trees.Set_Name (Nonterm, Syntax_Trees.Name (Tokens (Name_Index)));
      return (Label => Ok);
   end Propagate_Name;

   function Merge_Names
     (Nonterm     : in out Syntax_Trees.Recover_Token;
      Tokens      : in     Syntax_Trees.Recover_Token_Array;
      First_Index : in     Positive_Index_Type;
      Last_Index  : in     Positive_Index_Type)
     return Status
   is
      use Syntax_Trees;
   begin
      Set_Name (Nonterm, Name (Tokens (First_Index)) and Name (Tokens (Last_Index)));
      return (Label => Ok);
   end Merge_Names;

   function Terminate_Partial_Parse
     (Partial_Parse_Active    : in Boolean;
      Partial_Parse_Byte_Goal : in Buffer_Pos;
      Recover_Active          : in Boolean;
      Nonterm                 : in Syntax_Trees.Recover_Token)
     return Status
   is begin
      if Partial_Parse_Active and then
        (not Recover_Active) and then
        Syntax_Trees.Byte_Region (Nonterm).Last >= Partial_Parse_Byte_Goal
      then
         raise WisiToken.Partial_Parse;
      else
         return (Label => Ok);
      end if;
   end Terminate_Partial_Parse;

end WisiToken.In_Parse_Actions;
