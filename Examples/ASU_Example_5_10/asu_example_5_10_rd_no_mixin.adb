-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephe Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

with Ada.Tags;
with Ada.Text_IO;
package body ASU_Example_5_10_RD_No_Mixin is

   overriding function "or"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
      return Integer_Selection_Instance
   is
      use type Selection.Instance;
   begin
      return (Selection.Instance'(Left or Right) with Value => 0, Name => null);
   end "or";

   overriding function "or"
     (Left  : access OpenToken.Token.Class;
      Right : in     Integer_Selection_Instance)
      return Integer_Selection_Instance
   is
      use type Selection.Instance;
   begin
      return (Selection.Instance'(Left or Selection.Instance (Right)) with Value => 0, Name => null);
   end "or";

   overriding function "or"
     (Left  : in     Integer_Selection_Instance;
      Right : access OpenToken.Token.Class)
      return Integer_Selection_Instance
   is
      use type Selection.Instance;
   begin
      return (Selection.Instance'(Selection.Instance (Left) or Right) with Value => 0, Name => null);
   end "or";

   overriding function "or"
     (Left  : in Integer_Selection_Instance;
      Right : in Integer_Selection_Instance)
      return Integer_Selection_Instance
   is
      use type Selection.Instance;
   begin
      return
        (Selection.Instance'(Selection.Instance (Left) or Selection.Instance (Right)) with Value => 0, Name => null);
   end "or";

   function New_Integer_Selection_Instance
     (Old_Instance : in Integer_Selection_Instance)
      return Integer_Selection_Handle
   is begin
      return new Integer_Selection_Instance'Class'(Integer_Selection_Instance'Class (Old_Instance));
   end New_Integer_Selection_Instance;

   overriding procedure Build
     (Match : in out Integer_Selection_Instance;
      From  : in     OpenToken.Token.Instance'Class)
   is
      use type Ada.Tags.Tag;
   begin
      if From'Tag = Integer_Token'Tag then
         Match.Value := Integer_Token (From).Value;

      elsif From'Tag = Integer_Literal.Instance'Tag then
         Match.Value := Integer_Literal.Value (Integer_Literal.Instance (From));

      else
         raise OpenToken.Parse_Error with "unexpected token " &
           Token_IDs'Image (Master_Token.ID (Master_Token.Instance (From)));
      end if;
   end Build;

   overriding function Name (Item : in Integer_Selection_Instance) return String
   is begin
      return Item.Name.all;
   end Name;

   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
      return Expression_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Left & Right) with Value => 0, Name => null);
   end "&";

   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Expression_Instance)
      return Expression_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Left & Sequence.Instance (Right)) with Value => 0, Name => null);
   end "&";

   overriding function "&"
     (Left  : in     Expression_Instance;
      Right : access OpenToken.Token.Class)
      return Expression_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Sequence.Instance (Left) & Right) with Value => 0, Name => null);
   end "&";

   overriding function "&"
     (Left  : in Expression_Instance;
      Right : in Expression_Instance)
      return Expression_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Sequence.Instance (Left) & Sequence.Instance (Right)) with Value => 0, Name => null);
   end "&";

   function New_Expression_Instance (Name : in String; Item : in Expression_Instance) return Expression_Handle
   is begin
      return new Expression_Instance'(Sequence.Instance (Item) with Value => 0, Name => new String'(Name));
   end New_Expression_Instance;

   overriding procedure Build
     (Match : in out Expression_Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;
      use type Ada.Tags.Tag;

      I      : List_Iterator          := Initial_Iterator (Using); -- E, T, or (
      Left   : constant List_Iterator := I;
      Middle : List_Iterator;
   begin
      Next_Token (I); -- +, *, or E
      Middle := I;
      Next_Token (I); -- T, F, or )

      if Token_Handle (Middle)'Tag = Master_Token.Instance'Tag then
         case Master_Token.ID (Master_Token.Instance (Token_Handle (Middle).all)) is
         when Plus_Sign_ID =>
            declare
               --  E + T
               Left_Token  : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (Left).all);
               Right_Token : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (I).all);
            begin
               Match.Value := Left_Token.Value + Right_Token.Value;
            end;

         when Multiply_ID =>
            declare
               --  T * F
               Left_Token  : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (Left).all);
               Right_Token : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (I).all);
            begin
               Match.Value := Left_Token.Value * Right_Token.Value;
            end;

         when others =>
            raise OpenToken.Parse_Error with "unexpected token " &
              Token_IDs'Image (Master_Token.ID (Master_Token.Instance (Token_Handle (Middle).all)));
         end case;

      elsif Token_Handle (Middle)'Tag = Expression_Instance'Tag then
         declare
            --  ( E )
            Middle_Token : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (Middle).all);
         begin
            Match.Value := Middle_Token.Value;
         end;

      else
         raise OpenToken.Parse_Error with "unexpected token " & Ada.Tags.Expanded_Name (Token_Handle (Middle)'Tag);
      end if;

   end Build;

   overriding function Name (Item : in Expression_Instance) return String
   is begin
      return Item.Name.all;
   end Name;

   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : access OpenToken.Token.Class)
      return Result_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Left & Right) with null record);
   end "&";

   overriding function "&"
     (Left  : access OpenToken.Token.Class;
      Right : in     Result_Instance)
      return Result_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Left & Sequence.Instance (Right)) with null record);
   end "&";

   overriding function "&"
     (Left  : in     Result_Instance;
      Right : access OpenToken.Token.Class)
      return Result_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Sequence.Instance (Left) & Right) with null record);
   end "&";

   overriding function "&"
     (Left  : in Result_Instance;
      Right : in Result_Instance)
      return Result_Instance
   is
      use type Sequence.Instance;
   begin
      return (Sequence.Instance'(Sequence.Instance (Left) & Sequence.Instance (Right)) with null record);
   end "&";

   function New_Result_Instance
     (Item : in Result_Instance)
      return Result_Handle
   is begin
      return new Result_Instance'Class'(Result_Instance'Class (Item));
   end New_Result_Instance;

   overriding procedure Build
     (Match : in out Result_Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      pragma Unreferenced (Match);
      use OpenToken.Token.Linked_List;
      I          : constant List_Iterator := Initial_Iterator (Using); -- E
      Left_Token : Integer_Selection_Instance renames Integer_Selection_Instance (Token_Handle (I).all);
   begin
      Ada.Text_IO.Put_Line (Integer'Image (Left_Token.Value));
   end Build;

end ASU_Example_5_10_RD_No_Mixin;
