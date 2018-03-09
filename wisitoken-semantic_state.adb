--  Abstract :
--
--  see spec
--
--
--  Copyright (C) 2009, 2014, 2015, 2017, 2018 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body WisiToken.Semantic_State is

   ----------
   --  Public subprograms, declaration order

   overriding
   function Image
     (Item       : in Augmented_Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean := False)
     return String
   is
      use all type Ada.Text_IO.Count;
      ID_Image : constant String := WisiToken.Image (Item.ID, Descriptor);
   begin
      if ID_Only then
         --  No parens for consistency with previous unit test results.
         return ID_Image;

      elsif Item.Line /= Invalid_Line_Number and Trace_Action <= Detail then
         return "(" & ID_Image &
           Line_Number_Type'Image (Item.Line) & ":" & Int_Image (Integer (Item.Col)) & ")";

      elsif Item.Char_Region = Null_Buffer_Region then
         return "(" & ID_Image & ")";

      else
         return "(" & ID_Image & ", " & Image (Item.Char_Region) & ")";
      end if;
   end Image;

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.First_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Line
            else Token.First_Trailing_Comment_Line)
         else
           (if Token.First_Indent_Line = Invalid_Line_Number
            then Token.Line
            else Token.First_Indent_Line));
   end First_Line;

   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.Last_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Line
            else Token.Last_Trailing_Comment_Line)
         else
           (if Token.Last_Indent_Line = Invalid_Line_Number
            then Token.Line
            else Token.Last_Indent_Line));
   end Last_Line;

   function Image
     (Item       : in Augmented_Token_Access_Array;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is
      use all type SAL.Base_Peek_Type;
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"(";
   begin
      for I in Item'Range loop
         Result := Result & Image (Item (I).all, Descriptor);
         if I /= Item'Last then
            Result := Result & ", ";
         end if;
      end loop;
      Result := Result & ")";
      return -Result;
   end Image;

   ----------
   --  Semantic_State

   function Find
     (State : in Semantic_State;
      ID    : in Token_ID;
      Token : in Augmented_Token'Class)
     return Base_Token_Index
   is begin
      --  linear search for ID.
      for I in Token.First_Terminals_Index .. Token.Last_Terminals_Index loop
         if State.Terminals (I).ID = ID then
            return I;
         end if;
      end loop;
      return Augmented_Token_Arrays.No_Index;
   end Find;

   procedure Initialize
     (State      : in out Semantic_State;
      Line_Count : in     Line_Number_Type)
   is begin
      --  + 1 for data on line following last line; see Lexer_To_Augmented.
      State.Line_Begin_Pos.Set_Length (Ada.Containers.Count_Type (Line_Count + 1), Default => Invalid_Buffer_Pos);
      State.Line_Begin_Token.Set_Length
        (Ada.Containers.Count_Type (Line_Count + 1), Default => Augmented_Token_Arrays.No_Index);
      State.Line_Paren_State.Set_Length (Ada.Containers.Count_Type (Line_Count + 1));

      State.Reset;
   end Initialize;

   procedure Reset (State : in out Semantic_State)
   is begin
      State.Terminals.Clear;

      for S of State.Line_Paren_State loop
         S := 0;
      end loop;
      State.Current_Paren_State := 0;
   end Reset;

   procedure Lexer_To_Augmented
     (State      : in out          Semantic_State;
      Descriptor : in              WisiToken.Descriptor'Class;
      Token      : in              Base_Token;
      Lexer      : not null access WisiToken.Lexer.Instance'Class)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

   begin
      if Lexer.First then
         State.Line_Begin_Pos (Lexer.Line) := Lexer.Char_Region.First;
      end if;

      if Token.ID < Descriptor.First_Terminal then
         --  Non-grammar token

         if Token.ID = Descriptor.New_Line_ID then
            State.Line_Paren_State (Lexer.Line + 1) := State.Current_Paren_State;
         end if;

         if State.Terminals.Length = 0 then
            State.Leading_Non_Grammar.Append (Token);
         else
            declare
               Containing_Token : Augmented_Token renames State.Terminals (State.Terminals.Last_Index);

               Trailing_Blank : constant Boolean :=
                 Token.ID = Descriptor.New_Line_ID and
                 (Containing_Token.Non_Grammar.Length > 0 and then
                    Containing_Token.Non_Grammar (Containing_Token.Non_Grammar.Last_Index).ID = Descriptor.New_Line_ID);
            begin
               Containing_Token.First := Containing_Token.First or
                 (Lexer.First and (Token.ID = Descriptor.Comment_ID or Trailing_Blank));

               Containing_Token.Non_Grammar.Append ((Token.ID, Lexer.Line, Lexer.Column, Lexer.First));

               if Lexer.First then
                  State.Line_Begin_Token (Lexer.Line) := State.Terminals.Last_Index;
               end if;
            end;
         end if;

      else
         --  grammar token
         declare
            Temp : constant Augmented_Token :=
              (Token.ID,
               Byte_Region                 => Lexer.Byte_Region,
               Line                        => Lexer.Line,
               Col                         => Lexer.Column,
               Char_Region                 => Lexer.Char_Region,
               First                       => Lexer.First,
               Paren_State                 => State.Current_Paren_State,
               First_Terminals_Index       => State.Terminals.Last_Index + 1,
               Last_Terminals_Index        => State.Terminals.Last_Index + 1,
               First_Indent_Line           => (if Lexer.First then Lexer.Line else Invalid_Line_Number),
               Last_Indent_Line            => (if Lexer.First then Lexer.Line else Invalid_Line_Number),
               First_Trailing_Comment_Line => Invalid_Line_Number, -- Set by Reduce
               Last_Trailing_Comment_Line  => Invalid_Line_Number,
               Non_Grammar                 => <>);
         begin
            if Token.ID = Descriptor.Left_Paren_ID then
               State.Current_Paren_State := State.Current_Paren_State + 1;

            elsif Token.ID = Descriptor.Right_Paren_ID then
               State.Current_Paren_State := State.Current_Paren_State - 1;
            end if;

            State.Terminals.Append (Temp);

            if Lexer.First then
               State.Line_Begin_Token (Lexer.Line) := State.Terminals.Last_Index;
            end if;
         end;
      end if;

   end Lexer_To_Augmented;

   function First_Comment_Line
     (Tokens     : in Non_Grammar_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return Line_Number_Type
   is begin
      for Token of Tokens loop
         if Token.First and Token.ID = Descriptor.Comment_ID then
            return Token.Line;
         end if;
      end loop;
      return Invalid_Line_Number;
   end First_Comment_Line;

   function Last_Comment_Line
     (Tokens     : in Non_Grammar_Token_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return Line_Number_Type
   is begin
      for Token of reverse Tokens loop
         if Token.First and Token.ID = Descriptor.Comment_ID then
            return Token.Line;
         end if;
      end loop;
      return Invalid_Line_Number;
   end Last_Comment_Line;

   procedure Reduce
     (Nonterm        : in out Augmented_Token'Class;
      Tokens         : in     Augmented_Token_Access_Array;
      Descriptor     : in     WisiToken.Descriptor'Class;
      Compute_Indent : in     Boolean)
   is
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;

      Trailing_Comment_Done : Boolean := False;
   begin
      for I in reverse Tokens'Range loop
         --  'reverse' to find token containing trailing comments; last
         --  non-virtual and non-empty token.
         declare
            use all type SAL.Base_Peek_Type;
            Aug_Token : Augmented_Token renames Tokens (I).all;
         begin
            if Aug_Token.Byte_Region /= Null_Buffer_Region then
               --  Aug_Token not entirely virtual

               if Compute_Indent then
                  if Aug_Token.Non_Grammar.Length > 0 then
                     Aug_Token.First_Trailing_Comment_Line := First_Comment_Line (Aug_Token.Non_Grammar, Descriptor);
                     Aug_Token.Last_Trailing_Comment_Line  := Last_Comment_Line (Aug_Token.Non_Grammar, Descriptor);
                  end if;

                  if Aug_Token.First_Terminals_Index /= Augmented_Token_Arrays.No_Index then
                     Nonterm.First_Terminals_Index := Aug_Token.First_Terminals_Index;
                  end if;

                  if Nonterm.Last_Terminals_Index = Augmented_Token_Arrays.No_Index then
                     Nonterm.Last_Terminals_Index := Aug_Token.Last_Terminals_Index;
                  end if;

                  Nonterm.First := Nonterm.First or Aug_Token.First;

                  if Aug_Token.First then
                     if Aug_Token.First_Indent_Line /= Invalid_Line_Number then
                        Nonterm.First_Indent_Line := Aug_Token.First_Indent_Line;
                     elsif Trailing_Comment_Done and Aug_Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                        Nonterm.First_Indent_Line := Aug_Token.First_Trailing_Comment_Line;
                     end if;

                     if Nonterm.Last_Indent_Line = Invalid_Line_Number then
                        if Trailing_Comment_Done and Aug_Token.Last_Trailing_Comment_Line /= Invalid_Line_Number then
                           Nonterm.Last_Indent_Line := Aug_Token.Last_Trailing_Comment_Line;
                        elsif Aug_Token.Last_Indent_Line /= Invalid_Line_Number then
                           Nonterm.Last_Indent_Line := Aug_Token.Last_Indent_Line;
                        end if;
                     end if;
                  end if;

                  if not Trailing_Comment_Done then
                     Nonterm.First_Trailing_Comment_Line := Aug_Token.First_Trailing_Comment_Line;
                     Nonterm.Last_Trailing_Comment_Line  := Aug_Token.Last_Trailing_Comment_Line;
                     Trailing_Comment_Done := True;
                  end if;

               end if; --  Compute_Indent

               if Aug_Token.Line /= Invalid_Line_Number then
                  Nonterm.Line := Aug_Token.Line;
                  Nonterm.Col  := Aug_Token.Col;
               end if;

               if Nonterm.Char_Region.First > Aug_Token.Char_Region.First then
                  Nonterm.Char_Region.First := Aug_Token.Char_Region.First;
               end if;

               if Nonterm.Char_Region.Last < Aug_Token.Char_Region.Last then
                  Nonterm.Char_Region.Last := Aug_Token.Char_Region.Last;
               end if;

               Nonterm.Paren_State := Aug_Token.Paren_State;
            end if; -- Aug_Token not virtual
         end;
      end loop;
   end Reduce;

end WisiToken.Semantic_State;
