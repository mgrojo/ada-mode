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

with Ada.Characters.Handling;
package body WisiToken.Semantic_State is

   --  Body subprograms, alphabetical

   procedure Put
     (Trace     : in out WisiToken.Trace'Class;
      Stack     : in     WisiToken.Semantic_State.Augmented_Token_Arrays.Vector;
      Count     : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
      Top_First : in     Boolean                   := True)
   is
      --  Put top Count items on Stack; all if Count_Type'Last.
      --  Top is Stack.Last_Index
      use all type Positive_Index_Type;
      use WisiToken.Semantic_State.Augmented_Token_Arrays;
      use Ada.Containers;

      First : constant Positive_Index_Type :=
        (if Count = Ada.Containers.Count_Type'Last then 1 else Positive_Index_Type (Stack.Length - Count + 1));
   begin
      if Count = 0 then
         Trace.Put ("()");
         return;
      end if;

      Trace.Put ("(");
      if Top_First then
         for I in reverse First .. Stack.Last_Index loop

            Trace.Put (Stack (I).Image (Trace.Descriptor.all, ID_Only => False));
            if I /= First then
               Trace.Put (", ");
            end if;
         end loop;
      else
         for I in First .. Stack.Last_Index loop

            Trace.Put (Stack (I).Image (Trace.Descriptor.all, ID_Only => False));
            if I /= Stack.Last_Index then
               Trace.Put (", ");
            end if;
         end loop;
      end if;
      Trace.Put (")");
   end Put;

   procedure Put
     (Trace               : in out WisiToken.Trace'Class;
      Nonterm             : in     Augmented_Token;
      Index               : in     Natural;
      Stack               : in     WisiToken.Semantic_State.Augmented_Token_Arrays.Vector;
      Tokens_Length       : in     Ada.Containers.Count_Type;
      Include_Action_Name : in     Boolean)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Characters.Handling;
      use WisiToken.Semantic_State.Augmented_Token_Arrays;

      Action_Name : constant String :=
        (if Include_Action_Name
         then To_Lower (Image (Nonterm.ID, Trace.Descriptor.all)) & "_" & WisiToken.Int_Image (Index) & ": "
         else "");
   begin
      Trace.Put (Action_Name & Nonterm.Image (Trace.Descriptor.all, ID_Only => False) & " <= ");
      Put (Trace, Stack, Count => Tokens_Length, Top_First => False);
      Trace.New_Line;
   end Put;

   function Trailing_Blank_Line (State : in Semantic_State; ID : in Token_ID) return Boolean
   is
      --  Return True if token at State.All_Tokens.Last_Index ends with two New_Line
      --  tokens (ie, a blank line).
      use all type Ada.Containers.Count_Type;
      New_Line_ID : Token_ID renames State.Trace.Descriptor.New_Line_ID;
      I           : constant Positive_Index_Type := State.All_Tokens.Last_Index;
   begin
      return ID = New_Line_ID and State.All_Tokens (I).ID = New_Line_ID;
   end Trailing_Blank_Line;

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

      elsif Item.Line /= Invalid_Line_Number and Trace_Parse <= Detail then
         return "(" & ID_Image &
           Line_Number_Type'Image (Item.Line) & ":" & Int_Image (Integer (Item.Col)) & ")";

      elsif Item.Char_Region = Null_Buffer_Region then
         return "(" & ID_Image & ")";

      else
         return "(" & ID_Image & ", " & Image (Item.Char_Region) & ")";
      end if;
   end Image;

   procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Augmented_Token'Class)
   is begin
      Put (Trace, Item.Image (Trace.Descriptor.all, ID_Only => False));
   end Put;

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

   ----------
   --  Semantic_State

   function Find
     (State : in Semantic_State;
      ID    : in Token_ID;
      Token : in Augmented_Token'Class)
     return SAL.Base_Peek_Type
   is begin
      --  linear search for ID.
      for I in Token.First_All_Tokens_Index .. Token.Last_All_Tokens_Index loop
         if State.All_Tokens (I).ID = ID then
            return I;
         end if;
      end loop;
      return Invalid_All_Tokens_Index;
   end Find;

   function Find_Line_Begin
     (State : in Semantic_State;
      Line  : in Line_Number_Type;
      Start : in Augmented_Token'Class)
     return Positive_Index_Type
   is
      use all type SAL.Base_Peek_Type;
      Current : Positive_Index_Type :=
        (if Line <= Start.Line
         then Start.First_All_Tokens_Index
         else Start.Last_All_Tokens_Index);
   begin
      if State.All_Tokens (Current).Line >= Line then
         --  Search backwards
         loop
            exit when Current = State.All_Tokens.First_Index or else
              (State.All_Tokens (Current - 1).Line = Line - 1 and
                 State.All_Tokens (Current - 1).ID = State.Trace.Descriptor.New_Line_ID);
            Current := Current - 1;
         end loop;
         if State.All_Tokens (Current).ID = State.Trace.Descriptor.New_Line_ID then
            return Current - 1;
         else
            return Current;
         end if;

      else
         --  Search forwards
         loop
            exit when Current = State.All_Tokens.Last_Index or else
              State.All_Tokens (Current).ID = State.Trace.Descriptor.New_Line_ID;
            Current := Current + 1;
         end loop;

         if Current = State.All_Tokens.Last_Index then
            return Current;
         elsif State.All_Tokens (Current + 1).ID = State.Trace.Descriptor.New_Line_ID then
            return Current;
         else
            return Current + 1;
         end if;
      end if;
   end Find_Line_Begin;

   ----------
   --  Parser operations on Semantic_State

   procedure Initialize
     (State      : not null access Semantic_State;
      Line_Count : in              Line_Number_Type)
   is begin
      State.Line_Paren_State.Set_Length (Ada.Containers.Count_Type (Line_Count));

      State.Reset;
   end Initialize;

   procedure Reset (State : not null access Semantic_State)
   is begin
      State.Stack.Clear;
      State.All_Tokens.Clear;

      for S of State.Line_Paren_State loop
         S := 0;
      end loop;
      State.Current_Paren_State := 0;
   end Reset;

   procedure Put (State : not null access Semantic_State)
   is
      use WisiToken.Semantic_State;
   begin
      State.Trace.Put ("semantic state: stack: ");
      Put (State.Trace.all, State.Stack);
      State.Trace.New_Line;
   end Put;

   procedure Lexer_To_Augmented
     (State : not null access Semantic_State;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Temp_First : constant Boolean := Lexer.First and Lexer.Char_Region /= Null_Buffer_Region;

      Temp : constant Augmented_Token :=
        (Token.ID,
         Line                        => Lexer.Line,
         Col                         => Lexer.Column,
         Char_Region                 => Lexer.Char_Region,
         Byte_Region                 => Lexer.Byte_Region,
         Virtual                     => False,
         First_All_Tokens_Index      => State.All_Tokens.Last_Index + 1,
         Last_All_Tokens_Index       => Invalid_All_Tokens_Index,
         First                       => Temp_First,
         First_Indent_Line           => (if Temp_First then Lexer.Line else Invalid_Line_Number),
         Last_Indent_Line            => (if Temp_First then Lexer.Line else Invalid_Line_Number),
         First_Trailing_Comment_Line => Invalid_Line_Number,
         Last_Trailing_Comment_Line  => Invalid_Line_Number,
         Paren_State                 => State.Current_Paren_State);

   begin
      if Token.ID < State.Trace.Descriptor.First_Terminal then
         --  Non-grammar token
         if Token.ID = State.Trace.Descriptor.New_Line_ID then
            State.Line_Paren_State (Temp.Line + 1) := State.Current_Paren_State;
         end if;

         declare
            procedure Set_Prev_Trailing (Prev_Token : in out Augmented_Token)
            is
               --  Prev_Token is in either State.Stack or State.Lookahead_Queue; we
               --  must also update the copy in State.All_Tokens.
               Copy : Augmented_Token renames State.All_Tokens.Reference
                 (Prev_Token.First_All_Tokens_Index).Element.all;

               Trailing_Blank : constant Boolean := Trailing_Blank_Line (State.all, Token.ID);
            begin
               Prev_Token.Last_All_Tokens_Index := Temp.First_All_Tokens_Index;
               Copy.Last_All_Tokens_Index       := Temp.First_All_Tokens_Index;

               if Temp.First and (Temp.ID = State.Trace.Descriptor.Comment_ID or Trailing_Blank) then
                  Prev_Token.First := True;
                  Copy.First       := True;

                  if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Prev_Token.First_Trailing_Comment_Line := Temp.Line;
                     Copy.First_Trailing_Comment_Line       := Prev_Token.First_Trailing_Comment_Line;
                  end if;
                  Prev_Token.Last_Trailing_Comment_Line := Temp.Line;
                  Copy.Last_Trailing_Comment_Line       := Prev_Token.Last_Trailing_Comment_Line;
               end if;
            end Set_Prev_Trailing;

         begin
            if State.Stack.Length = 0 then
               --  no previous token
               null;
            else
               Set_Prev_Trailing
                 (Augmented_Token (State.Stack.Reference (SAL.Peek_Type (State.Stack.Length)).Element.all));
            end if;
         end;

         if Trace_Parse > Extra then
            State.Trace.Put_Line
              ("lexer to non_grammar: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
         end if;

      else
         --  grammar token

         if Token.ID = State.Trace.Descriptor.Left_Paren_ID then
            State.Current_Paren_State := State.Current_Paren_State + 1;

         elsif Token.ID = State.Trace.Descriptor.Right_Paren_ID then
            State.Current_Paren_State := State.Current_Paren_State - 1;
         end if;

         if Trace_Parse > Extra then
            State.Trace.Put_Line
              ("lexer to augmented: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
         end if;
      end if;

      State.All_Tokens.Append (Temp);
   end Lexer_To_Augmented;

   procedure Reduce_Stack
     (State       : not null access Semantic_State;
      Nonterm     : in              Base_Token;
      Index       : in              Natural;
      Base_Tokens : in              WisiToken.Base_Token_Arrays.Vector;
      Action      : in              WisiToken.Semantic_State.Semantic_Action)
   is
      use WisiToken.Semantic_State;
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;

      Aug_Nonterm : Augmented_Token :=
        (ID                          => Nonterm.ID,
         Line                        => Invalid_Line_Number,
         Col                         => 0,
         Byte_Region                 => Nonterm.Byte_Region,
         Char_Region                 => Null_Buffer_Region,
         Virtual                     => False,
         First                       => False,
         First_All_Tokens_Index      => Invalid_All_Tokens_Index,
         Last_All_Tokens_Index       => Invalid_All_Tokens_Index,
         First_Indent_Line           => Invalid_Line_Number,
         Last_Indent_Line            => Invalid_Line_Number,
         First_Trailing_Comment_Line => Invalid_Line_Number,
         Last_Trailing_Comment_Line  => Invalid_Line_Number,
         Paren_State                 => 0);

      Stack_I         : Augmented_Token_Arrays.Cursor := State.Stack.To_Cursor
        (SAL.Base_Peek_Type (State.Stack.Length - Base_Tokens.Length + 1));
      Aug_Tokens      : Augmented_Token_Arrays.Vector;
      First_Set       : Boolean                       := False;
      Paren_State_Set : Boolean                       := False;
   begin
      for I in Base_Tokens.First_Index .. Base_Tokens.Last_Index loop
         declare
            use all type SAL.Base_Peek_Type;
            Base_Token : WisiToken.Base_Token renames Base_Tokens.Element (I);
            Aug_Token  : Augmented_Token renames State.Stack (Stack_I).Element.all;
         begin
            if Base_Token.ID /= Aug_Token.ID then
               raise Programmer_Error with "parser token: " &
                 Base_Token.Image (State.Trace.Descriptor.all) &
                 ", state token: " & Aug_Token.Image (State.Trace.Descriptor.all);
            end if;

            Aug_Nonterm.Virtual := Aug_Nonterm.Virtual or Aug_Token.Virtual;

            if Aug_Token.Byte_Region /= Null_Buffer_Region then
               --  Aug_Token not entirely virtual

               if Aug_Nonterm.First_All_Tokens_Index = Invalid_All_Tokens_Index then
                  Aug_Nonterm.First_All_Tokens_Index := Aug_Token.First_All_Tokens_Index;
               end if;

               if Aug_Token.Last_All_Tokens_Index /= Invalid_All_Tokens_Index then
                  Aug_Nonterm.Last_All_Tokens_Index := Aug_Token.Last_All_Tokens_Index;
               else
                  Aug_Nonterm.Last_All_Tokens_Index := Aug_Token.First_All_Tokens_Index;
               end if;

               if not First_Set then
                  if Aug_Token.First then
                     Aug_Nonterm.First := True;
                     First_Set := True;

                     if I = Base_Tokens.Last_Index then
                        Aug_Nonterm.First_Indent_Line := Aug_Token.First_Indent_Line;
                        Aug_Nonterm.Last_Indent_Line  := Aug_Token.Last_Indent_Line;
                     else
                        if Aug_Token.First_Indent_Line = Invalid_Line_Number then
                           Aug_Nonterm.First_Indent_Line := Aug_Token.First_Trailing_Comment_Line;
                        else
                           Aug_Nonterm.First_Indent_Line := Aug_Token.First_Indent_Line;
                        end if;

                        if Aug_Token.Last_Trailing_Comment_Line = Invalid_Line_Number then
                           Aug_Nonterm.Last_Indent_Line := Aug_Token.Last_Indent_Line;
                        else
                           Aug_Nonterm.Last_Indent_Line := Aug_Token.Last_Trailing_Comment_Line;
                        end if;
                     end if;
                  end if;
               end if;

               if Aug_Nonterm.Line = Invalid_Line_Number and Aug_Token.Line /= Invalid_Line_Number then
                  Aug_Nonterm.Line := Aug_Token.Line;
                  Aug_Nonterm.Col  := Aug_Token.Col;
               end if;

               if Aug_Nonterm.Char_Region.First > Aug_Token.Char_Region.First then
                  Aug_Nonterm.Char_Region.First := Aug_Token.Char_Region.First;
               end if;

               if Aug_Nonterm.Char_Region.Last < Aug_Token.Char_Region.Last then
                  Aug_Nonterm.Char_Region.Last := Aug_Token.Char_Region.Last;
               end if;
            end if; -- Aug_Token not virtual

            if not Paren_State_Set then
               Aug_Nonterm.Paren_State := Aug_Token.Paren_State;
               Paren_State_Set := True;
            end if;

            Aug_Tokens.Append (Aug_Token);
         end;

         Next (Stack_I);
      end loop;

      --  Find the trailing non-empty token, to set Aug_Nonterm.First_,
      --  Last_Trailing_Comment_Line.
      declare
         Cursor : Augmented_Token_Arrays.Cursor := Aug_Tokens.Last;
         use Augmented_Token_Arrays;
      begin
         loop
            exit when Cursor = No_Element;
            declare
               Token : Augmented_Token renames Augmented_Token
                 (Constant_Reference (Aug_Tokens, Cursor).Element.all);
            begin
               if Token.Char_Region /= Null_Buffer_Region then
                  Aug_Nonterm.First_Trailing_Comment_Line := Token.First_Trailing_Comment_Line;
                  Aug_Nonterm.Last_Trailing_Comment_Line  := Token.Last_Trailing_Comment_Line;
                  exit;
               end if;
            end;
            Cursor := Previous (Cursor);
         end loop;
      end;

      --  Find the trailing token with First set, to set Aug_Nonterm.Last_Indent_Line.
      declare
         Cursor : Augmented_Token_Arrays.Cursor := Aug_Tokens.Last;
         use Augmented_Token_Arrays;
      begin
         loop
            exit when Cursor = No_Element;
            declare
               Token : Augmented_Token renames Augmented_Token
                 (Constant_Reference (Aug_Tokens, Cursor).Element.all);
            begin
               if Token.Last_Indent_Line /= Invalid_Line_Number then
                  Aug_Nonterm.Last_Indent_Line := Token.Last_Indent_Line;
                  exit;
               end if;
            end;
            Cursor := Previous (Cursor);
         end loop;
      end;

      if Trace_Parse > Detail then
         Put
           (State.Trace.all, Aug_Nonterm, Index, Aug_Tokens, Aug_Tokens.Length, Include_Action_Name => Action /= null);
      end if;

      for I in 1 .. Base_Tokens.Length loop
         State.Stack.Delete_Last;
      end loop;

      if Action /= null then
         Action (Aug_Nonterm, Aug_Tokens);
      end if;

      State.Stack.Append (Aug_Nonterm);
   end Reduce_Stack;

end WisiToken.Semantic_State;
