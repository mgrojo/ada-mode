--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with SAL;
package body WisiToken.Token_Line_Comment is

   function Trailing_Blank_Line (State : in State_Type; ID : in Token_ID) return Boolean
   is
      --  Return True if token at State.All_Tokens ends with two New_Line
      --  tokens (ie, a blank line).
      use all type Ada.Containers.Count_Type;
      New_Line_ID : Token_ID renames State.Trace.Descriptor.New_Line_ID;
      I           : constant Positive_Index_Type := State.All_Tokens.Last_Index;
   begin
      return ID = New_Line_ID and State.All_Tokens (I).ID = New_Line_ID;
   end Trailing_Blank_Line;

   ----------
   --  Public subprograms, declaration order

   function First_Line
     (Token             : in Token_Line_Comment.Token;
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
     (Token             : in Token_Line_Comment.Token;
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

   function Find
     (State : in State_Type;
      ID    : in Token_ID;
      Token : in Token_Line_Comment.Token'Class)
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
     (State : in State_Type;
      Line  : in Line_Number_Type;
      Start : in Token'Class)
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
              (State.All_Tokens (Current - 1).Line = Line and
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

   overriding
   procedure Initialize (State : not null access State_Type; Init : in WisiToken.Semantic_State.Init_Data'Class)
   is
      Line_Count : Line_Number_Type renames Init_Data (Init).Line_Count;
   begin
      Token_Region.Initialize (Token_Region.State_Type (State.all)'Access, Init);

      State.Line_Paren_State.Set_Length (Ada.Containers.Count_Type (Line_Count));

      State.Reset (Init_Done => True);
   end Initialize;

   overriding
   procedure Reset (State : not null access State_Type; Init_Done : in Boolean := False)
   is begin
      if not Init_Done then
         Token_Region.Reset (Token_Region.State_Type (State.all)'Access);
      end if;

      State.All_Tokens.Clear;

      for S of State.Line_Paren_State loop
         S := 0;
      end loop;
      State.Current_Paren_State := 0;
   end Reset;

   overriding procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      Token : in              Base_Token;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Temp_First : constant Boolean := Lexer.First and Lexer.Char_Region /= Null_Buffer_Region;

      Temp : constant Token_Line_Comment.Token :=
        (Token.ID,
         Name                        => Token.Name,
         Virtual                     => False,
         Line                        => Lexer.Line,
         Col                         => Lexer.Column,
         Char_Region                 => Lexer.Char_Region,
         Byte_Region                 => Lexer.Byte_Region,
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
            State.Line_Paren_State (Temp.Line) := State.Current_Paren_State;
         end if;

         declare
            procedure Set_Prev_Trailing (Prev_Token : in out Token_Line_Comment.Token)
            is
               --  Prev_Token is in either State.Stack or State.Lookahead_Queue; we
               --  must also update the copy in State.All_Tokens.
               Copy : Token_Line_Comment.Token renames State.All_Tokens.Reference
                 (Prev_Token.First_All_Tokens_Index).Element.all;

               Trailing_Blank : constant Boolean := Trailing_Blank_Line (State.all, Token.ID);
            begin
               Prev_Token.Last_All_Tokens_Index := Temp.First_All_Tokens_Index;
               Copy.Last_All_Tokens_Index       := Temp.First_All_Tokens_Index;

               if Temp.First and (Temp.ID = State.Trace.Descriptor.Comment_ID or Trailing_Blank) then
                  Prev_Token.First := True;
                  Copy.First       := True;

                  if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Prev_Token.First_Trailing_Comment_Line := (if Trailing_Blank then Temp.Line - 1 else Temp.Line);
                     Copy.First_Trailing_Comment_Line       := Prev_Token.First_Trailing_Comment_Line;
                  end if;
                  Prev_Token.Last_Trailing_Comment_Line := (if Trailing_Blank then Temp.Line - 1 else Temp.Line);
                  Copy.Last_Trailing_Comment_Line       := Prev_Token.Last_Trailing_Comment_Line;
               end if;
            end Set_Prev_Trailing;

         begin
            if State.Lookahead_Queue.Length = 0 then
               if State.Stack.Length = 0 then
                  --  no previous token
                  null;
               else
                  Set_Prev_Trailing
                    (Token_Line_Comment.Token (State.Stack.Reference (SAL.Peek_Type (State.Stack.Length)).Element.all));
               end if;
            else
               Set_Prev_Trailing
                 (Token_Line_Comment.Token
                    (State.Lookahead_Queue.Variable_Peek
                       (State.Lookahead_Queue.Length).Element.all));
            end if;
         end;

         if Trace_Parse > Extra then
            State.Trace.Put_Line
              ("lexer to non_grammar: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
         end if;

      else
         if Token.ID = State.Trace.Descriptor.Left_Paren_ID then
            State.Current_Paren_State := State.Current_Paren_State + 1;

         elsif Token.ID = State.Trace.Descriptor.Right_Paren_ID then
            State.Current_Paren_State := State.Current_Paren_State - 1;
         end if;

         State.Lookahead_Queue.Put (Temp);

         if Trace_Parse > Extra then
            State.Trace.Put_Line
              ("lexer to lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
         end if;
      end if;

      State.All_Tokens.Append (Temp);
   end Lexer_To_Lookahead;

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      Token : in              Base_Token)
   is
      Temp : constant Token_Line_Comment.Token :=
        (ID                          => Token.ID,
         Name                        => Token.Name,
         Virtual                     => True,
         Line                        => Invalid_Line_Number,
         Col                         => 0,
         Char_Region                 => Null_Buffer_Region,
         Byte_Region                 => Null_Buffer_Region,
         First                       => False,
         First_All_Tokens_Index      => Invalid_All_Tokens_Index,
         Last_All_Tokens_Index       => Invalid_All_Tokens_Index,
         First_Indent_Line           => Invalid_Line_Number,
         Last_Indent_Line            => Invalid_Line_Number,
         First_Trailing_Comment_Line => Invalid_Line_Number,
         Last_Trailing_Comment_Line  => Invalid_Line_Number,
         Paren_State                 => 0);
   begin
      State.Lookahead_Queue.Add_To_Head (Temp);

      if Trace_Parse > Extra then
         State.Trace.Put_Line
           ("virtual_to_lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Virtual_To_Lookahead;

   overriding
   procedure Reduce_Stack
     (State       : not null access State_Type;
      Nonterm     : in              Base_Token;
      Index       : in              Natural;
      Base_Tokens : in              WisiToken.Base_Token_Arrays.Vector;
      Action      : in              WisiToken.Semantic_State.Semantic_Action)
   is
      use WisiToken.Semantic_State;
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;

      Aug_Nonterm : Token :=
        (ID                          => Nonterm.ID,
         Name                        => Nonterm.Name,
         Virtual                     => False,
         Line                        => Invalid_Line_Number,
         Col                         => 0,
         Byte_Region                 => Nonterm.Byte_Region,
         Char_Region                 => Null_Buffer_Region,
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
            Aug_Token  : Token renames Token (State.Stack (Stack_I).Element.all);
         begin
            if Base_Token.ID /= Aug_Token.ID then
               raise Programmer_Error with "parser token: " &
                 Base_Token.Image (State.Trace.Descriptor.all) &
                 ", state token: " & Aug_Token.Image (State.Trace.Descriptor.all);
            end if;

            if Aug_Nonterm.First_All_Tokens_Index = Invalid_All_Tokens_Index and
              Aug_Token.First_All_Tokens_Index /= Invalid_All_Tokens_Index
            then
               Aug_Nonterm.First_All_Tokens_Index := Aug_Token.First_All_Tokens_Index;
            end if;

            if Aug_Token.Last_All_Tokens_Index /= Invalid_All_Tokens_Index then
               Aug_Nonterm.Last_All_Tokens_Index := Aug_Token.Last_All_Tokens_Index;
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

            if not Paren_State_Set then
               Aug_Nonterm.Paren_State := Aug_Token.Paren_State;
               Paren_State_Set := True;
            end if;

            Aug_Tokens.Append (Aug_Token);

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
               Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
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
               Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
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
         Token_Region.Put
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

end WisiToken.Token_Line_Comment;
