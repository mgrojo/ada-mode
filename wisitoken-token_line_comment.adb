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

package body WisiToken.Token_Line_Comment is

   overriding procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      Temp : constant Token :=
        (ID,
         Virtual     => False,
         Line        => Lexer.Line,
         Col         => Lexer.Column,
         Char_Region => Lexer.Char_Region,
         Byte_Region => Lexer.Byte_Region,
         Non_Grammar => WisiToken.Augmented_Token_Arrays.Empty_Vector);

   begin
      if ID < State.Trace.Descriptor.First_Terminal then
         --  Non-grammar token
         if State.Stack.Length = 0 then
            State.Initial_Non_Grammar.Append (Temp);
         else
            declare
               Prev_Token : Token renames Token
                 (State.Stack.Reference (State.Stack.Length).Element.all);
            begin
               Prev_Token.Non_Grammar.Append (Temp);
            end;
         end if;
      else
         State.Lookahead_Queue.Put (Temp);

      end if;
      if Trace_Parse > 2 then
         State.Trace.Put_Line
           ("lexer_to_lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Lexer_To_Lookahead;

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in     Token_ID)
   is
      Temp : constant Token :=
        (ID,
         Virtual     => True,
         Line        => 0,
         Col         => 0,
         Char_Region => Null_Buffer_Region,
         Byte_Region => Null_Buffer_Region,
         Non_Grammar => WisiToken.Augmented_Token_Arrays.Empty_Vector);
   begin
      State.Lookahead_Queue.Add_To_Head (Temp);

      if Trace_Parse > 2 then
         State.Trace.Put_Line
           ("virtual_to_lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Virtual_To_Lookahead;

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      IDs     : in              WisiToken.Token_Array;
      Action  : in              Semantic_Action)
   is
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;

      Aug_Nonterm : Token :=
        (WisiToken.Token_Region.Token with WisiToken.Augmented_Token_Arrays.Empty_Vector);

      Stack_I    : Augmented_Token_Arrays.Cursor := State.Stack.To_Cursor (State.Stack.Length - IDs.Length + 1);
      Aug_Tokens : Augmented_Token_Arrays.Vector;
   begin
      Aug_Nonterm.ID      := Nonterm;
      Aug_Nonterm.Virtual := False;

      for I in IDs.First_Index .. IDs.Last_Index loop
         declare
            use all type Ada.Text_IO.Count;
            ID    : Token_ID renames IDs.Element (I);
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token (State.Stack (Stack_I).Element.all);
         begin
            if ID /= State.Stack (Stack_I).ID then
               raise Programmer_Error;
            end if;

            if Action /= null then
               Aug_Tokens.Append (Token);
            end if;

            if Aug_Nonterm.Line = 0 and Token.Line > 0 then
               Aug_Nonterm.Line := Token.Line;
               Aug_Nonterm.Col  := Token.Col;
            end if;

            if Aug_Nonterm.Char_Region.First > Token.Char_Region.First then
               Aug_Nonterm.Char_Region.First := Token.Char_Region.First;
            end if;

            if Aug_Nonterm.Char_Region.Last < Token.Char_Region.Last then
               Aug_Nonterm.Char_Region.Last := Token.Char_Region.Last;
            end if;

            if Aug_Nonterm.Byte_Region.First > Token.Byte_Region.First then
               Aug_Nonterm.Byte_Region.First := Token.Byte_Region.First;
            end if;

            if Aug_Nonterm.Byte_Region.Last < Token.Byte_Region.Last then
               Aug_Nonterm.Byte_Region.Last := Token.Byte_Region.Last;
            end if;

            if I = IDs.Last_Index then
               if Token.Non_Grammar.Length > 0 then
                  declare
                     Last_Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
                       (Token.Non_Grammar (Token.Non_Grammar.Last_Index).Element.all);
                  begin
                     Aug_Nonterm.Char_Region.Last := Last_Token.Char_Region.Last;
                     Aug_Nonterm.Byte_Region.Last := Last_Token.Byte_Region.Last;
                  end;
               end if;
            end if;
         end;

         Next (Stack_I);
      end loop;

      if Trace_Parse > 0 then
         --  We use the stack for the trace, not Aug_Tokens, because
         --  we don't compute aug_tokens when Action is null.
         Token_Region.Put
           (State.Trace.all, Aug_Nonterm, Index, State.Stack, IDs.Length, Include_Action_Name => Action /= null);
      end if;

      for I in 1 .. IDs.Length loop
         State.Stack.Delete_Last;
      end loop;

      if Action /= null then
         Action (Aug_Nonterm, Index, Aug_Tokens);
      end if;

      State.Stack.Append (Aug_Nonterm);
   end Reduce_Stack;

end WisiToken.Token_Line_Comment;
