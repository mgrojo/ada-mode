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

pragma License (GPL);

with Ada.Characters.Handling;
package body WisiToken.Token_Region is

   function Image
     (Descriptor : in WisiToken.Descriptor'Class;
      Item       : in Token;
      ID_Only    : in Boolean)
     return String
   is
      use all type Ada.Text_IO.Count;
      Name : constant String := WisiToken.Image (Descriptor, Item.ID);
   begin
      if ID_Only then
         return Name;

      elsif Item.Line > 0 then
         return "(" & Name &
           Ada.Text_IO.Count'Image (Item.Line) & ":" & Int_Image (Integer (Item.Col)) & ")";

      elsif Item.Region = Null_Buffer_Region then
         return "(" & Name & ")";

      else
         --  For test result backward compatiblity:
         --  we don't call Image (Item.Region) here
         return "(" & Name &
           Integer'Image (Item.Region.Begin_Pos) & " ." & Integer'Image (Item.Region.End_Pos) & ")";
      end if;
   end Image;

   procedure Put
     (Trace     : in out WisiToken.Trace'Class;
      Stack     : in     Augmented_Token_Array;
      Count     : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'First;
      Top_First : in     Boolean                   := True)
   is
      --  Put top Count items on Stack; all if -1.
      use Augmented_Token_Arrays;
      use Ada.Containers;

      First : constant Count_Type := (if Count = Ada.Containers.Count_Type'First then 1 else Stack.Length - Count + 1);
   begin
      Trace.Put ("(");
      if Top_First then
         for I in reverse First .. Stack.Last_Index loop

            Trace.Put (Image (Trace.Descriptor.all, Token (Stack (I).Element.all), ID_Only => False));
            if I /= First then
               Trace.Put (", ");
            end if;
         end loop;
      else
         for I in First .. Stack.Last_Index loop

            Trace.Put (Image (Trace.Descriptor.all, Token (Stack (I).Element.all), ID_Only => False));
            if I /= Stack.Last_Index then
               Trace.Put (", ");
            end if;
         end loop;
      end if;
      Trace.Put (")");
   end Put;

   procedure Put
     (Trace : in out WisiToken.Trace'Class;
      Queue : in     Token_Queues.Queue_Type)
   is
      use all type SAL.Base_Peek_Type;
   begin
      Trace.Put ("(");
      for I in 1 .. Queue.Count loop
         Trace.Put (Image (Trace.Descriptor.all, Queue.Peek (I), ID_Only => False));
         if I < Queue.Count then
            Trace.Put (", ");
         end if;
      end loop;
      Trace.Put (")");
   end Put;

   procedure Put
     (Trace               : in out WisiToken.Trace'Class;
      Nonterm             : in     Token;
      Index               : in     Natural;
      Stack               : in     Augmented_Token_Array;
      Tokens_Length       : in     Ada.Containers.Count_Type;
      Include_Action_Name : in     Boolean)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Characters.Handling;
      use Augmented_Token_Arrays;

      Action_Name : constant String :=
        (if Include_Action_Name
         then To_Lower (Image (Trace.Descriptor.all, Nonterm.ID)) & "_" & WisiToken.Int_Image (Index) & ": "
         else "");
   begin
      Trace.Put (Action_Name & Image (Trace.Descriptor.all, Nonterm, ID_Only => False) & " <= ");
      Put (Trace, Stack, Count => Tokens_Length, Top_First => False);
      Trace.New_Line;
   end Put;

   ----------
   --  Public subprograms

   procedure Put
     (File_Name  : in String;
      List       : in Error_Data_Lists.List;
      Descriptor : in WisiToken.Descriptor'Class)
   is
      use Ada.Text_IO;
   begin
      for Item of List loop
         if Item.Error_Token.Line = 0 then
            Put_Line
              (File_Name & ": syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                 ", found '" & Image (Descriptor, Item.Error_Token, ID_Only => False) & "'");
         else
            Put_Line
              (Error_Message
                 (File_Name, Item.Error_Token.Line, Item.Error_Token.Col,
                  "syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                 ", found '" & Image (Descriptor, Item.Error_Token, ID_Only => True) & "'"));
         end if;

         if Item.Invalid_Region /= Null_Buffer_Region then
            Put_Line ("   invalid_region: " & Image (Item.Invalid_Region));
         end if;

         if Item.Recover /= null then
            null;
            --  FIXME: Image (recover)
         end if;
      end loop;
   end Put;

   overriding
   procedure Put (State : access State_Type)
   is
   begin
      State.Trace.Put ("semantic state: stack: ");
      Put (State.Trace.all, State.Stack);
      State.Trace.New_Line;
      State.Trace.Put ("semantic state: lookahead queue: ");
      Put (State.Trace.all, State.Lookahead_Queue);
      State.Trace.New_Line;
      --  FIXME: invalid_regions?
   end Put;

   overriding
   procedure Reset (State : access State_Type)
   is begin
      State.Stack.Clear;
      State.Lookahead_Queue.Clear;
      State.Errors.Clear;
   end Reset;

   overriding
   procedure Lexer_To_Lookahead
     (State : access          State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'class)
   is
      Temp : constant Token := (ID, Line => Lexer.Line, Col => Lexer.Column, Region => Lexer.Bounds);
   begin
      State.Lookahead_Queue.Put (Temp);

      if Trace_Parse > 3 then
         State.Trace.Put_Line
           ("lexer_to_lookahead: " & Image (State.Trace.Descriptor.all, Temp, ID_Only => False));
      end if;
   end Lexer_To_Lookahead;

   overriding
   procedure Virtual_To_Lookahead
     (State : access State_Type;
      ID    : in     Token_ID)
   is
      Temp : constant Token := (ID, Line => 0, Col => 0, Region => Null_Buffer_Region);
   begin
      State.Lookahead_Queue.Add_To_Head (Temp);

      if Trace_Parse > 3 then
         State.Trace.Put_Line
           ("virtual_to_lookahead: " & Image (State.Trace.Descriptor.all, Temp, ID_Only => False));
      end if;
   end Virtual_To_Lookahead;

   overriding
   procedure Push_Current
     (State : access State_Type;
      ID    : in     Token_ID)
   is
      Temp : constant Token := State.Lookahead_Queue.Get;
   begin
      if ID /= Temp.ID then
         raise Programmer_Error with "token_region.push_current: ID " &
           Image (State.Trace.Descriptor.all, ID) &
           ", Token " & Image (State.Trace.Descriptor.all, Temp, ID_Only => False);
      end if;

      State.Stack.Append (Temp);

      if Trace_Parse > 3 then
         State.Trace.Put_Line
           ("push_current: " & Image (State.Trace.Descriptor.all, Temp, ID_Only => False));
      end if;
   end Push_Current;

   overriding procedure Error
     (State     : access State_Type;
      Expecting : in     Token_ID_Set)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Trace_Parse > 3 then
         State.Trace.Put_Line ("expecting: " & Image (State.Trace.Descriptor.all, Expecting));
      end if;

      State.Errors.Append
        ((First_Terminal => State.Trace.Descriptor.First_Terminal,
          Last_Terminal  => State.Trace.Descriptor.Last_Terminal,
          Error_Token    => State.Lookahead_Queue.Peek,
          Expecting      => Expecting,

          --  The following are set in Recover
          Invalid_Region => Null_Buffer_Region,
          Recover        => null));

      State.Invalid_Region := Null_Buffer_Region;
   end Error;

   overriding
   procedure Discard_Lookahead
     (State : access State_Type;
      ID    : in     Token_ID)
   is
      Token : constant Token_Region.Token := State.Lookahead_Queue.Get;
   begin
      if ID /= Token.ID then
         raise Programmer_Error with "token_region.discard_lookahead: ID " &
           Image (State.Trace.Descriptor.all, ID) &
           ", Token " & Image (State.Trace.Descriptor.all, Token, ID_Only => False);
      end if;
      State.Invalid_Region := State.Invalid_Region and Token.Region;
      if Trace_Parse > 3 then
         State.Trace.Put_Line ("discard_lookahead: " & Image (State.Trace.Descriptor.all, Token, ID_Only => False));
      end if;
   end Discard_Lookahead;

   overriding
   procedure Discard_Stack
     (State : access State_Type;
      ID    : in     Token_ID)
   is
      Token : constant Token_Region.Token := Token_Region.Token (Augmented_Token_Arrays.Element (State.Stack.Last));
   begin
      State.Stack.Delete_Last;
      if ID /= Token.ID then
         raise Programmer_Error;
      end if;
      State.Invalid_Region := State.Invalid_Region and Token.Region;
      if Trace_Parse > 3 then
         State.Trace.Put_Line ("discard_stack: " & Image (State.Trace.Descriptor.all, Token, ID_Only => False));
      end if;
   end Discard_Stack;

   overriding
   procedure Reduce_Stack
     (State   : access State_Type;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      IDs     : in     WisiToken.Token.List.Instance;
      Action  : in     Semantic_Action)
   is
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;
      use all type WisiToken.Token.List.List_Iterator;

      ID_I : WisiToken.Token.List.List_Iterator := IDs.First;

      Aug_Nonterm : Token                         := Default_Token;
      Stack_I     : Augmented_Token_Arrays.Cursor := State.Stack.To_Cursor (State.Stack.Length - IDs.Length + 1);
      Aug_Tokens  : Augmented_Token_Arrays.Vector;
   begin
      Aug_Nonterm.ID := Nonterm;

      loop
         exit when Is_Null (ID_I);
         declare
            use all type Ada.Text_IO.Count;
            ID    : Token_ID renames Current (ID_I);
            Token : Token_Region.Token renames Token_Region.Token (State.Stack (Stack_I).Element.all);
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

            if Aug_Nonterm.Region.Begin_Pos > Token.Region.Begin_Pos then
               Aug_Nonterm.Region.Begin_Pos := Token.Region.Begin_Pos;
            end if;

            if Aug_Nonterm.Region.End_Pos < Token.Region.End_Pos then
               Aug_Nonterm.Region.End_Pos := Token.Region.End_Pos;
            end if;
         end;

         Next (ID_I);
         Next (Stack_I);
      end loop;

      if Trace_Parse > 1 then
         --  We use the stack for the trace, not Aug_Tokens, because
         --  we don't compute aug_tokens when Action is null.
         Put
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

   overriding
   procedure Recover
     (State   : access State_Type;
      Recover : in     WisiToken.Token.Recover_Data'Class)
   is
      Error : Error_Data renames State.Errors.Reference (State.Errors.Last);
   begin
      Error.Invalid_Region := State.Invalid_Region;
      Error.Recover        := new WisiToken.Token.Recover_Data'Class'(Recover);
      if Trace_Parse > 3 then
         State.Trace.Put_Line
           ("recover: invalid_region " & Image (Error.Invalid_Region));
      end if;
   end Recover;

end WisiToken.Token_Region;
