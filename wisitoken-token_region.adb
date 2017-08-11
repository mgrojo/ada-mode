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
      Name : constant String := WisiToken.Image (Descriptor, Item.ID);
   begin
      if ID_Only then
         return Name;

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
     (Trace : in out WisiToken.Trace'Class;
      Stack : in     Augmented_Token_Array;
      Count : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'First)
   is
      --  Put top Count items on Stack; all if -1.
      use Augmented_Token_Arrays;
      use all type Ada.Containers.Count_Type;

      I : Cursor := Stack.To_Cursor (if Count = Ada.Containers.Count_Type'First then 1 else Stack.Length - Count + 1);
   begin
      Trace.Put ("(");
      loop
         exit when I = No_Element;

         Trace.Put (Image (Trace.Descriptor.all, Token (Stack (I).Element.all), ID_Only => False));
         Next (I);
         if I /= No_Element then
            Trace.Put (", ");
         end if;
      end loop;
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
         if I /= Queue.Count - 1 then
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
      Put (Trace, Stack, Tokens_Length);
      Trace.New_Line;
   end Put;

   ----------
   --  Public subprograms

   overriding
   procedure Put (State : access State_Type)
   is
   begin
      State.Trace.Put ("semantic state: stack: ");
      Put (State.Trace.all, State.Stack);
      State.Trace.New_Line;
      State.Trace.Put ("semantic state: input queue: ");
      Put (State.Trace.all, State.Input_Queue);
      State.Trace.New_Line;
      --  FIXME: invalid_regions?
   end Put;

   overriding
   procedure Reset (State : access State_Type)
   is begin
      State.Stack.Clear;
      State.Input_Queue.Clear;
      State.Errors.Clear;
   end Reset;

   overriding
   procedure Input_Token
     (State : access State_Type;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
   is
      use all type WisiToken.Lexer.Handle;
   begin
      if Lexer = null then
         State.Input_Queue.Add_To_Head ((Token, Null_Buffer_Region));
      else
         State.Input_Queue.Put ((Token, Lexer.Bounds));
      end if;
   end Input_Token;

   overriding
   procedure Input_Lookahead
     (State : access State_Type;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
   is begin
      State.Lookahead.Put ((Token, Lexer.Bounds));
   end Input_Lookahead;

   overriding
   procedure Move_Lookahead_To_Input
     (State : access State_Type;
      Token : in     Token_ID)
   is
      Tok : constant Token_Region.Token := State.Lookahead.Get;
   begin
      if Token /= Tok.ID then
         raise Programmer_Error with "token_region.move_lookahead: Token " &
           Image (State.Trace.Descriptor.all, Token) &
           ", Tok " & Image (State.Trace.Descriptor.all, Tok, ID_Only => False);
      end if;

      State.Input_Queue.Put (Tok);
   end Move_Lookahead_To_Input;

   overriding
   procedure Move_Input_To_Lookahead
     (State : access State_Type;
      Token : in     Token_ID)
   is
      Tok : constant Token_Region.Token := State.Input_Queue.Get;
   begin
      if Token /= Tok.ID then
         raise Programmer_Error with "token_region.move_lookahead: Token " &
           Image (State.Trace.Descriptor.all, Token) &
           ", Tok " & Image (State.Trace.Descriptor.all, Tok, ID_Only => False);
      end if;

      State.Lookahead.Put (Tok);
   end Move_Input_To_Lookahead;

   overriding
   procedure Push_Token
     (State : access State_Type;
      Token : in     Token_ID)
   is
      Tok : constant Token_Region.Token := State.Input_Queue.Get;
   begin
      if Token /= Tok.ID then
         raise Programmer_Error with "token_region.push_token: Token " &
           Image (State.Trace.Descriptor.all, Token) &
           ", Tok " & Image (State.Trace.Descriptor.all, Tok, ID_Only => False);
      end if;

      State.Stack.Append (Tok);
   end Push_Token;

   overriding procedure Error
     (State     : access State_Type;
      Expecting : in     Token_ID_Set)
   is begin
      State.Errors.Append
        ((First_Terminal => State.Trace.Descriptor.First_Terminal,
          Last_Terminal  => State.Trace.Descriptor.Last_Terminal,
          Error_Token    => State.Input_Queue.Peek,
          Expecting      => Expecting,

          --  The following are set in Recover
          Invalid_Region => Null_Buffer_Region,
          Recover        => null));
   end Error;

   overriding
   procedure Discard_Token
     (State : access State_Type;
      Token : in     Token_ID)
   is
      Tok : constant Token_Region.Token := State.Input_Queue.Get;
   begin
      if Token /= Tok.ID then
         raise Programmer_Error;
      end if;
      State.Invalid_Region := State.Invalid_Region and Tok.Region;
   end Discard_Token;

   overriding
   procedure Pop_Token
     (State : access State_Type;
      Token : in     Token_ID)
   is
      Tok : constant Token_Region.Token := Token_Region.Token (Augmented_Token_Arrays.Element (State.Stack.Last));
   begin
      State.Stack.Delete_Last;
      if Token /= Tok.ID then
         raise Programmer_Error;
      end if;
      State.Invalid_Region := State.Invalid_Region and Tok.Region;
   end Pop_Token;

   overriding
   procedure Merge_Tokens
     (State   : access State_Type;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     WisiToken.Token.List.Instance;
      Action  : in     Semantic_Action)
   is
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;
      use all type WisiToken.Token.List.List_Iterator;

      ID_I : WisiToken.Token.List.List_Iterator := Tokens.First;

      Aug_Nonterm : Token                         := Default_Token;
      Stack_I     : Augmented_Token_Arrays.Cursor := State.Stack.To_Cursor (State.Stack.Length - Tokens.Length + 1);
      Aug_Tokens  : Augmented_Token_Arrays.Vector;
   begin
      Aug_Nonterm.ID := Nonterm;

      loop
         exit when Is_Null (ID_I);
         declare
            ID : Token_ID renames Current (ID_I);
            Token : Token_Region.Token renames Token_Region.Token (State.Stack (Stack_I).Element.all);
         begin
            if ID /= State.Stack (Stack_I).ID then
               raise Programmer_Error;
            end if;

            if Action /= null then
               Aug_Tokens.Append (Token);
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
           (State.Trace.all, Aug_Nonterm, Index, State.Stack, Tokens.Length, Include_Action_Name => Action /= null);
      end if;

      for I in 1 .. Tokens.Length loop
         State.Stack.Delete_Last;
      end loop;

      if Action /= null then
         Action (Aug_Nonterm, Index, Aug_Tokens);
      end if;

      State.Stack.Append (Aug_Nonterm);
   end Merge_Tokens;

   overriding
   procedure Recover
     (State         : access State_Type;
      Popped_Tokens : in     WisiToken.Token_Array;
      Pushed_Tokens : in     WisiToken.Token_Array;
      Recover       : in     WisiToken.Token.Recover_Data_Access)
   is
      use all type Ada.Containers.Count_Type;
      use all type WisiToken.Token.List.List_Iterator;

      Region : Buffer_Region := State.Invalid_Region; -- discarded tokens
      Tok    : Token;
   begin
      State.Invalid_Region := Null_Buffer_Region;

      if Trace_Parse > 2 then
         Put (State);
      end if;

      for ID of Popped_Tokens loop
         Tok := Token (State.Stack.Element (State.Stack.Last_Index));
         State.Stack.Delete_Last;

         if ID /= Tok.ID then
            raise Programmer_Error;
         end if;

         Region := Region and Tok.Region;
      end loop;

      for ID of Pushed_Tokens loop
         State.Stack.Append (Token'(ID, Null_Buffer_Region));
      end loop;

      declare
         Error : Error_Data renames State.Errors.Reference (State.Errors.Last);
      begin
         Error.Invalid_Region := Region;
         Error.Recover        := Recover;
      end;

      if Trace_Parse > 2 then
         if Popped_Tokens.Length + Pushed_Tokens.Length > 0 then
            Put (State);
         end if;
      end if;
   end Recover;

end WisiToken.Token_Region;
