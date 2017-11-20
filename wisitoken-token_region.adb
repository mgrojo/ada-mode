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

   overriding
   function Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor'Class;
      ID_Only    : in Boolean)
     return String
   is
      use all type Ada.Text_IO.Count;
      Name : constant String := WisiToken.Image (Descriptor, Item.ID);
   begin
      if ID_Only then
         --  No parens for consistency with previous unit test results.
         return Name;

      elsif Item.Line /= Invalid_Line_Number then
         return "(" & Name &
           Line_Number_Type'Image (Item.Line) & ":" & Int_Image (Integer (Item.Col)) & ")";

      elsif Item.Char_Region = Null_Buffer_Region then
         return "(" & Name & ")";

      else
         --  For test result backward compatiblity, we don't call Image
         --  (Item.Region) here
         return "(" & Name &
           Buffer_Pos'Image (Item.Char_Region.First) & " ." & Buffer_Pos'Image (Item.Char_Region.Last) & ")";
      end if;
   end Image;

   procedure Put
     (Trace     : in out WisiToken.Trace'Class;
      Stack     : in     Augmented_Token_Array;
      Count     : in     Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
      Top_First : in     Boolean                   := True)
   is
      --  Put top Count items on Stack; all if Count_Type'Last.
      --  Top is Stack.Last_Index
      use Augmented_Token_Arrays;
      use Ada.Containers;

      First : constant Count_Type := (if Count = Ada.Containers.Count_Type'Last then 1 else Stack.Length - Count + 1);
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
      Nonterm             : in     Token'Class;
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
      Trace.Put (Action_Name & Nonterm.Image (Trace.Descriptor.all, ID_Only => False) & " <= ");
      Put (Trace, Stack, Count => Tokens_Length, Top_First => False);
      Trace.New_Line;
   end Put;

   ----------
   --  Public subprograms, declaration order

   overriding
   procedure Initialize (State : not null access State_Type; Init : in WisiToken.Token.Init_Data'Class)
   is
      pragma Unreferenced (Init);
   begin
      State.Reset;
   end Initialize;

   overriding
   procedure Reset (State : not null access State_Type; Init_Done : in Boolean := False)
   is
      pragma Unreferenced (Init_Done);
   begin
      State.Stack.Clear;
      State.Lookahead_Queue.Clear;

      --  WORKAROUND: GNAT GPL 2016: just state.errors.clear leaves some
      --  info in state.errors(0) that comes back when we do Set_Length 2.
      for List of State.Errors loop
         List.Clear;
      end loop;
      State.Errors.Clear;
   end Reset;

   function Active_Error_List (State : not null access State_Type) return Error_List_Arrays.Constant_Reference_Type
   is
      use all type Ada.Containers.Count_Type;
      use Error_List_Arrays;
      Cursor : Error_List_Arrays.Cursor := State.Errors.First;

      Active_Count : Integer := 0;
      Active_Index : Natural;
   begin
      loop
         exit when Cursor = No_Element;
         if Has_Element (Cursor)  and then
           Element (Cursor).Length > 0
         then
            Active_Count := Active_Count + 1;
            Active_Index := To_Index (Cursor);
         end if;
         Next (Cursor);
      end loop;

      if Active_Count = 1 then
         return Constant_Reference (State.Errors, Active_Index);
      else
         raise Programmer_Error;
      end if;
   end Active_Error_List;

   procedure Put
     (Source_File_Name : in String;
      Errors           : in Error_List_Arrays.Vector;
      Descriptor       : in WisiToken.Descriptor'Class)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
   begin
      for I in Errors.First_Index .. Errors.Last_Index loop
         if Errors (I).Length > 0 then
            Put_Line ("parser" & Integer'Image (I) & " errors:");
            for Item of Errors (I) loop
               if Item.Error_Token.Line = Invalid_Line_Number then
                  Put_Line
                    (Source_File_Name & ": syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                       ", found '" & Item.Error_Token.Image (Descriptor, ID_Only => False) & "'");
               else
                  Put_Line
                    (Error_Message
                       (Source_File_Name, Item.Error_Token.Line, Item.Error_Token.Col,
                        "syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                          ", found '" & Item.Error_Token.Image (Descriptor, ID_Only => True) & "'"));
               end if;

               if Item.Recover /= null then
                  Put_Line ("   recover: " & Item.Recover.Image (Descriptor));
               end if;
            end loop;
            New_Line;
         end if;
      end loop;
   end Put;

   overriding
   procedure Put (State : not null access State_Type)
   is
   begin
      State.Trace.Put ("semantic state: stack: ");
      Put (State.Trace.all, State.Stack);
      State.Trace.New_Line;
      State.Trace.Put ("semantic state: lookahead queue: ");
      Put (State.Trace.all, State.Lookahead_Queue);
      State.Trace.New_Line;
   end Put;

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'class)
   is
      use all type SAL.Base_Peek_Type;
      Temp : constant Token :=
        (ID,
         Virtual     => False,
         Line        => Lexer.Line,
         Col         => Lexer.Column,
         Char_Region => Lexer.Char_Region,
         Byte_Region => Lexer.Byte_Region);
   begin
      State.Lookahead_Queue.Put (Temp);

      if Trace_Parse > 2 then
         State.Trace.Put_Line
           ("lexer_to_lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Lexer_To_Lookahead;

   overriding
   procedure Error
     (State     : not null access State_Type;
      Parser_ID : in     Natural;
      Expecting : in     Token_ID_Set)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Parser_ID > State.Errors.Last_Index then
         State.Errors.Set_Length (Ada.Containers.Count_Type (Parser_ID + 1)); -- Parser_ID is 0 indexed.
         State.Errors.Replace_Element (Parser_ID, Error_Data_Lists.Empty_List);
      end if;

      declare
         Error_List : Error_Data_Lists.List renames State.Errors.Reference (Parser_ID).Element.all;
      begin
         Error_List.Append
           ((First_Terminal => State.Trace.Descriptor.First_Terminal,
             Last_Terminal  => State.Trace.Descriptor.Last_Terminal,
             Error_Token    => Token (State.Lookahead_Queue.Peek (State.Lookahead_Queue.Count).Element.all),
             Expecting      => Expecting,

             --  The following is set in Recover
             Recover => null));
      end;
   end Error;

   overriding
   procedure Spawn
     (State     : not null access State_Type;
      Old_Parser_ID : in              Natural;
      New_Parser_ID : in              Natural)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if New_Parser_ID > State.Errors.Last_Index then
         State.Errors.Set_Length (Ada.Containers.Count_Type (New_Parser_ID + 1)); -- Parser_ID is 0 indexed.
         State.Errors.Replace_Element (New_Parser_ID, Error_Data_Lists.Empty_List);
      end if;

      declare
         Old_Error_List : Error_Data_Lists.List renames State.Errors.Reference (Old_Parser_ID).Element.all;
         New_Error_List : Error_Data_Lists.List renames State.Errors.Reference (New_Parser_ID).Element.all;
      begin
         New_Error_List := Old_Error_List;
      end;
   end Spawn;

   overriding
   procedure Terminate_Parser
     (State     : not null access State_Type;
      Parser_ID : in     Natural)
   is begin
      --  We don't use Delete because that slides element above Parser_ID,
      --  changing there indices.
      State.Errors (Parser_ID).Clear;
   end Terminate_Parser;

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in     Token_ID)
   is
      Temp : constant Token :=
        (ID,
         Virtual     => True,
         Line        => Invalid_Line_Number,
         Col         => 0,
         Char_Region => Null_Buffer_Region,
         Byte_Region => Null_Buffer_Region);
   begin
      State.Lookahead_Queue.Add_To_Head (Temp);

      if Trace_Parse > 2 then
         State.Trace.Put_Line
           ("virtual_to_lookahead: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Virtual_To_Lookahead;

   overriding
   procedure Push_Current
     (State : not null access State_Type;
      ID    : in     Token_ID)
   is
      Temp : constant WisiToken.Augmented_Token'Class := State.Lookahead_Queue.Get;
   begin
      if ID /= Temp.ID then
         raise Programmer_Error with "token_region.push_current: ID " &
           Image (State.Trace.Descriptor.all, ID) &
           ", Token " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False);
      end if;

      State.Stack.Append (Temp);

      if Trace_Parse > 2 then
         State.Trace.Put_Line
           ("push_current: " & Temp.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Push_Current;

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      IDs     : in     WisiToken.Token_Array;
      Action  : in     Semantic_Action)
   is
      use all type Ada.Containers.Count_Type;
      use all type Augmented_Token_Arrays.Cursor;

      Aug_Nonterm : Token;
      Stack_I     : Augmented_Token_Arrays.Cursor := State.Stack.To_Cursor (State.Stack.Length - IDs.Length + 1);
      Aug_Tokens  : Augmented_Token_Arrays.Vector;
   begin
      Aug_Nonterm.ID := Nonterm;

      for I in IDs.First_Index .. IDs.Last_Index loop
         declare
            use all type Ada.Text_IO.Count;
            ID    : Token_ID renames IDs.Element (I);
            Token : Token_Region.Token renames Token_Region.Token (State.Stack (Stack_I).Element.all);
         begin
            if ID /= State.Stack (Stack_I).ID then
               raise Programmer_Error;
            end if;

            if Action /= null then
               Aug_Tokens.Append (Token);
            end if;

            if Aug_Nonterm.Line = Invalid_Line_Number and Token.Line /= Invalid_Line_Number then
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
         end;

         Next (Stack_I);
      end loop;

      if Trace_Parse > 0 then
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
   procedure Discard_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID)
   is
      Token : constant Token_Region.Token := Token_Region.Token (State.Lookahead_Queue.Get);
   begin
      if ID /= Token.ID then
         raise Programmer_Error with "token_region.discard_lookahead: ID " &
           Image (State.Trace.Descriptor.all, ID) &
           ", Token " & Token.Image (State.Trace.Descriptor.all, ID_Only => False);
      end if;

      if Trace_Parse > 2 then
         State.Trace.Put_Line ("discard_lookahead: " & Token.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Discard_Lookahead;

   overriding
   procedure Discard_Stack
     (State : not null access State_Type;
      ID    : in              Token_ID)
   is
      Token : constant Token_Region.Token := Token_Region.Token
        (Augmented_Token_Arrays.Element (State.Stack.Last));
   begin
      State.Stack.Delete_Last;

      if ID /= Token.ID then
         raise Programmer_Error;
      end if;
      if Trace_Parse > 2 then
         State.Trace.Put_Line ("discard_stack: " & Token.Image (State.Trace.Descriptor.all, ID_Only => False));
      end if;
   end Discard_Stack;

   overriding
   procedure Recover
     (State     : not null access State_Type;
      Parser_ID : in     Natural;
      Recover   : in     WisiToken.Token.Recover_Data'Class)
   is
      Error_List : Error_Data_Lists.List renames State.Errors.Reference (Parser_ID).Element.all;
      Error      : Error_Data renames Error_List.Reference (Error_List.Last);
   begin
      Error.Recover := new WisiToken.Token.Recover_Data'Class'(Recover);
      if Trace_Parse > 2 then
         State.Trace.Put_Line (Natural'Image (Parser_ID) & ": recover");
      end if;
   end Recover;

end WisiToken.Token_Region;
