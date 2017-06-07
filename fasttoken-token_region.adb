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
package body FastToken.Token_Region is

   function Image
     (Descriptor : in FastToken.Descriptor'Class;
      Item       : in Token;
      ID_Only    : in Boolean)
     return String
   is
      Name : constant String := FastToken.Image (Descriptor, Item.ID);
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

   procedure Put_Trace
     (Trace               : in out FastToken.Trace'Class;
      Nonterm             : in     Token;
      Index               : in     Natural;
      Stack               : in     Token_Stacks.Vector;
      Tokens_Length       : in     Ada.Containers.Count_Type;
      Include_Action_Name : in     Boolean)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Characters.Handling;
      use Token_Stacks;

      Action_Name : constant String :=
        (if Include_Action_Name
         then To_Lower (Image (Trace.Descriptor.all, Nonterm.ID)) & "_" & FastToken.Int_Image (Index) & ": "
         else "");

      I : Cursor := Stack.To_Cursor (Stack.Length - Tokens_Length + 1);
   begin
      Trace.Put (Action_Name & Image (Trace.Descriptor.all, Nonterm, ID_Only => False) & " <= ");
      loop
         exit when I = No_Element;

         Trace.Put (Image (Trace.Descriptor.all, Token (Stack (I).Element.all), ID_Only => False));
         Next (I);
         if I /= No_Element then
            Trace.Put (", ");
         end if;
      end loop;
      Trace.New_Line;
   end Put_Trace;

   overriding
   procedure Reset (State : access State_Type)
   is begin
      State.Stack.Clear;
      State.Pending_Input.Clear;
      State.Invalid_Regions.Clear;
   end Reset;

   overriding
   procedure Input_Token
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle)
   is begin
      State.Pending_Input.Put ((Token, Lexer.Bounds));
   end Input_Token;

   overriding
   procedure Push_Token
     (ID    : in     Token_ID;
      State : access State_Type)
   is
      Tok : constant Token := State.Pending_Input.Get;
   begin
      if ID /= Tok.ID then
         raise Programmer_Error;
      end if;

      State.Stack.Append (Tok);
   end Push_Token;

   overriding
   procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     FastToken.Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type)
   is
      use all type Ada.Containers.Count_Type;
      use all type Token_Stacks.Cursor;
      use all type FastToken.Token.List.List_Iterator;

      ID_I : FastToken.Token.List.List_Iterator := Tokens.First;

      Aug_Nonterm : Token               := Default_Token;
      Stack_I     : Token_Stacks.Cursor := State.Stack.To_Cursor (State.Stack.Length - Tokens.Length + 1);
      Aug_Tokens  : Token_Stacks.Vector;
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
               Token_Stacks.Append (Aug_Tokens, Token);
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
         Put_Trace
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
     (Popped_Tokens  : in     FastToken.Token.List.Instance;
      Skipped_Tokens : in     FastToken.Token.List.Instance;
      Pushed_Token   : in     Token_ID;
      State          : access State_Type)
   is
      use all type FastToken.Token.List.List_Iterator;

      Region : Buffer_Region                      := Null_Buffer_Region;
      I      : FastToken.Token.List.List_Iterator := Popped_Tokens.First;
      Tok    : Token;
   begin
      loop
         exit when Is_Null (I);
         Tok := Token (State.Stack.Element (State.Stack.Last_Index));
         State.Stack.Delete_Last;

         if ID (I) /= Tok.ID then
            raise Programmer_Error;
         end if;

         Region := Region and Tok.Region;
         Next (I);
      end loop;

      I := Skipped_Tokens.First;
      loop
         exit when Is_Null (I);
         Tok := State.Pending_Input.Get;

         if ID (I) /= Tok.ID then
            raise Programmer_Error;
         end if;

         Region := Region and Tok.Region;
         Next (I);
      end loop;

      State.Stack.Append (Token'(Pushed_Token, Region));
      State.Invalid_Regions.Append (Region);
   end Recover;

end FastToken.Token_Region;
