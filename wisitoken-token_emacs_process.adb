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

with Ada.Strings.Bounded;
with Ada.Text_IO;
package body WisiToken.Token_Emacs_Process is

   ----------
   --  body subprograms

   --  See [1]
   Input_Token_Code   : constant String := "0 ";
   Push_Token_Code    : constant String := "1 ";
   Discard_Input_Code : constant String := "2 ";
   Error_Code         : constant String := "3 ";
   Merge_Tokens_Code  : constant String := "4 ";
   Recover_Code       : constant String := "5 ";

   function To_Code (ID : in Token_ID) return String
   is begin
      return Int_Image (ID);
   end To_Code;

   function To_Codes (Tokens : in Token_ID_Set) return String
   is
      --  A token image consists of:
      --
      --  ID - int
      Chars_Per_Token : constant Integer := Integer'Width;

      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 2 + Integer (Tokens'Length) * Chars_Per_Token);
      use Bounded;

      Token_Line : Bounded_String := To_Bounded_String ("[");
   begin
      for I in Tokens'Range loop
         if Tokens (I) then
            Token_Line := Token_Line & Token_ID'Image (I);
         end if;
      end loop;
      return To_String (Token_Line & "]");
   end To_Codes;

   function To_Codes (Tokens : in Token.List.Instance) return String
   is
      use Ada.Text_IO; -- Count
      use Token.List;
      --  A token image consists of:
      --
      --  ID - int
      Chars_Per_Token : constant Integer := Integer'Width;

      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 2 + Integer (Tokens.Length) * Chars_Per_Token);
      use Bounded;

      I          : List_Iterator  := Tokens.First;
      Token_Line : Bounded_String := To_Bounded_String ("[");
   begin
      loop
         exit when I = Null_Iterator;
         Token_Line := Token_Line & Integer'Image (Token_ID'Pos (ID (I)));
         Next (I);
      end loop;
      return To_String (Token_Line & "]");
   end To_Codes;

   ----------
   --  Spec visible subrograms

   overriding
   procedure Input_Token
     (State : access State_Type;
      Token : in     Token_ID;
      Lexer : in     WisiToken.Lexer.Handle)
   is
      use all type WisiToken.Lexer.Handle;
   begin
      --  If Lexer is non-null, the elisp lexer has already
      --  put the tokens in the state input queue.
      --
      --  If Lexer is null, the token was inserted by error recovery, so
      --  it is sent to the elisp process.

      if Trace_Parse > 3 then
         Put (State.Trace.all, Token);
      end if;
      if Lexer = null then
         Ada.Text_IO.Put_Line ("[" & Input_Token_Code & To_Code (Token) & "]");
      end if;
   end Input_Token;

   overriding
   procedure Push_Token
     (State : access State_Type;
      Token : in     Token_ID)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Push_Token_Code & To_Code (Token) & "]");
   end Push_Token;

   overriding
   procedure Error
     (State     : access State_Type;
      Expecting : in     Token_ID_Set)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Error_Code & To_Codes (Expecting) & "]");
   end Error;

   overriding
   procedure Discard_Input
     (State : access State_Type;
      Token : in     Token_ID)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Discard_Input_Code & To_Code (Token) & "]");
   end Discard_Input;

   overriding
   procedure Discard_Lookahead
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Pop_Token
     (State : access State_Type;
      Token : in     Token_ID);

   overriding
   procedure Merge_Tokens
     (State   : access State_Type;
      Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action)
   is
      pragma Unreferenced (State, Action);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Merge_Tokens_Code & To_Code (Nonterm) & To_Codes (Tokens) & Integer'Image (Index) & "]");
   end Merge_Tokens;

   overriding
   procedure Recover
     (State         : access State_Type;
      Popped_Tokens : in     Token.List.Instance;
      Pushed_Tokens : in     Token.List.Instance;
      Recover       : in     WisiToken.Token.Recover_Data_Access)
   is
      pragma Unreferenced (Recover);
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Recover_Code & To_Codes (Popped_Tokens) & To_Codes (Pushed_Tokens) & "]");
      --  FIXME: send Recover
   end Recover;

end WisiToken.Token_Emacs_Process;
