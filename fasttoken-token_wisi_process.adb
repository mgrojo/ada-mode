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
package body FastToken.Token_Wisi_Process is

   ----------
   --  body subprograms

   --  See [1]
   Push_Token_Code    : constant String := "1 ";
   Discard_Token_Code : constant String := "2 ";
   Merge_Tokens_Code  : constant String := "3 ";
   Recover_Code       : constant String := "4 ";

   function To_Code (ID : in Token_ID) return String
   is begin
      return Int_Image (ID);
   end To_Code;

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
     (Token : in     Token_ID;
      State : access State_Type;
      Lexer : in     FastToken.Lexer.Handle)
   is
      pragma Unreferenced (Lexer);
   begin
      if Trace_Parse > 3 then
         Put_Trace (State.Trace.all, Token);
      end if;
   end Input_Token;

   overriding
   procedure Push_Token
     (Token : in     Token_ID;
      State : access State_Type)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Push_Token_Code & To_Code (Token) & "]");
   end Push_Token;

   overriding
   procedure Discard_Token
     (Token : in     Token_ID;
      State : access State_Type)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Discard_Token_Code & To_Code (Token) & "]");
   end Discard_Token;

   overriding
   procedure Merge_Tokens
     (Nonterm : in     Token_ID;
      Index   : in     Natural;
      Tokens  : in     Token.List.Instance;
      Action  : in     Semantic_Action;
      State   : access State_Type)
   is
      pragma Unreferenced (State, Action);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Merge_Tokens_Code & To_Code (Nonterm) & To_Codes (Tokens) & Integer'Image (Index) & "]");
   end Merge_Tokens;

   overriding
   procedure Recover
     (Popped_Tokens  : in     Token.List.Instance;
      Pushed_Token   : in     Token_ID;
      State          : access State_Type)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Recover_Code & To_Codes (Popped_Tokens) & To_Code (Pushed_Token) & "]");
   end Recover;

end FastToken.Token_Wisi_Process;
