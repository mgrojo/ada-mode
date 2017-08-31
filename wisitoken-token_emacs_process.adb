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
with WisiToken.Parser.LR.McKenzie_Recover;
package body WisiToken.Token_Emacs_Process is

   ----------
   --  body subprograms

   --  See [1]
   Lexer_To_Lookahead_Code   : constant String := "1 ";
   Error_Code                : constant String := "2 ";
   Spawn_Code                : constant String := "3 ";
   Terminate_Parser_Code     : constant String := "4 ";
   Virtual_To_Lookahead_Code : constant String := "5 ";
   Push_Current_Code         : constant String := "6 ";
   Reduce_Stack_Code         : constant String := "7 ";
   Discard_Lookahead_Code    : constant String := "8 ";
   Discard_Stack_Code        : constant String := "9 ";
   Recover_Code              : constant String := "10 ";

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

      Token_Line : Bounded_String := To_Bounded_String (" [");
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

   function To_Codes (Tokens : in Token_Array) return String
   is
      --  A token image consists of:
      --
      --  ID - int
      Chars_Per_Token : constant Integer := Integer'Width;

      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 2 + Integer (Tokens.Length) * Chars_Per_Token);
      use Bounded;

      Token_Line : Bounded_String := To_Bounded_String ("[");
   begin
      for ID of Tokens loop
         Token_Line := Token_Line & Integer'Image (Token_ID'Pos (ID));
      end loop;
      return To_String (Token_Line & "]");
   end To_Codes;

   ----------
   --  Spec visible subrograms

   overriding
   procedure Lexer_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID;
      Lexer : not null access WisiToken.Lexer.Instance'Class)
   is
      pragma Unreferenced (Lexer);
   begin
      if Trace_Parse > 3 then
         Put (State.Trace.all, ID);
      end if;
      Ada.Text_IO.Put_Line ("[" & Lexer_To_Lookahead_Code & To_Code (ID) & "]");
   end Lexer_To_Lookahead;

   overriding
   procedure Error
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Expecting : in              Token_ID_Set)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Error_Code & Integer'Image (Parser_ID) & To_Codes (Expecting) & "]");
   end Error;

   overriding
   procedure Spawn
     (State         : not null access State_Type;
      Old_Parser_ID : in              Natural;
      New_Parser_ID : in              Natural)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Spawn_Code & Integer'Image (Old_Parser_ID) & Integer'Image (New_Parser_ID) & "]");
   end Spawn;

   overriding
   procedure Terminate_Parser
     (State     : not null access State_Type;
      Parser_ID : in              Natural)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Terminate_Parser_Code & Integer'Image (Parser_ID) & "]");
   end Terminate_Parser;

   overriding
   procedure Virtual_To_Lookahead
     (State : not null access State_Type;
      ID    : in              Token_ID)
   is begin
      if Trace_Parse > 3 then
         Put (State.Trace.all, ID);
      end if;
      Ada.Text_IO.Put_Line ("[" & Virtual_To_Lookahead_Code & To_Code (ID) & "]");
   end Virtual_To_Lookahead;

   overriding
   procedure Push_Current
     (State : not null access State_Type;
      ID    : in              Token_ID)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Push_Current_Code & To_Code (ID) & "]");
   end Push_Current;

   overriding
   procedure Reduce_Stack
     (State   : not null access State_Type;
      Nonterm : in              Token_ID;
      Index   : in              Natural;
      IDs     : in              Token.List.Instance;
      Action  : in              Semantic_Action)
   is
      pragma Unreferenced (State, Action);
   begin
      Ada.Text_IO.Put_Line
        ("[" & Reduce_Stack_Code & To_Code (Nonterm) & To_Codes (IDs) & Integer'Image (Index) & "]");
   end Reduce_Stack;

   overriding
   procedure Discard_Lookahead
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Discard_Lookahead_Code & Integer'Image (Parser_ID) & " " & To_Code (ID) & "]");
   end Discard_Lookahead;

   overriding
   procedure Discard_Stack
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      ID        : in              Token_ID)
   is
      pragma Unreferenced (State);
   begin
      Ada.Text_IO.Put_Line ("[" & Discard_Stack_Code & Integer'Image (Parser_ID) &  " " & To_Code (ID) & "]");
   end Discard_Stack;

   overriding
   procedure Recover
     (State     : not null access State_Type;
      Parser_ID : in              Natural;
      Recover   : in              WisiToken.Token.Recover_Data'Class)
   is
      pragma Unreferenced (State);
      use WisiToken.Parser.LR.McKenzie_Recover;
      use all type Token_Arrays.Vector;
   begin
      if Recover in Configuration'Class then
         declare
            Config : Configuration renames Configuration (Recover);
            Pushed : constant Token_Array := WisiToken.Parser.LR.Extract_IDs (Config.Pushed);
         begin
            Ada.Text_IO.Put_Line
              ("[" & Recover_Code & Integer'Image (Parser_ID) & To_Codes (Pushed & Config.Inserted) & "]");
         end;
      else
         Ada.Text_IO.Put_Line ("[" & Recover_Code & Integer'Image (Parser_ID) & "[]]");
      end if;
   end Recover;

end WisiToken.Token_Emacs_Process;
