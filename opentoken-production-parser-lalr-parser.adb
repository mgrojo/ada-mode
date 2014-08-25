--  Copyright (C) 2002 - 2005, 2008 - 2014 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
package body OpenToken.Production.Parser.LALR.Parser is

   --  The following types are used for the Parser's stack. The stack
   --  contains the tokens that have been read or derived, and the
   --  parser states in which that occurred.

   type State_Node;
   type State_Node_Ptr is access State_Node;

   type State_Node is record
      State      : State_Index;
      Seen_Token : Token.Handle;
      Next       : State_Node_Ptr;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (State_Node, State_Node_Ptr);

   --  Return the action for the given state index and terminal ID.
   --  The final action in the action list for a state is returned if no
   --  other node matches ID.
   function Action_For
     (Table : in Parse_Table_Ptr;
      State : in State_Index;
      ID    : in Tokenizer.Terminal_ID)
     return Parse_Action_Rec
   is
      use type Tokenizer.Terminal_ID;
      Action_Node : Action_Node_Ptr := Table.all (State).Action_List;
   begin
      while Action_Node.Next /= null and Action_Node.Symbol /= ID loop
         Action_Node := Action_Node.Next;
      end loop;

      if Action_Node.Action.Next = null then
         return Action_Node.Action.Item;
      else
         raise Parse_Error with "conflicting actions in state" & State_Index'Image (State);
      end if;
   end Action_For;

   function Goto_For
     (Table : in Parse_Table_Ptr;
      State : in State_Index;
      ID    : in Token.Token_ID)
     return State_Index
   is
      use type Tokenizer.Terminal_ID;
      Goto_Node : Goto_Node_Ptr := Table.all (State).Goto_List;
   begin
      while Goto_Node.Next /= null and Goto_Node.Symbol /= ID loop
         Goto_Node := Goto_Node.Next;
      end loop;

      return Goto_Node.State;
   end Goto_For;

   type Token_Array is array (Integer range <>) of Token.Token_ID;

   function Expecting (Table : in Parse_Table_Ptr; State : in State_Index) return Token_Array
   is
      Action : Action_Node_Ptr := Table (State).Action_List;
      Count  : Integer         := 0;
   begin
      loop
         exit when Action = null;

         Count  := Count + 1;
         Action := Action.Next;
      end loop;

      --  Last action is error; don't include it.
      declare
         Result : Token_Array (1 .. Count - 1);
      begin
         Action := Table (State).Action_List;
         for I in Result'Range loop
            Result (I) := Action.Symbol;
            Action     := Action.Next;
         end loop;
         return Result;
      end;
   end Expecting;

   function Names (Analyzer : in Tokenizer.Instance; Tokens : in Token_Array) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in Tokens'Range loop
         Result := Result & "'" & Tokenizer.Name (Analyzer, Tokens (I));
         if I = Tokens'Last then
            Result := Result & "'";
         else
            Result := Result & "' or ";
         end if;

      end loop;
      return To_String (Result);
   end Names;

   procedure Reduce_Stack
     (Stack  : in out State_Node_Ptr;
      LHS         : in Nonterminal.Handle;
      Action      : in Nonterminal.Synthesize;
      Token_Count : in Natural)
   is
      use type Nonterminal.Synthesize;

      Arguments    : Token_List.Instance;
      Popped_State : State_Node_Ptr;
      Args_Added   : Natural := 0;

      New_Token : constant Nonterminal.Handle := new Nonterminal.Class'(LHS.all);
   begin
      --  Pop the indicated number of token states from the stack, and
      --  call the production action routine to create a new
      --  nonterminal token.
      --
      --  Leave Stack.State containing post-reduce state and produced
      --  token (after action call).

      --  Build the argument list, while popping all but the last
      --  argument's state off of the stack.
      if Token_Count > 0 then
         loop
            Token_List.Enqueue (Arguments, Stack.Seen_Token);

            Args_Added := Args_Added + 1;
            exit when Args_Added = Token_Count;

            --  Leave the state containing the first token in
            --  Production on Stack; overwrite it with the produced
            --  token
            Popped_State := Stack;
            Stack        := Stack.Next;
            Free (Popped_State);
         end loop;
         Stack.State      := Stack.Next.State;
         Stack.Seen_Token := Token.Handle (New_Token);
      else
         --  Empty production; push a new item on the stack.
         Stack := new State_Node'
           (State      => Stack.State,
            Seen_Token => Token.Handle (New_Token),
            Next       => Stack);
      end if;

      Action (New_Token.all, Arguments, Token.ID (LHS.all));
      Token_List.Clean (Arguments);

   end Reduce_Stack;

   overriding procedure Parse (Parser : in out Instance)
   is
      Stack        : State_Node_Ptr := new State_Node; -- Stack is current state, stack.next is prev state, etc.
      Seen_Token   : Token.Handle;
      Action       : Parse_Action_Rec;
      Popped_State : State_Node_Ptr;

      use type Token_List.Instance;
   begin

      --  Get the first token from the analyzer
      begin
         Tokenizer.Find_Next (Parser.Analyzer);
      exception
      when E : Syntax_Error =>
         raise Syntax_Error with
           Int_Image (Line (Parser)) &
           ":" &
           Int_Image (Column (Parser) - 1) &
           " " &
           Ada.Exceptions.Exception_Message (E);
      end;

      Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

      Stack.State := State_Index'First;
      loop
         Action := Action_For
           (Table => Parser.Table,
            State => Stack.State,
            ID    => Token.ID (Seen_Token.all));

         if Trace_Parse then
            declare
               use type Token.Handle;
               Stack_I : State_Node_Ptr := Stack;
            begin
               for I in 1 .. 10 loop
                  exit when Stack_I = null;
                  Ada.Text_IO.Put_Line
                    (State_Index'Image (Stack_I.State) & " : " &
                       (if Stack_I.Seen_Token = null then ""
                         else Token.Token_Image (Token.ID (Stack_I.Seen_Token.all))));
                  Stack_I := Stack_I.Next;
               end loop;
            end;
            Ada.Text_IO.Put
              (State_Index'Image (Stack.State) & " : " &
                 Token.Token_Image (Token.ID (Seen_Token.all)) & " : " &
                 Parse_Action_Verbs'Image (Action.Verb));
         end if;

         case Action.Verb is
         when Shift =>
            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            Stack := new State_Node'
              (State      => Action.State,
               Seen_Token => Seen_Token,
               Next       => Stack);

            begin
               Tokenizer.Find_Next (Parser.Analyzer);
            exception
            when E : Syntax_Error =>
               raise Syntax_Error with
                 Int_Image (Line (Parser)) &
                 ":" &
                 Int_Image (Column (Parser) - 1) &
                 " " &
                 Ada.Exceptions.Exception_Message (E);
            end;

            Seen_Token := new Token.Class'(Token.Class (Tokenizer.Get (Parser.Analyzer)));

         when Reduce =>

            Reduce_Stack (Stack, Action.LHS, Action.Action, Action.Token_Count);

            Stack.State := Goto_For
              (Table => Parser.Table,
               State => Stack.State,
               ID    => Token.ID (Action.LHS.all));

            if Trace_Parse then
               Ada.Text_IO.Put_Line
                 (Integer'Image (Action.Token_Count) & " tokens to " &
                    Token.Token_Image (Token.ID (Action.LHS.all)) &
                    ", goto state" & State_Index'Image (Stack.State));
            end if;

         when Accept_It =>

            Reduce_Stack (Stack, Action.LHS, Action.Action, Action.Token_Count);

            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Clean up
            Token.Free (Stack.Seen_Token);
            while Stack /= null loop
               Popped_State := Stack;
               Stack := Stack.Next;
               Token.Free (Popped_State.Seen_Token);
               Free (Popped_State);
            end loop;

            return;

         when Error =>
            if Trace_Parse then
               Ada.Text_IO.New_Line;
            end if;

            --  Clean up
            declare
               ID     : constant String := Token.Name (Seen_Token.all);
               Lexeme : constant String := Tokenizer.Lexeme (Parser.Analyzer);

               Expecting_Tokens : constant Token_Array := Expecting (Parser.Table, Stack.State);
            begin

               Token.Free (Stack.Seen_Token);
               while Stack /= null loop
                  Popped_State := Stack;
                  Stack := Stack.Next;
                  Token.Free (Popped_State.Seen_Token);
                  Free (Popped_State);
               end loop;

               raise Syntax_Error with
                 Int_Image (Line (Parser)) &
                 ":" &
                 Int_Image (Column (Parser) - 1) &
                 ": Syntax error; expecting " &
                 Names (Parser.Analyzer, Expecting_Tokens) &
                 "; found " &
                 ID &
                 " '" &
                 Lexeme &
                 "'";
            end;
         end case;

      end loop;
   end Parse;

   function Initialize
     (Table    : in Parse_Table_Ptr;
      Analyzer : in Tokenizer.Instance)
     return Instance
   is begin
      return (Analyzer, Table);
   end Initialize;

   function Int_Image (Item : in Integer) return String
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return Trim (Integer'Image (Item), Both);
   end Int_Image;

end OpenToken.Production.Parser.LALR.Parser;
