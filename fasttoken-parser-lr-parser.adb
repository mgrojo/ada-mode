--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
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

with Ada.Strings.Unbounded;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser_Lists;
package body FastToken.Parser.LR.Parser is

   type Token_ID_Array is array (Integer range <>) of Token_ID;

   function Expecting (Table : in Parse_Table_Ptr; State : in State_Index) return Token_ID_Array
   is
      Action : Action_Node_Ptr := Table.States (State).Action_List;
      Count  : Integer         := 0;
   begin
      loop
         exit when Action = null;

         Count  := Count + 1;
         Action := Action.Next;
      end loop;

      --  Last action is error; don't include it.
      declare
         Result : Token_ID_Array (1 .. Count - 1);
      begin
         Action := Table.States (State).Action_List;
         for I in Result'Range loop
            Result (I) := Action.Symbol;
            Action     := Action.Next;
         end loop;
         return Result;
      end;
   end Expecting;

   function Names
     (Descriptor : in FastToken.Descriptor'Class;
      Tokens     : in Token_ID_Array)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      --  More than 10 names is just confusing, and forces the rest of
      --  the error message to overflow the GNAT exception message
      --  limit.
      for I in Tokens'First .. Integer'Min (Tokens'Last, 10) loop
         Result := Result & Image (Descriptor, Tokens (I));
         if I /= Tokens'Last then
            Result := Result & ", ";
         end if;
      end loop;
      return To_String (Result);
   end Names;

   procedure Reduce_Stack
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Semantic_State : access FastToken.Token.Semantic_State'Class)
   is
      Tokens : Token.List.Instance;
   begin
      --  Pop the token states from the stack
      for I in 1 .. Action.Token_Count loop
         Token.List.Prepend (Tokens, Current_Parser.Pop.Token);
      end loop;

      declare
         Action_Token : constant Parser_Lists.Action_Token := (Action, Tokens);
      begin
         if Current_Parser.Active_Parser_Count > 1 then
            Current_Parser.Enqueue (Action_Token);
            if Trace_Parse > 1 then
               Semantic_State.Trace.Put ("pending ");
               Parser_Lists.Put_Trace (Semantic_State.Trace.all, Action_Token);
               Semantic_State.Trace.New_Line;
               Semantic_State.Trace.Put_Line
                 (" action count:" & Integer'Image (Current_Parser.Pending_Actions_Count));
            end if;
         else
            FastToken.Token.Merge_Tokens (Action.LHS, Action.Index, Tokens, Action.Action, Semantic_State);
            --  Merge_Tokens puts a trace, with extra token info

            Token.List.Clean (Tokens);
         end if;
      end;
   end Reduce_Stack;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Current_Token  : in Token_ID;
      Parser         : in Instance)
   is
      Trace : FastToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      if Trace_Parse > 1 then
         if Trace_Parse > 2 then
            Parser_Lists.Put_Trace_Top_10 (Trace, Current_Parser);
         end if;
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              State_Image (Current_Parser.Peek.State) & ": " &
              Image (Trace.Descriptor.all, Current_Token) & " : ");
         Put_Trace (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Push ((Action.State, Current_Token));

         declare
            Action_Token : constant Parser_Lists.Action_Token := (Action, Token.List.Only (Current_Token));
            Trace : FastToken.Trace'Class renames Parser.Semantic_State.Trace.all;
         begin
            if Current_Parser.Active_Parser_Count > 1 then
               Current_Parser.Enqueue (Action_Token);
               if Trace_Parse > 1 then
                  Trace.Put ("pending ");
                  Parser_Lists.Put_Trace (Trace, Action_Token);
                  Trace.New_Line;
                  Trace.Put_Line (" action count:" & Integer'Image (Current_Parser.Pending_Actions_Count));
               end if;
            else
               FastToken.Token.Push_Token (Current_Token, Parser.Semantic_State);
            end if;
         end;

      when Reduce =>
         Reduce_Stack (Current_Parser, Action, Parser.Semantic_State);

         Current_Parser.Push
           ((State    => Goto_For
               (Table => Parser.Table.all,
                State => Current_Parser.Peek.State,
                ID    => Action.LHS),
             Token    => Action.LHS));

         if Trace_Parse > 1 then
            Trace.Put_Line (" ... goto state " & State_Image (Current_Parser.Peek.State));
         end if;

      when Accept_It =>
         Reduce_Stack
           (Current_Parser,
            (Reduce, Action.LHS, Action.Action, Action.Index, Action.Token_Count),
            Parser.Semantic_State);

      when Error =>
         null;

      end case;

      Current_Parser.Set_Verb (Action.Verb);
   end Do_Action;

   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : all Parsers.Verb return Accept, Shift, or Error - get a
   --  new token, execute Shift parsers, terminate Error parsers.
   --
   --  Reduce : some Parsers.Verb return Reduce - no new token,
   --  execute Reduce parsers.
   --
   --  Error : all Parsers.Verb return Error; report errors, terminate
   --  parse.
   function Parse_Verb (Parsers : in Parser_Lists.List) return Parse_Action_Verbs
   is
      Shift_Count  : Integer := 0;
      Accept_Count : Integer := 0;
      Error_Count  : Integer := 0;
   begin
      --  Cursor.Verb is the last action a parser took. If it was Shift,
      --  that parser used the input token, and should not be executed
      --  again until another input token is available, after all
      --  parsers have shifted the current token or terminated.
      for Cursor in Parsers.Iterate loop

         case Parser_Lists.Verb (Cursor) is
         when Shift =>
            Shift_Count := Shift_Count + 1;

         when Reduce =>
            return Reduce;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            Error_Count := Error_Count + 1;
         end case;
      end loop;

      if Parsers.Count = Accept_Count then
         return Accept_It;
      elsif Parsers.Count = Error_Count then
         return Error;
      elsif Parsers.Count = Shift_Count + Accept_Count + Error_Count then
         return Shift;
      else
         raise Programmer_Error;
      end if;
   end Parse_Verb;

   function Duplicate_State
     (Parsers        : aliased in out Parser_Lists.List;
      Current_Parser :         in     Parser_Lists.Cursor)
     return Boolean
   is
      use Parser_Lists;
   begin
      for I in Parsers.Iterate loop
         declare
            Cursor : constant Parser_Lists.Cursor := To_Cursor (Parsers, I);
         begin
            if Cursor /= Current_Parser and then Stack_Equal (Cursor, Current_Parser) then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Duplicate_State;

   procedure Execute_Pending
     (Current_Parser : in     Parser_Lists.Cursor;
      Semantic_State : access FastToken.Token.Semantic_State'Class)
   is
      Action_Token : Parser_Lists.Action_Token;
   begin
      if Trace_Parse > 1 then
         Semantic_State.Trace.Put_Line ("execute pending");
      end if;
      loop
         exit when Current_Parser.Pending_Actions_Empty;
         Action_Token := Current_Parser.Dequeue;

         case Action_Token.Action.Verb is
         when Shift =>
            Token.Push_Token (Token.List.Current (Token.List.First (Action_Token.Tokens)), Semantic_State);

         when Reduce =>
            Token.Merge_Tokens
              (Action_Token.Action.LHS, Action_Token.Action.Index, Action_Token.Tokens, Action_Token.Action.Action,
               Semantic_State);

         when Accept_It | Error =>
            raise Programmer_Error with "execute_pending; " & Parse_Action_Verbs'Image (Action_Token.Action.Verb);
         end case;

         Token.List.Clean (Action_Token.Tokens);
      end loop;
   end Execute_Pending;

   overriding procedure Parse (Parser : in out Instance)
   is
      Parsers        : Parser_Lists.List := Parser_Lists.New_List
        (First_State_Index  => Parser.Table.State_First,
         First_Parser_Label => Parser.First_Parser_Label);
      Current_Verb   : Parse_Action_Verbs;
      Current_Token  : Token_ID;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;
      Keep_Going     : Boolean;
   begin
      Parser.Skipped_Tokens.Clear;
      FastToken.Token.Reset (Parser.Semantic_State);

      loop
         --  exit on Accept_It action or syntax error.

         Current_Verb := Parse_Verb (Parsers);

         case Current_Verb is
         when Shift =>
            Current_Token := Parser.Lexer.Find_Next;
            FastToken.Token.Input_Token (Current_Token, Parser.Semantic_State, Parser.Lexer);

         when Accept_It =>
            declare
               Count : constant Integer := Parsers.Count;
            begin
               --  FIXME: Free (Parsers);
               --  Action points into the parser table; it does not get free'd.

               if Count > 1 then
                  --  Panic mode error resolution does not help with this.
                  raise Parse_Error with
                    Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                    ": Ambiguous parse:" & Integer'Image (Count) & " parsers active.";
               end if;
            end;
            return;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; attempt recovery,
            if Any (Parser.Table.Panic_Recover) then
               Keep_Going := Panic_Mode.Panic_Mode (Parser, Parsers, Current_Token);
            else
               Keep_Going := False;
            end if;

            if Keep_Going and Parsers.Count = 1 then
               declare
                  Panic : Parser_Lists.Panic_Reference renames Parser_Lists.First (Parsers).Panic_Ref;
               begin
                  FastToken.Token.Recover
                    (Panic.Popped_Tokens, Parser.Skipped_Tokens, Panic.Pushed_Token, Parser.Semantic_State);
               end;

               --  FIXME: else push panic onto pending
            else
               --  report errors
               declare
                  ID     : constant String := Image (Parser.Semantic_State.Trace.Descriptor.all, Current_Token);
                  Lexeme : constant String := Parser.Lexer.Lexeme;

                  --  FIXME: merge expecting from all active parsers
                  Expecting_Tokens : constant Token_ID_Array := Expecting (Parser.Table, Parsers.First.Peek.State);
               begin
                  --  FIXME: Free (Parsers);
                  raise Syntax_Error with
                    Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                    ": Syntax error; expecting one of " &
                    Names (Parser.Semantic_State.Trace.Descriptor.all, Expecting_Tokens) &
                    "; found " & ID & " '" & Lexeme & "'";
               end;
            end if;

         end case;

         Current_Parser := Parser_Lists.First (Parsers);
         loop
            exit when Current_Parser.Is_Done;

            --  All parsers reduce as much as possible, then shift
            --  Current_Token, then wait until all parsers have
            --  shifted it.

            if Current_Verb = Shift and Current_Parser.Verb = Error then
               if Trace_Parse > 0 then
                  Parser.Semantic_State.Trace.Put_Line
                    (Integer'Image (Current_Parser.Label) & ": terminate (" &
                       Int_Image (Parsers.Count - 1) & " active)");
               end if;
               Current_Parser.Free;

               if Parsers.Count = 1 then
                  Execute_Pending (Parsers.First, Parser.Semantic_State);
               end if;

            elsif Parser.Terminate_Same_State and then
              (Current_Verb = Shift and Duplicate_State (Parsers, Current_Parser))
            then
               if Trace_Parse > 0 then
                  Parser.Semantic_State.Trace.Put_Line
                    (Integer'Image (Current_Parser.Label) & ": duplicate state; terminate (" &
                       Int_Image (Parsers.Count - 1) & " active)");
               end if;
               Current_Parser.Free;

               if Parsers.Count = 1 then
                  Execute_Pending (Parsers.First, Parser.Semantic_State);
               end if;

            elsif Current_Parser.Verb = Current_Verb then

               Action := Action_For
                 (Table => Parser.Table.all,
                  State => Current_Parser.Peek.State,
                  ID    => Current_Token);

               if Action.Next /= null then
                  --  conflict; spawn a new parser
                  if Parsers.Count = Parser.Max_Parallel then
                     raise Parse_Error with
                       Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                       ": too many parallel parsers required in grammar state" &
                       State_Index'Image (Current_Parser.Peek.State) &
                       "; simplify grammar, or increase max-parallel (" &
                       Integer'Image (Parser.Max_Parallel) & ")";

                  else
                     if Trace_Parse > 0 then
                        Parser.Semantic_State.Trace.Put
                          ("spawn parser from " & Int_Image (Current_Parser.Label));
                     end if;
                     Parsers.Prepend_Copy (Current_Parser);
                     if Trace_Parse > 0 then
                        Parser.Semantic_State.Trace.Put_Line (" (" & Int_Image (Parsers.Count) & " active)");
                     end if;
                     Do_Action (Action.Next.Item, Parsers.First, Current_Token, Parser);
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Current_Token, Parser);

               Current_Parser.Next;
            else
               Current_Parser.Next;
            end if;
         end loop;
      end loop;
   end Parse;

   function New_Parser
     (Lexer                :         in     Lexer_Pkg.Handle;
      Table                :         in     Parse_Table_Ptr;
      Semantic_State       : aliased in out FastToken.Token.Semantic_State'Class;
      Max_Parallel         :         in     Integer := 15;
      First_Parser_Label   :         in     Integer := 1;
      Terminate_Same_State :         in     Boolean := False)
     return Instance
   is begin
      return
        (Lexer, Table, Semantic_State'Access, Token_Pkg.List.Null_List,
         Max_Parallel, First_Parser_Label, Terminate_Same_State);
   end New_Parser;

end FastToken.Parser.LR.Parser;
