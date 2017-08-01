--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
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

with WisiToken.Parser.LR.McKenzie_Recover;
with WisiToken.Parser.LR.Panic_Mode;
with WisiToken.Parser.LR.Parser_Lists;
package body WisiToken.Parser.LR.Parser is

   procedure Expecting
     (Table  : in     Parse_Table_Ptr;
      State  : in     State_Index;
      Result : in out WisiToken.Token_ID_Set)
   is
      Action : Action_Node_Ptr := Table.States (State).Action_List;
   begin
      loop
         --  Last action is error; don't include it.
         exit when Action.Next = null;

         Result (Action.Symbol) := True;
         Action := Action.Next;
      end loop;
   end Expecting;

   procedure Reduce_Stack
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Semantic_State : access WisiToken.Token.Semantic_State'Class)
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
               Parser_Lists.Put (Semantic_State.Trace.all, Action_Token);
               Semantic_State.Trace.New_Line;
               Semantic_State.Trace.Put_Line
                 (" action count:" & Integer'Image (Current_Parser.Pending_Actions_Count));
            end if;
         else
            Semantic_State.Merge_Tokens (Action.LHS, Action.Index, Tokens, Action.Action);
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
      Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      if Trace_Parse > 1 then
         if Trace_Parse > 2 then
            Parser_Lists.Put_Top_10 (Trace, Current_Parser);
         end if;
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              State_Image (Current_Parser.Peek.State) & ": " &
              Image (Trace.Descriptor.all, Current_Token) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Push ((Action.State, Current_Token));

         declare
            Action_Token : constant Parser_Lists.Action_Token := (Action, Token.List.Only (Current_Token));
            Trace : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
         begin
            if Current_Parser.Active_Parser_Count > 1 then
               Current_Parser.Enqueue (Action_Token);
               if Trace_Parse > 1 then
                  Trace.Put ("pending ");
                  Parser_Lists.Put (Trace, Action_Token);
                  Trace.New_Line;
                  Trace.Put_Line (" action count:" & Integer'Image (Current_Parser.Pending_Actions_Count));
               end if;
            else
               Parser.Semantic_State.Push_Token (Current_Token);
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
      Semantic_State : access WisiToken.Token.Semantic_State'Class)
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
            Semantic_State.Push_Token (Token.List.Current (Token.List.First (Action_Token.Tokens)));

         when Reduce =>
            Semantic_State.Merge_Tokens
              (Action_Token.Action.LHS, Action_Token.Action.Index, Action_Token.Tokens, Action_Token.Action.Action);

         when Accept_It | Error =>
            raise Programmer_Error with "execute_pending; " & Parse_Action_Verbs'Image (Action_Token.Action.Verb);
         end case;

         Token.List.Clean (Action_Token.Tokens);
      end loop;
   end Execute_Pending;

   overriding procedure Parse (Parser : in out Instance)
   is
      use all type Ada.Containers.Count_Type;
      Parsers        : Parser_Lists.List := Parser_Lists.New_List
        (First_State_Index  => Parser.Table.State_First,
         First_Parser_Label => Parser.First_Parser_Label);
      Current_Verb   : Parse_Action_Verbs;
      Current_Token  : Token_ID;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;
   begin
      WisiToken.Token.Reset (Parser.Semantic_State);
      Parser.Lookahead.Clear;

      loop
         --  exit on Accept_It action or syntax error.

         Current_Verb := Parse_Verb (Parsers);

         case Current_Verb is
         when Shift =>
            if Parser.Lookahead.Length = 0 then
               Current_Token := Parser.Lexer.Find_Next;
               Parser.Semantic_State.Input_Token (Current_Token, Parser.Lexer);
            else
               --  Input_Token was called for these when read from Lexer.
               Current_Token := Parser.Lookahead (Positive_Index_Type'First);
               Parser.Lookahead.Delete_First;
            end if;

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
            --  All parsers errored; attempt recovery
            declare
               use Parser_Lists;
               Descriptor : WisiToken.Descriptor'Class renames Parser.Semantic_State.Trace.Descriptor.all;
               Expecting  : WisiToken.Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False);
               Keep_Going : Boolean                := False;
            begin
               for I in Parsers.Iterate loop
                  LR.Parser.Expecting (Parser.Table, To_Cursor (Parsers, I).Peek.State, Expecting);
               end loop;

               Parser.Semantic_State.Error (Expecting);

               if Parser.Enable_McKenzie_Recover then
                  Keep_Going := McKenzie_Recover.Recover (Parser, Parsers, Current_Token);
               end if;

               if not (Keep_Going and Parser.Enable_Panic_Recover) and then Any (Parser.Table.Panic_Recover) then
                  Keep_Going := Panic_Mode.Recover (Parser, Parsers, Current_Token);

                  if Keep_Going then
                     --  FIXME: delete or move into Panic_Mode
                     declare
                        Recover : Parser_Lists.Recover_Reference renames Parser_Lists.First (Parsers).Recover_Ref;
                     begin
                        Parser.Semantic_State.Recover (Recover.Popped_Tokens, Recover.Pushed_Tokens);
                     end;
                  end if;
               end if;

               if Trace_Parse > 0 then
                  if Keep_Going then
                     Parser.Semantic_State.Trace.Put_Line
                       ("recover: succeed, current_token " &
                          Image (Parser.Semantic_State.Trace.Descriptor.all, Current_Token) &
                          " lookahead count " & Ada.Containers.Count_Type'Image (Parser.Lookahead.Length));
                     if Parsers.Count > 1 then
                        Parser.Semantic_State.Trace.Put_Line ("recover abandoned; parsers.count > 1");
                     end if;
                  else
                     Parser.Semantic_State.Trace.Put_Line ("recover: fail");
                  end if;
               end if;

               if Keep_Going and Parsers.Count = 1 then
                  null;
                  --  FIXME: if parsers_count > 1 push recover onto pending
               else
                  --  report errors
                  declare
                     ID     : constant String := Image (Descriptor, Current_Token);
                     Lexeme : constant String := Parser.Lexer.Lexeme; --  FIXME: wrong if recover filled lookahead

                     --  More than 10 names is just confusing, and forces the rest of
                     --  the error message to overflow the GNAT exception message
                     --  limit.
                     Expecting_Names : constant String := Image (Descriptor, Expecting, Max_Count => 10);
                  begin
                     --  FIXME: Free (Parsers);
                     raise Syntax_Error with
                       Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                       ": Syntax error; expecting one of " & Expecting_Names &
                       "; found " & ID & " '" & Lexeme & "'";
                  end;
               end if;
            end;
         end case;

         Current_Parser := Parser_Lists.First (Parsers);
         loop
            exit when Current_Parser.Is_Done;

            --  All parsers reduce as much as possible, then shift
            --  Current_Token, then wait until all parsers have
            --  shifted it.

            if Current_Verb = Shift and Current_Parser.Verb = Error then
               --  This parser errored on current input, some other
               --  parser can continue with current input. This is how
               --  grammar conflicts are resolved when the input text
               --  is valid, so just terminate this parser.

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
     (Lexer                   :         in     WisiToken.Lexer.Handle;
      Table                   :         in     Parse_Table_Ptr;
      Semantic_State          : aliased in out WisiToken.Token.Semantic_State'Class;
      Max_Parallel            :         in     Integer := 15;
      First_Parser_Label      :         in     Integer := 1)
     return Instance
   is begin
      return
        (Lexer, Table, Semantic_State'Access, Empty_Token_Array,
         Max_Parallel, First_Parser_Label,
         Terminate_Same_State    => True,
         Enable_Panic_Recover    => False,
         Enable_McKenzie_Recover => False);
   end New_Parser;

end WisiToken.Parser.LR.Parser;
