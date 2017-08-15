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
      use all type Ada.Containers.Count_Type;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Tokens       : Token.List.Instance;
   begin
      for I in 1 .. Action.Token_Count loop
         Token.List.Prepend (Tokens, Parser_State.Stack.Pop.ID);
      end loop;

      if Current_Parser.Active_Parser_Count > 1 then
         declare
            Pend_Item : constant Parser_Lists.Pend_Item := (Parser_Lists.Merge, Action, Tokens);
         begin
            Parser_State.Pend_Items.Put (Pend_Item);
            if Trace_Parse > 1 then
               Semantic_State.Trace.Put ("pending ");
               Parser_Lists.Put (Semantic_State.Trace.all, Pend_Item);
               Semantic_State.Trace.New_Line;
               Semantic_State.Trace.Put_Line
                 (" action count:" & SAL.Base_Peek_Type'Image (Parser_State.Pend_Items.Count));
            end if;
         end;
      else
         Semantic_State.Merge_Tokens (Action.LHS, Action.Index, Tokens, Action.Action);
         --  Merge_Tokens puts a trace, with extra token info

         Token.List.Clean (Tokens);
      end if;
   end Reduce_Stack;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Current_Token  : in Token_ID;
      Parser         : in Instance)
   is
      use all type Ada.Containers.Count_Type;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Trace        : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
   begin
      if Trace_Parse > 1 then
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              State_Image (Parser_State.Stack.Peek.State) & ": " &
              Image (Trace.Descriptor.all, Current_Token) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Parser_State.Stack.Push ((Action.State, Current_Token));

         if Current_Parser.Active_Parser_Count > 1 then
            declare
               Pend_Item : constant Parser_Lists.Pend_Item := (Parser_Lists.Push, Current_Token);
            begin
               Parser_State.Pend_Items.Put (Pend_Item);
               if Trace_Parse > 1 then
                  Trace.Put ("pending ");
                  Parser_Lists.Put (Trace, Pend_Item);
                  Trace.New_Line;
                  Trace.Put_Line (" action count:" & SAL.Base_Peek_Type'Image (Parser_State.Pend_Items.Count));
               end if;
            end;
         else
            Parser.Semantic_State.Push_Token (Current_Token);
         end if;

      when Reduce =>
         Current_Parser.Pre_Reduce_Stack_Save;

         Reduce_Stack (Current_Parser, Action, Parser.Semantic_State);

         Parser_State.Stack.Push
           ((State    => Goto_For
               (Table => Parser.Table.all,
                State => Parser_State.Stack.Peek.State,
                ID    => Action.LHS),
             ID       => Action.LHS));

         if Trace_Parse > 1 then
            Trace.Put_Line (" ... goto state " & State_Image (Parser_State.Stack.Peek.State));
         end if;

      when Accept_It =>
         Reduce_Stack
           (Current_Parser,
            (Reduce, Action.LHS, Action.Action, Action.Index, Action.Token_Count),
            Parser.Semantic_State);

      when Error =>
         Current_Parser.Save_Verb; -- For error recovery

      end case;

      Current_Parser.Set_Verb (Action.Verb);
   end Do_Action;

   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : all Parsers.Verb return Accept, Shift, or Error. All
   --  parsers have the same lookahead (or none). Get a token from
   --  Parsers.Lookahead or Lexer.Find_Next, execute Shift parsers,
   --  terminate Error parsers.
   --
   --  Shift_Local_Lookahead : some Parsers.Verb return Shift, but
   --  have different active lookaheads. Get a token from the
   --  appropriate Lookahead, execute Shift. This can happen only
   --  after an error recovery with multiple parsers, that return
   --  different error recover actions.
   --
   --  Reduce : some Parsers.Verb return Reduce - no new token,
   --  execute Reduce parsers.
   --
   --  Error : all Parsers.Verb return Error; report errors, terminate
   --  parse.
   function Parse_Verb
     (Parser  : in out LR.Instance'Class;
      Parsers : in out Parser_Lists.List)
     return All_Parse_Action_Verbs
   is
      --  WORKAROUND: Parsers could be 'in', but GNAT GPL 2016 requires 'in out'
      use Ada.Containers;
      use all type SAL.Base_Peek_Type;

      Shift_Count       : Count_Type := 0;
      Shift_Local_Count : Count_Type := 0;
      Accept_Count      : Count_Type := 0;
      Error_Count       : Count_Type := 0;

      Max_Shared_Lookahead_Index : SAL.Base_Peek_Type := SAL.Base_Peek_Type'First;
   begin
      for Parser_State of Parsers loop
         Max_Shared_Lookahead_Index := SAL.Base_Peek_Type'Max
           (Max_Shared_Lookahead_Index, Parser_State.Shared_Lookahead_Index);
      end loop;

      for Parser_State of Parsers loop
         case Parser_State.Verb is
         when Shift | Shift_Local_Lookahead =>
            if Parser_State.Local_Lookahead.Length > 0 then
               Shift_Local_Count := Shift_Local_Count + 1;
               Parser_State.Set_Verb (Shift_Local_Lookahead);

            elsif Parser.Lookahead.Count > 0 and
              Max_Shared_Lookahead_Index /= SAL.Base_Peek_Type'First and
              Max_Shared_Lookahead_Index /= Parser_State.Shared_Lookahead_Index
            then
               Shift_Local_Count := Shift_Local_Count + 1;
               Parser_State.Set_Verb (Shift_Local_Lookahead);

            else
               Shift_Count := Shift_Count + 1;
            end if;

         when Reduce =>
            return Reduce;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            Error_Count := Error_Count + 1;
         end case;
      end loop;

      if Shift_Local_Count = 0 and Parser.Lookahead.Length > 0 then
         --  All parsers are at the same lookahead. Parse will fetch current
         --  token from Lookahead (1).
         for I in 1 .. Max_Shared_Lookahead_Index - 1 loop
            Parser.Lookahead.Drop;
            --  parsers semantic_state operations were executed or
            --  pended when each lookahead was processed.
         end loop;

         for Parser_State of Parsers loop
            Parser_State.Shared_Lookahead_Index := 1;
         end loop;
      end if;

      if Parsers.Count = Accept_Count then
         return Accept_It;
      elsif Parsers.Count = Error_Count then
         return Error;
      elsif Shift_Local_Count > 0 then
         return Shift_Local_Lookahead;
      elsif Shift_Count > 0 then
         return Shift;
      else
         raise Programmer_Error;
      end if;
   end Parse_Verb;

   function Duplicate_State
     (Parsers        : in out Parser_Lists.List;
      Current_Parser : in     Parser_Lists.Cursor)
     return Boolean
   is
      --  WORKAROUND: Parsers could be 'in', but GNAT GPL 2016 requires 'in out'
      use all type Parser_Stacks.Stack_Type;
   begin
      for Parser_State of Parsers loop
         if Parser_State.Label /= Current_Parser.Label and then
           Parser_State.Stack = Current_Parser.State_Ref.Stack
         then
            return True;
         end if;
      end loop;
      return False;
   end Duplicate_State;

   procedure Execute_Pending
     (Current_Parser : in     Parser_Lists.Cursor;
      Semantic_State : access WisiToken.Token.Semantic_State'Class)
   is
      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Item         : Parser_Lists.Pend_Item;
   begin
      if Trace_Parse > 1 then
         Semantic_State.Trace.Put_Line ("execute pending");
      end if;
      if Trace_Parse > 3 then
         WisiToken.Token.Put (Semantic_State);
      end if;
      loop
         exit when Parser_State.Pend_Items.Is_Empty;
         Item := Parser_State.Pend_Items.Get;

         if Trace_Parse > 3 then
            Parser_Lists.Put (Semantic_State.Trace.all, Item);
            Semantic_State.Trace.New_Line;
         end if;

         case Item.Verb is
         when Parser_Lists.Input =>
            Semantic_State.Input_Token (Item.ID, Lexer => null);

         when Parser_Lists.Lookahead_To_Input =>
            Semantic_State.Move_Lookahead_To_Input (Item.ID);

         when Parser_Lists.Input_To_Lookahead =>
            Semantic_State.Move_Input_To_Lookahead (Item.ID);

         when Parser_Lists.Push =>
            Semantic_State.Push_Token (Item.ID);

         when Parser_Lists.Merge =>
            Semantic_State.Merge_Tokens
              (Item.Action.LHS, Item.Action.Index, Item.Tokens, Item.Action.Action);

            Token.List.Clean (Item.Tokens);

         when Parser_Lists.Pop =>
            Semantic_State.Pop_Token (Item.ID);

         when Parser_Lists.Discard_Input =>
            Semantic_State.Discard_Input (Item.ID);

         when Parser_Lists.Discard_Lookahead =>
            Semantic_State.Discard_Lookahead (Item.ID);

         when Parser_Lists.Recover =>
            Semantic_State.Recover (Item.Recover.all);
            WisiToken.Token.Free (Item.Recover);

         end case;
      end loop;
   end Execute_Pending;

   overriding procedure Parse (Parser : in out Instance)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Trace      : WisiToken.Trace'Class renames Parser.Semantic_State.Trace.all;
      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;

      Parsers              : Parser_Lists.List := Parser_Lists.New_List
        (First_State_Index  => Parser.Table.State_First,
         First_Parser_Label => Parser.First_Parser_Label);
      Current_Verb         : All_Parse_Action_Verbs;
      Shared_Current_Token : Token_ID;
      Current_Parser       : Parser_Lists.Cursor;
      Action               : Parse_Action_Node_Ptr;

      function Get_Current_Token (Current_Parser : in Parser_Lists.Parser_State) return Token_ID
      is
         --  For use after recover only.
      begin
         if Current_Parser.Local_Lookahead.Length > 0 then
            return Current_Parser.Local_Lookahead.Peek;
         else
            return Parser.Lookahead.Peek (Current_Parser.Shared_Lookahead_Index);
         end if;
      end Get_Current_Token;
   begin
      WisiToken.Token.Reset (Parser.Semantic_State);
      Parser.Lookahead.Clear;

      loop
         --  exit on Accept_It action or syntax error.

         Current_Verb := Parse_Verb (Parser, Parsers);

         case Current_Verb is
         when Shift_Local_Lookahead =>
            --  Handled in parser loop below
            null;

         when Shift =>
            if Parser.Lookahead.Length > 0 then
               --  Input_Lookahead was called for these when read from
               --  Lexer during error recover.
               Shared_Current_Token := Parser.Lookahead.Get;
               if Parsers.Count > 1 then
                  for Parser_State of Parsers loop
                     Parser_State.Pend_Items.Put ((Parser_Lists.Lookahead_To_Input, Shared_Current_Token));
                  end loop;
               else
                  Parser.Semantic_State.Move_Lookahead_To_Input (Shared_Current_Token);
               end if;
            else
               Shared_Current_Token := Parser.Lexer.Find_Next;
               Parser.Semantic_State.Input_Token (Shared_Current_Token, Parser.Lexer);
            end if;

            for Parser_State of Parsers loop
               Parser_State.Current_Token := Shared_Current_Token;
            end loop;

         when Accept_It =>
            declare
               Count : constant Ada.Containers.Count_Type := Parsers.Count;
            begin
               if Count > 1 then
                  --  Panic mode error resolution does not help with this.
                  raise Parse_Error with
                    Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                    ": Ambiguous parse:" & Ada.Containers.Count_Type'Image (Count) & " parsers active.";
               end if;
            end;
            return;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; attempt recovery
            declare
               use Parser_Lists;
               Expecting  : WisiToken.Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Terminal => False);
               Keep_Going : Boolean := False;

               Lookahead_Count : SAL.Base_Peek_Type := Parser.Lookahead.Length;
            begin
               for Parser_State of Parsers loop
                  Lookahead_Count := Lookahead_Count + Parser_State.Local_Lookahead.Length;
                  LR.Parser.Expecting (Parser.Table, Parser_State.Stack.Peek.State, Expecting);
               end loop;

               --  FIXME: each parser is in a different state; report
               --  each error separately. On the other hand, we
               --  normally expect all but one parser to error out, so
               --  it's not clear how to handle this.
               Parser.Semantic_State.Error (Expecting);

               if Lookahead_Count > 0 then
                  --  We are still recovering from previous error; if we
                  --  attempt recover, we'll lose track of the
                  --  lookaheads. That means the previous recovery was
                  --  not optimal, but it's too late to do anything about
                  --  it now.
                  --
                  --  FIXME: recover should verify that parsing can
                  --  continue thru the non-shared lookaheads, before
                  --  accepting a solution. Or we could use a
                  --  lookahead stack.
                  Keep_Going := False;
                  Trace.Put_Line ("recover not attempted; previous recover lookahead still active");

               else
                  --  Recover algorithms expect current token on
                  --  Parser.Lookahead, will update Parser.Lookahead
                  --  or Parsers (*).Local_Lookahead with new input
                  --  tokens, and set Parsers (*).Current_Token and
                  --  Parsers (*).Verb.
                  Parser.Lookahead.Put (Shared_Current_Token);
                  if Parsers.Count > 1 then
                     for Parser_State of Parsers loop
                        Parser_State.Pend_Items.Put ((Parser_Lists.Input_To_Lookahead, Shared_Current_Token));
                     end loop;
                  else
                     Parser.Semantic_State.Move_Input_To_Lookahead (Shared_Current_Token);
                  end if;

                  if Parser.Enable_McKenzie_Recover then
                     Keep_Going := McKenzie_Recover.Recover (Parser, Parsers);
                  end if;

                  if ((not Keep_Going) and Parser.Enable_Panic_Recover and Parsers.Count = 1) and then
                    Any (Parser.Table.Panic_Recover)
                  then
                     Keep_Going := Panic_Mode.Recover (Parser, Parsers);
                  end if;
               end if;

               if Trace_Parse > 0 then
                  if Keep_Going then
                     if Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: succeed, parser count" & Ada. Containers.Count_Type'Image (Parsers.Count));
                        if Trace_Parse > 1 then
                           Trace.Put ("shared lookahead: ");
                           Put (Trace, Parser.Lookahead);
                           Trace.New_Line;

                           for Parser_State of Parsers loop
                              Trace.Put (Integer'Image (Parser_State.Label) & " lookahead: ");
                              Put (Trace, Parser_State.Local_Lookahead);
                              Trace.Put (SAL.Base_Peek_Type'Image (Parser_State.Shared_Lookahead_Index));
                              Trace.New_Line;
                           end loop;
                        else
                           for Parser_State of Parsers loop
                              Trace.Put_Line
                                (Integer'Image (Parser_State.Label) & ": current_token " &
                                   Image (Descriptor, Get_Current_Token (Parser_State)) &
                                   " lookahead count " & SAL.Base_Peek_Type'Image
                                     (Parser.Lookahead.Length + Parser_State.Local_Lookahead.Length));
                           end loop;
                        end if;
                     else
                        --  single parser
                        if Trace_Parse > 1 then
                           Trace.Put ("recover: succeed, lookahead ");
                           Put (Trace, Parsers.First.State_Ref.Local_Lookahead);
                           Put (Trace, Parser.Lookahead);
                           Trace.New_Line;
                        else
                           Trace.Put_Line
                             ("recover: succeed lookahead count " & SAL.Base_Peek_Type'Image (Parser.Lookahead.Length));
                        end if;
                     end if;
                  else
                     if Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: fail, parser count" & Ada.Containers.Count_Type'Image (Parsers.Count));
                     else
                        Trace.Put_Line ("recover: fail");
                     end if;
                  end if;
                  Trace.New_Line;
               end if;

               if Keep_Going then
                  Shared_Current_Token := Invalid_Token_ID;

                  declare
                     Shift_Local_Count : Integer := 0;
                  begin

                     for Parser_State of Parsers loop
                        case Parser_State.Verb is
                        when Shift_Local_Lookahead =>
                           Shift_Local_Count := Shift_Local_Count + 1;

                        when Reduce =>
                           Current_Verb := Reduce;
                           if Trace_Parse > 2 then
                              Trace.Put_Line ("new current_verb: " & All_Parse_Action_Verbs'Image (Current_Verb));
                           end if;

                        when Error =>
                           null;

                        when Shift | Accept_It =>
                           raise Programmer_Error;
                        end case;
                     end loop;

                     if Shift_Local_Count > 0 then
                        Current_Verb := Shift_Local_Lookahead;
                        --  Skips processing below
                        if Trace_Parse > 2 then
                           Trace.Put_Line ("new current_verb: " & All_Parse_Action_Verbs'Image (Current_Verb));
                        end if;
                     end if;
                  end;

               else
                  --  Terminate with error. Semantic_State has all the
                  --  required info (recorded by Error, above), so we
                  --  just raise the exception.
                  raise Syntax_Error;
               end if;
            end;
         end case;

         --  We don't use 'for Parser_State of Parsers loop' here,
         --  because terminate on error and spawn on conflict require
         --  changing the parser list.
         Current_Parser := Parser_Lists.First (Parsers);
         loop
            exit when Current_Parser.Is_Done;

            --  All parsers reduce as much as possible, then shift
            --  Current_Token, then wait until all parsers have
            --  shifted it.

            if Current_Parser.Verb = Error then
               --  This parser errored on last input, some other
               --  parser can continue (else we would have handled
               --  this above). This is how grammar conflicts are
               --  resolved when the input text is valid, so just
               --  terminate this parser.

               if Trace_Parse > 0 then
                  Trace.Put_Line
                    (Integer'Image (Current_Parser.Label) & ": terminate (" &
                       Int_Image (Integer (Parsers.Count) - 1) & " active)");
               end if;
               Current_Parser.Free;

               if Parsers.Count = 1 then
                  Execute_Pending (Parsers.First, Parser.Semantic_State);
               end if;

            else
               if Current_Verb = Shift_Local_Lookahead and Current_Parser.Verb = Shift_Local_Lookahead then
                  declare
                     State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
                  begin
                     if Trace_Parse > 2 then
                        Trace.Put ("shared lookahead: ");
                        Put (Trace, Parser.Lookahead);
                        Trace.New_Line;
                        Trace.Put (Integer'Image (State.Label) & " lookahead: ");
                        Put (Trace, State.Local_Lookahead);
                        Trace.Put (SAL.Base_Peek_Type'Image (State.Shared_Lookahead_Index));
                        Trace.New_Line;
                        Parser.Semantic_State.Put;
                        Trace.New_Line;
                     end if;

                     if State.Local_Lookahead.Length > 0 then
                        --  These were inserted by special rules at start of
                        --  recover; neither Input_Token nor Input_Lookahead
                        --  was called.

                        State.Current_Token := State.Local_Lookahead.Get;

                        if Parsers.Count > 1 then
                           State.Pend_Items.Put ((Parser_Lists.Input, State.Current_Token));
                        else
                           Parser.Semantic_State.Input_Token (State.Current_Token, null);
                        end if;

                     elsif Parser.Lookahead.Length >= State.Shared_Lookahead_Index then
                        --  Input_Lookahead was called for these when read from
                        --  Lexer during error recover.

                        State.Current_Token := Parser.Lookahead.Peek (State.Shared_Lookahead_Index);
                        State.Shared_Lookahead_Index := State.Shared_Lookahead_Index + 1;

                        if Parsers.Count > 1 then
                           State.Pend_Items.Put ((Parser_Lists.Lookahead_To_Input, State.Current_Token));
                        else
                           Parser.Semantic_State.Move_Lookahead_To_Input (State.Current_Token);
                        end if;

                     else
                        --  waiting for other parsers to finish with lookaheads.
                        State.Set_Verb (Shift);
                     end if;
                  end;
               end if;

               if Trace_Parse > 2 then
                  Parser_Lists.Put_Top_10 (Trace, Current_Parser);
               end if;

               if Parser.Terminate_Same_State and then
                 (Current_Verb in Shift | Shift_Local_Lookahead and Duplicate_State (Parsers, Current_Parser))
               then
                  if Trace_Parse > 0 then
                     Trace.Put_Line
                       (Integer'Image (Current_Parser.Label) & ": duplicate state; terminate (" &
                          Int_Image (Integer (Parsers.Count) - 1) & " active)");
                  end if;
                  Current_Parser.Free;

                  if Parsers.Count = 1 then
                     Execute_Pending (Parsers.First, Parser.Semantic_State);
                  end if;

               elsif Current_Parser.Verb = Current_Verb
               then
                  declare
                     State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
                  begin
                     Action := Action_For
                       (Table => Parser.Table.all,
                        State => State.Stack.Peek.State,
                        ID    => State.Current_Token);
                  end;

                  if Action.Next /= null then
                     --  conflict; spawn a new parser
                     if Parsers.Count = Parser.Max_Parallel then
                        raise Parse_Error with
                          Int_Image (Parser.Lexer.Line) & ":" & Int_Image (Parser.Lexer.Column) &
                          ": too many parallel parsers required in grammar state" &
                          State_Index'Image (Current_Parser.State_Ref.Stack.Peek.State) &
                          "; simplify grammar, or increase max-parallel (" &
                          Ada.Containers.Count_Type'Image (Parser.Max_Parallel) & ")";

                     else
                        if Trace_Parse > 0 then
                           Trace.Put
                             ("spawn parser from " & Int_Image (Current_Parser.Label));
                        end if;
                        Parsers.Prepend_Copy (Current_Parser);
                        if Trace_Parse > 0 then
                           Trace.Put_Line (" (" & Int_Image (Integer (Parsers.Count)) & " active)");
                        end if;
                        Do_Action (Action.Next.Item, Parsers.First, Parsers.First.State_Ref.Current_Token, Parser);
                     end if;
                  end if;

                  --  Must spawn new parser before modifying current parser stack.
                  Do_Action (Action.Item, Current_Parser, Current_Parser.State_Ref.Current_Token, Parser);

                  Current_Parser.Next;
               else
                  --  Current parser is waiting for others to catch up
                  Current_Parser.Next;
               end if;
            end if;
         end loop;
      end loop;
   end Parse;

   function New_Parser
     (Lexer              :         in     WisiToken.Lexer.Handle;
      Table              :         in     Parse_Table_Ptr;
      Semantic_State     : aliased in out WisiToken.Token.Semantic_State'Class;
      Max_Parallel       :         in     Ada.Containers.Count_Type := 15;
      First_Parser_Label :         in     Integer                   := 1)
     return Instance
   is begin
      return
        (Lexer, Table, Semantic_State'Access,
         Lookahead               => Token_Queues.Empty_Queue,
         Enable_Panic_Recover    => False,
         Enable_McKenzie_Recover => False,
         Max_Parallel            => Max_Parallel,
         First_Parser_Label      => First_Parser_Label,
         Terminate_Same_State    => True);
   end New_Parser;

end WisiToken.Parser.LR.Parser;
