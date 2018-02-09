--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017, 2018 Stephe Leake
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

with WisiToken.LR.McKenzie_Recover;
with WisiToken.LR.Parser_Lists;
package body WisiToken.LR.Parser is

   function Reduce_Stack_1
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Semantic_State : access WisiToken.Semantic_State.Semantic_State'Class;
      Nonterm        :    out Base_Token;
      Lexer          : in     WisiToken.Lexer.Handle;
      Trace          : in out WisiToken.Trace'Class)
     return WisiToken.Semantic_Checks.Check_Status_Label
   is
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Tokens       : Base_Token_Arrays.Vector;
      Status       : constant Semantic_Checks.Check_Status :=
        Reduce_Stack (Parser_State.Stack, Action, Nonterm, Tokens, Lexer, Trace, Trace_Parse);
   begin
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      --  Even when Reduce_Stack returns Error, it did reduce the stack, so
      --  reduce the Semantic_State stack.
      if Current_Parser.Active_Parser_Count > 1 then
         Parser_State.Pend ((Parser_Lists.Reduce_Stack, Action, Nonterm, Tokens), Semantic_State.Trace.all);
      else
         Semantic_State.Reduce_Stack (Nonterm, Action.Index, Tokens, Action.Action);
         --  puts a trace, with extra token info
      end if;

      case Status.Label is
      when Ok =>
         return Ok;

      when Error =>
         Semantic_State.Error (Current_Parser.Label, Status.Tokens, Status.Code);
         return Error;
      end case;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Current_Token  : in Base_Token;
      Shared_Parser  : in Instance)
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;
      Nonterm      : Base_Token;
      Status       : Semantic_Checks.Check_Status_Label;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              State_Image (Parser_State.Stack.Peek.State) & ": " &
              Image (Current_Token, Trace.Descriptor.all) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Action.Verb);
         Parser_State.Stack.Push ((Action.State, Current_Token));

         if Current_Parser.Active_Parser_Count > 1 then
            Parser_State.Pend ((Parser_Lists.Push_Current, Current_Token), Trace);
         else
            Shared_Parser.Semantic_State.Push_Current (Current_Token);
         end if;

         Parser_State.Last_Shift_Was_Virtual := Parser_State.Current_Token_Is_Virtual;

      when Reduce =>
         Current_Parser.Pre_Reduce_Stack_Save;

         Status := Reduce_Stack_1
           (Current_Parser, Action, Shared_Parser.Semantic_State, Nonterm, Shared_Parser.Lexer, Trace);

         --  Even when Reduce_Stack_1 returns Error, it did reduce the stack, so
         --  push Nonterm.
         Parser_State.Stack.Push
           ((State    => Goto_For
               (Table => Shared_Parser.Table.all,
                State => Parser_State.Stack.Peek.State,
                ID    => Action.LHS),
             Token    => Nonterm));

         case Status is
         when Ok =>
            Current_Parser.Set_Verb (Action.Verb);

            if Trace_Parse > Detail then
               Trace.Put_Line (" ... goto state " & State_Image (Parser_State.Stack.Peek.State));
            end if;

         when Error =>
            Current_Parser.Save_Verb; -- For error recovery
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Accept_It =>
         case Reduce_Stack_1
           (Current_Parser,
            (Reduce, Action.LHS, Action.Action, Action.Check, Action.Index, Action.Token_Count),
            Shared_Parser.Semantic_State, Nonterm, Shared_Parser.Lexer, Trace)
         is
         when Ok =>
            Current_Parser.Set_Verb (Action.Verb);

         when Error =>
            Current_Parser.Save_Verb; -- For error recovery
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Current_Parser.Save_Verb; -- For error recovery
         Current_Parser.Set_Verb (Action.Verb);

         Parser_State.Zombie_Token_Count := 1;

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Current_Parser.State_Ref.Stack.Peek.State);
         begin
            Shared_Parser.Semantic_State.Error (Current_Parser.Label, Expecting);

            if Trace_Parse > Outline then
               Put
                 (Trace,
                  Integer'Image (Current_Parser.Label) & ": expecting: " &
                    Image (Expecting, Trace.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;

   end Do_Action;

   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : some Parsers.Verb return Shift, all with the same current
   --  token in Parsers.Shared_Lookahead.
   --
   --  Shift_Local_Lookahead : some Parsers.Verb return Shift, but have
   --  different current tokens (either local or shared). Or there is
   --  only one parser, but it has active local lookahead. This can
   --  happen only after error recovery.
   --
   --  Reduce : some Parsers.Verb return Reduce.
   --
   --  Error : all Parsers.Verb return Error.
   procedure Parse_Verb
     (Parser                     : in out LR.Instance'Class;
      Parsers                    : in out Parser_Lists.List;
      Verb                       :    out All_Parse_Action_Verbs;
      Max_Shared_Lookahead_Index :    out SAL.Peek_Type;
      Zombie_Count               :    out SAL.Base_Peek_Type)
   is
      use all type SAL.Base_Peek_Type;

      Shift_Count       : SAL.Base_Peek_Type := 0;
      Shift_Local_Count : SAL.Base_Peek_Type := 0;
      Accept_Count      : SAL.Base_Peek_Type := 0;
      Error_Count       : SAL.Base_Peek_Type := 0;
   begin
      Max_Shared_Lookahead_Index := SAL.Peek_Type'First;
      Zombie_Count               := 0;

      for Parser_State of Parsers loop
         Max_Shared_Lookahead_Index := SAL.Base_Peek_Type'Max
           (Max_Shared_Lookahead_Index, Parser_State.Shared_Lookahead_Index);
      end loop;

      for Parser_State of Parsers loop
         case Parser_State.Verb is
         when Shift | Shift_Local_Lookahead =>
            if Parser_State.Local_Lookahead.Length > 0 or Parser_State.Current_Token_Is_Virtual then
               Shift_Local_Count := Shift_Local_Count + 1;
               Parser_State.Set_Verb (Shift_Local_Lookahead);

            elsif Max_Shared_Lookahead_Index /= Parser_State.Shared_Lookahead_Index then
               Shift_Local_Count := Shift_Local_Count + 1;
               Parser_State.Set_Verb (Shift_Local_Lookahead);

            else
               Shift_Count := Shift_Count + 1;
            end if;

         when Reduce =>
            Verb := Reduce;
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            if Parser.Enable_McKenzie_Recover then
               --  This parser is waiting for others to error; they can continue
               --  parsing.
               Zombie_Count := Zombie_Count + 1;
            else
               Error_Count := Error_Count + 1;
            end if;
         end case;
      end loop;

      if Shift_Local_Count = 0 and Zombie_Count = 0 then
         --  All parsers are at the same current token. Drop preceding ones
         --  from Parser.Lookahead. Semantic state has already handled the
         --  lookaheads we are dropping here. Main loop will drop one more.
         for I in 1 .. Max_Shared_Lookahead_Index - 1 loop
            Parser.Shared_Lookahead.Drop;
         end loop;

         for Parser_State of Parsers loop
            Parser_State.Shared_Lookahead_Index := 1;
         end loop;
      end if;

      if Accept_Count > 0 and Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

      elsif Parsers.Count = Error_Count + Zombie_Count then
         Verb := Error;

      elsif Shift_Local_Count > 0 then
         Verb := Shift_Local_Lookahead;

      elsif Shift_Count > 0 then
         Verb := Shift;

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
     (Parser         : in Instance;
      Current_Parser : in Parser_Lists.Cursor)
   is
      Semantic_State : WisiToken.Semantic_State.Semantic_State_Access renames Parser.Semantic_State;
      Parser_State   : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Item           : Parser_Lists.Pend_Item;
   begin
      if Trace_Parse > Detail then
         Semantic_State.Trace.Put_Line (Integer'Image (Parser_State.Label) & ": execute pending");
      end if;

      if Trace_Parse > Extra then
         Semantic_State.Put;
         Semantic_State.Trace.Put ("shared lookahead: ");
         Semantic_State.Trace.Put (Image (Parser.Shared_Lookahead, Semantic_State.Trace.Descriptor.all));
         Semantic_State.Trace.New_Line;
      end if;
      loop
         exit when Parser_State.Pend_Items.Is_Empty;
         Item := Parser_State.Pend_Items.Get;

         case Item.Verb is
         when Parser_Lists.Virtual_To_Lookahead =>
            Semantic_State.Virtual_To_Lookahead (Item.Token);

         when Parser_Lists.Push_Current =>
            Semantic_State.Push_Current (Item.Token);

         when Parser_Lists.Discard_Stack =>
            Semantic_State.Discard_Stack (Item.Discard_ID);

         when Parser_Lists.Discard_Lookahead =>
            Semantic_State.Discard_Lookahead (Item.Discard_ID);

         when Parser_Lists.Reduce_Stack =>
            Semantic_State.Reduce_Stack
              (Item.Nonterm, Item.Action.Index, Item.Tokens, Item.Action.Action);

         when Parser_Lists.Recover =>
            Semantic_State.Recover (Parser_State.Label, Item.Recover.all);
            WisiToken.Semantic_State.Free (Item.Recover);

         end case;
      end loop;
   end Execute_Pending;

   procedure Parse (Shared_Parser : in out Instance)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Trace      : WisiToken.Trace'Class renames Shared_Parser.Semantic_State.Trace.all;
      Descriptor : WisiToken.Descriptor'Class renames Trace.Descriptor.all;

      Parsers        : Parser_Lists.List := Parser_Lists.New_List
        (First_State_Index  => Shared_Parser.Table.State_First,
         First_Parser_Label => Shared_Parser.First_Parser_Label);
      Current_Verb   : All_Parse_Action_Verbs;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;

      First_Token                : Boolean := True;
      Max_Shared_Lookahead_Index : SAL.Base_Peek_Type;
      Zombie_Count               : SAL.Base_Peek_Type;
      Resume_Active              : Boolean := False;

      procedure Terminate_Parser (Cur : in out Parser_Lists.Cursor)
      is begin
         if Trace_Parse > Outline then
            Trace.Put_Line
              (Integer'Image (Cur.Label) & ": terminate (" &
                 Int_Image (Integer (Parsers.Count) - 1) & " active)");
         end if;

         Shared_Parser.Semantic_State.Terminate_Parser (Cur.Label);

         Cur.Free;

         if Parsers.Count = 1 then
            Execute_Pending (Shared_Parser, Parsers.First);
         end if;
      end Terminate_Parser;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input, and some other parser(s) can
            --  continue (else we would have handled this elsewhere). This is how
            --  grammar conflicts are resolved when the input text is valid, in
            --  which case we should just terminate this parser. However, this may
            --  be due to invalid input text, so we keep the parser alive but
            --  suspended for a few tokens, to see if the other parsers also
            --  error, in which case they all participate in error recovery.

            --  We do not create zombie parsers during resume.
            if not Resume_Active then
               --  Parser is now a zombie
               if Trace_Parse > Detail then
                  Trace.Put_Line (Integer'Image (Check_Parser.Label) & ": zombie");
               end if;
               Check_Parser.Next;

            else
               Terminate_Parser (Check_Parser);
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      --  We do not call Shared_Parser.Semantic_State.Reset here; we assume the
      --  caller has called Initialize or Reset.

      Shared_Parser.Shared_Lookahead.Clear;

      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Parsers, Current_Verb, Max_Shared_Lookahead_Index, Zombie_Count);

         --  When parsing in the absense of errors, the current token for all
         --  parsers is at Shared_Parser.Shared_Lookahead(1).
         --
         --  When there are zombie parsers (ie, parsers that have encountered
         --  an error, but are not terminated yet), the current token for each
         --  parser is at
         --  Shared_Parser.Shared_Lookahead(Parsers(*).Shared_Lookahead_Index).
         --
         --  When resuming after error recovery, the shift verb is
         --  Shift_Local_Lookahead; the current token is at either
         --  Parsers(*).Local_Lookahead(1) (inserted during error recovery), or
         --  Shared_Parser.Shared_Lookahead(Parsers(*).Shared_Lookahead_Index)
         --  (read ahead from Lexer during error recovery). 'resuming' is
         --  finished when all parsers are at the same current token.
         --
         --  Error recovery should ensure that the resume parsing can complete
         --  without error, so we cannot have zombie parsers while resuming.
         case Current_Verb is
         when Shift =>
            Resume_Active := False;

            --  Set Parsers(*).Current_Token to a copy of
            --  Shared_Parser.Shared_Lookahead(Parsers(*).Shared_Lookahead_Index).

            --  We just pushed the current token on the stack, so we are done with
            --  it, unless a zombie still needs it. We don't do this in Do_Shift,
            --  because we don't know Zombie_Count then.
            if Zombie_Count = 0 and not First_Token then
               Shared_Parser.Shared_Lookahead.Drop;
            end if;
            First_Token := False;

            for Parser_State of Parsers loop
               if Parser_State.Verb = Error then
                  if Shared_Parser.Enable_McKenzie_Recover then
                     Parser_State.Zombie_Token_Count := Parser_State.Zombie_Token_Count + 1;
                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Integer'Image (Parser_State.Label) & ": zombie (" &
                             Int_Image
                               (Shared_Parser.Table.McKenzie_Param.Check_Limit - Parser_State.Zombie_Token_Count) &
                             " tokens remaining)");
                     end if;
                  end if;
               else
                  if Zombie_Count = 0 then
                     Parser_State.Shared_Lookahead_Index := 1;
                  elsif not Parser_State.Last_Shift_Was_Virtual then
                     Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;
                  end if;

                  if Parser_State.Shared_Lookahead_Index > Shared_Parser.Shared_Lookahead.Length then
                     Shared_Parser.Shared_Lookahead.Put
                       (Next_Grammar_Token (Shared_Parser.Lexer, Shared_Parser.Semantic_State));
                  end if;

                  Parser_State.Current_Token := Shared_Parser.Shared_Lookahead.Peek
                    (Parser_State.Shared_Lookahead_Index);
                  Parser_State.Current_Token_Is_Virtual := False;
               end if;
            end loop;

         when Shift_Local_Lookahead =>
            --  Set Parsers(*).Current_Token

            --  Note that there cannot be any zombie parsers here.
            if Zombie_Count > 0 then
               raise Programmer_Error;
            end if;

            if Trace_Parse > Extra then
               Trace.Put ("shared lookahead: ");
               Trace.Put (Image (Shared_Parser.Shared_Lookahead, Trace.Descriptor.all));
               Trace.New_Line;
            end if;

            for Parser_State of Parsers loop
               if Parser_State.Verb = Shift_Local_Lookahead then
                  if Trace_Parse > Extra then
                     Trace.Put (Integer'Image (Parser_State.Label) & " lookahead: ");
                     Trace.Put (Image (Parser_State.Local_Lookahead, Trace.Descriptor.all));
                     Trace.Put (SAL.Base_Peek_Type'Image (Parser_State.Shared_Lookahead_Index));
                     Trace.New_Line;
                     Shared_Parser.Semantic_State.Put;
                  end if;

                  if Parser_State.Local_Lookahead.Length > 0 then
                     --  These were inserted during error recovery.

                     Parser_State.Current_Token            := Parser_State.Local_Lookahead.Get;
                     Parser_State.Current_Token_Is_Virtual := True; -- in case we transition to normal parsing.

                     if Parsers.Count > 1 then
                        Parser_State.Pend ((Parser_Lists.Virtual_To_Lookahead, Parser_State.Current_Token), Trace);
                     else
                        Shared_Parser.Semantic_State.Virtual_To_Lookahead (Parser_State.Current_Token);
                     end if;

                  elsif Shared_Parser.Shared_Lookahead.Length >= Parser_State.Shared_Lookahead_Index then
                     --  These where read from Lexer during error recovery.

                     if not Parser_State.Current_Token_Is_Virtual then
                        Parser_State.Shared_Lookahead_Index := Parser_State.Shared_Lookahead_Index + 1;
                     end if;

                     Parser_State.Current_Token := Shared_Parser.Shared_Lookahead.Peek
                       (Parser_State.Shared_Lookahead_Index);

                     Parser_State.Current_Token_Is_Virtual := False; -- in case we transition to normal parsing.

                  else
                     --  Done with all lookaheads; waiting for other parsers to finish with
                     --  lookaheads, so do nothing this cycle.
                     Parser_State.Set_Verb (Shift);
                  end if;
               end if;
            end loop;

         when Accept_It =>
            declare
               Count : constant SAL.Base_Peek_Type := Parsers.Count;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  if Trace_Parse > Outline then
                     Trace.Put_Line (Integer'Image (Parsers.First.Label) & ": succeed");
                  end if;
                  return;

               elsif Zombie_Count + 1 = Count then
                  --  All but one are zombies
                  Current_Parser := Parsers.First;
                  loop
                     if Current_Parser.Verb = Accept_It then
                        Execute_Pending (Shared_Parser, Current_Parser);
                        if Trace_Parse > Outline then
                           Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": succeed");
                        end if;
                     else
                        Terminate_Parser (Current_Parser);
                     end if;
                     Current_Parser.Next;
                     exit when Current_Parser.Is_Done;
                  end loop;

                  return;

               else
                  if Shared_Parser.Semantic_State.Parser_Errors.Length > 0 or
                    Shared_Parser.Semantic_State.Lexer_Errors.Length > 0
                  then
                     --  There was an error previously. We assume that caused the ambiguous
                     --  parse, and we pick the first parser arbitrarily to allow the parse
                     --  to succeed. We terminate the other parsers so the first parser
                     --  executes pending actions.
                     Current_Parser := Parsers.First;
                     Current_Parser.Next;
                     loop
                        Terminate_Parser (Current_Parser);
                        exit when Current_Parser.Is_Done;
                     end loop;

                     return;

                  else
                     --  There were no previous errors. We allow the parse to fail, on the
                     --  assumption that an otherwise correct input should not yeild an
                     --  ambiguous parse.
                     raise Parse_Error with Error_Message
                       ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                        "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                  end if;
               end if;
            end;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; attempt recovery
            declare
               use all type McKenzie_Recover.Recover_Status;
               Recover_Result : McKenzie_Recover.Recover_Status := Fail;
            begin
               --  Recover algorithms expect current token at
               --  Parsers(*).Current_Token, will update
               --  Shared_Parser.Shared_Lookahead and/or Parsers(*).Local_Lookahead
               --  with new input tokens, and set Parsers(*).Current_Token and
               --  Parsers(*).Verb.

               if Shared_Parser.Enable_McKenzie_Recover then
                  Recover_Result := McKenzie_Recover.Recover (Shared_Parser, Parsers);
               end if;

               if Trace_Parse > Outline then
                  if Recover_Result in Success | Ignore then
                     if Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: succeed, parser count" & SAL.Base_Peek_Type'Image (Parsers.Count));

                        if Trace_Parse > Extra then
                           Trace.Put ("shared lookahead: ");
                           Trace.Put (Image (Shared_Parser.Shared_Lookahead, Trace.Descriptor.all));
                           Trace.New_Line;

                           for Parser_State of Parsers loop
                              Trace.Put (Integer'Image (Parser_State.Label) & ": lookahead: ");
                              Trace.Put (Image (Parser_State.Local_Lookahead, Trace.Descriptor.all));
                              Trace.Put (SAL.Base_Peek_Type'Image (Parser_State.Shared_Lookahead_Index));
                              Trace.New_Line;
                              Trace.Put_Line
                                (Integer'Image (Parser_State.Label) & ": current_token: " &
                                   Image (Parser_State.Current_Token, Descriptor) &
                                   (if Parser_State.Current_Token_Is_Virtual then " virtual" else ""));
                           end loop;
                        end if;
                     else
                        --  single parser
                        Trace.Put_Line ("recover: succeed");
                        if Trace_Parse > Extra then
                           Trace.Put ("recover: succeed, lookahead ");
                           Trace.Put (Image (Parsers.First.State_Ref.Local_Lookahead, Trace.Descriptor.all));
                           Trace.Put (Image (Shared_Parser.Shared_Lookahead, Trace.Descriptor.all));
                           Trace.New_Line;
                           Trace.Put_Line
                             ("current_token: " & Image (Parsers.First.State_Ref.Current_Token, Descriptor));
                        end if;
                     end if;
                  else
                     if Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: fail, parser count" & SAL.Base_Peek_Type'Image (Parsers.Count));
                     else
                        Trace.Put_Line ("recover: fail");
                     end if;
                  end if;
                  Trace.New_Line;
               end if;

               if Recover_Result in Success | Ignore then
                  Resume_Active := Recover_Result = Success;

                  declare
                     Shift_Local_Count : Integer := 0;
                  begin
                     for Parser_State of Parsers loop
                        case Parser_State.Verb is
                        when Shift_Local_Lookahead =>
                           Shift_Local_Count := Shift_Local_Count + 1;

                           Parser_State.Zombie_Token_Count := 0;

                        when Reduce =>
                           Current_Verb := Reduce;
                           if Trace_Parse > Extra then
                              Trace.Put_Line ("new current_verb: " & All_Parse_Action_Verbs'Image (Current_Verb));
                           end if;

                           Parser_State.Zombie_Token_Count := 0;

                        when Error =>
                           --  Force this parser to be terminated.
                           if Shared_Parser.Enable_McKenzie_Recover then
                              Parser_State.Zombie_Token_Count := Shared_Parser.Table.McKenzie_Param.Check_Limit + 1;
                           end if;

                        when Shift =>
                           if Current_Verb /= Reduce then
                              Current_Verb := Shift;
                           end if;

                        when  Accept_It =>
                           raise Programmer_Error;
                        end case;
                     end loop;

                     if Shift_Local_Count > 0 then
                        Current_Verb := Shift_Local_Lookahead;

                        if Trace_Parse > Extra then
                           Trace.Put_Line ("new current_verb: " & All_Parse_Action_Verbs'Image (Current_Verb));
                        end if;
                     end if;
                  end;

               else
                  --  Terminate with error. Semantic_State has all the required info
                  --  (recorded by Error in Do_Action), so we just raise the exception.
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

            if Trace_Parse > Extra then
               Trace.Put_Line
                 ("current_verb: " & Parse_Action_Verbs'Image (Current_Verb) &
                    "," & Integer'Image (Current_Parser.Label) &
                    ".verb: " & Parse_Action_Verbs'Image (Current_Parser.Verb));
            end if;

            --  Each branch of the following 'if' calls either Current_Parser.Free
            --  (which advances to the next parser) or Current_Parser.Next.

            if Current_Parser.Verb = Error then
               --  This parser is a zombie; see Check_Error above.
               --
               --  Check to see if it is time to terminate it
               if Shared_Parser.Enable_McKenzie_Recover and then
                 Current_Parser.State_Ref.Zombie_Token_Count <= Shared_Parser.Table.McKenzie_Param.Check_Limit
               then
                  if Trace_Parse > Detail then
                     Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": zombie");
                  end if;

                  Current_Parser.Next;
               else
                  Terminate_Parser (Current_Parser);
               end if;

            elsif Shared_Parser.Terminate_Same_State and then
              (Current_Verb in Shift | Shift_Local_Lookahead and Duplicate_State (Parsers, Current_Parser))
            then
               if Trace_Parse > Outline then
                  Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": duplicate state");
               end if;

               Terminate_Parser (Current_Parser);

            elsif Current_Parser.Verb = Current_Verb then
               if Trace_Parse > Extra then
                  Parser_Lists.Put_Top_10 (Trace, Current_Parser);
               end if;

               declare
                  State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
               begin
                  Action := Action_For
                    (Table => Shared_Parser.Table.all,
                     State => State.Stack.Peek.State,
                     ID    => State.Current_Token.ID);
               end;

               if Action.Next /= null then
                  --  Conflict; spawn a new parser (before modifying Current_Parser
                  --  stack).

                  if Parsers.Count = Shared_Parser.Max_Parallel then
                     raise Parse_Error with Error_Message
                       ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                        ": too many parallel parsers required in grammar state" &
                          State_Index'Image (Current_Parser.State_Ref.Stack.Peek.State) &
                          "; simplify grammar, or increase max-parallel (" &
                          SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");

                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line
                          ("spawn parser from " & Int_Image (Current_Parser.Label) &
                             " (" & Int_Image (1 + Integer (Parsers.Count)) & " active)");
                     end if;

                     Parsers.Prepend_Copy (Current_Parser);
                     Shared_Parser.Semantic_State.Spawn (Current_Parser.Label, Parsers.First.Label);
                     Do_Action (Action.Next.Item, Parsers.First, Parsers.First.State_Ref.Current_Token, Shared_Parser);

                     --  We must terminate error parsers immediately in order to avoid
                     --  zombie parsers during recovery.
                     declare
                        Temp : Parser_Lists.Cursor := Parsers.First;
                     begin
                        Check_Error (Temp);
                     end;
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Current_Parser.State_Ref.Current_Token, Shared_Parser);
               Check_Error (Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
         end loop;
      end loop;
   end Parse;

   procedure New_Parser
     (Parser               :    out Instance;
      Lexer                : in     WisiToken.Lexer.Handle;
      Table                : in     Parse_Table_Ptr;
      Semantic_State       : in     WisiToken.Semantic_State.Semantic_State_Access;
      Max_Parallel         : in     SAL.Base_Peek_Type := Default_Max_Parallel;
      First_Parser_Label   : in     Integer            := 1;
      Terminate_Same_State : in     Boolean            := True)
   is begin
      Parser.Lexer                   := Lexer;
      Parser.Table                   := Table;
      Parser.Semantic_State          := Semantic_State;
      Parser.Shared_Lookahead        := Base_Token_Queues.Empty_Queue;
      Parser.Enable_McKenzie_Recover :=
        Table.McKenzie_Param.Cost_Limit /= WisiToken.LR.Default_McKenzie_Param.Cost_Limit;
      Parser.Max_Parallel            := Max_Parallel;
      Parser.First_Parser_Label      := First_Parser_Label;
      Parser.Terminate_Same_State    := Terminate_Same_State;
   end New_Parser;

end WisiToken.LR.Parser;
