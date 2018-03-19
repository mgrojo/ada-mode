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

with Ada.Characters.Handling;
with Ada.Characters;
with WisiToken.LR.McKenzie_Recover;
package body WisiToken.LR.Parser is

   function Reduce_Stack_1
     (Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      Nonterm        :    out WisiToken.Syntax_Trees.Valid_Node_Index;
      Lexer          : in     WisiToken.Lexer.Handle;
      Resume_Active  : in     Boolean;
      Trace          : in out WisiToken.Trace'Class)
     return WisiToken.Semantic_Checks.Check_Status_Label
   is
      use all type SAL.Base_Peek_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
      Status       : constant Semantic_Checks.Check_Status := Reduce_Stack
          (Parser_State.Stack, Parser_State.Tree, Action, Nonterm, Lexer, Trace, Trace_Parse);
   begin
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      case Status.Label is
      when Ok =>
         return Ok;

      when Error =>

         if Resume_Active then
            --  Ignore this error; that's how McKenzie_Recover decided to fix it
            return Ok;

         else
            Parser_State.Errors.Append
              ((Label          => Check,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Tokens         => Status.Tokens,
                Code           => Status.Code,
                Recover        => (others => <>)));
            return Error;
         end if;
      end case;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in Parse_Action_Rec;
      Current_Parser : in Parser_Lists.Cursor;
      Shared_Parser  : in LR.Parser.Parser)
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Nonterm      : WisiToken.Syntax_Trees.Valid_Node_Index;
      Status       : Semantic_Checks.Check_Status_Label;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (Integer'Image (Current_Parser.Label) & ": " &
              Image (Parser_State.Stack.Peek.State) & ": " &
              Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Action.Verb);
         Parser_State.Stack.Push ((Action.State, Parser_State.Current_Token));
         Parser_State.Tree.Set_State (Parser_State.Current_Token, Action.State);

         Parser_State.Last_Shift_Was_Virtual := Parser_State.Current_Token_Is_Virtual;

      when Reduce =>
         Current_Parser.Pre_Reduce_Stack_Save;

         Status := Reduce_Stack_1
           (Current_Parser, Action, Nonterm, Shared_Parser.Lexer, Shared_Parser.Resume_Active, Trace);

         --  Even when Reduce_Stack_1 returns Error, it did reduce the stack, so
         --  push Nonterm.
         Parser_State.Stack.Push
           ((State    => Goto_For
               (Table => Shared_Parser.Table.all,
                State => Parser_State.Stack (1).State,
                ID    => Action.LHS),
             Token    => Nonterm));

         Parser_State.Tree.Set_State (Nonterm, Parser_State.Stack (1).State);

         case Status is
         when Ok =>
            Current_Parser.Set_Verb (Action.Verb);

            if Trace_Parse > Detail then
               Trace.Put_Line (" ... goto state " & Image (Parser_State.Stack.Peek.State));
            end if;

         when Error =>
            Current_Parser.Save_Verb; -- For error recovery
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Accept_It =>
         case Reduce_Stack_1
           (Current_Parser,
            (Reduce, Action.LHS, Action.Action, Action.Check, Action.Token_Count, Action.Production, Action.Name_Index),
            Nonterm, Shared_Parser.Lexer, Shared_Parser.Resume_Active, Trace)
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
            Parser_State.Errors.Append
              ((Label          => LR.Action,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Error_Token    => Parser_State.Current_Token,
                Expecting      => Expecting,
                Recover        => (others => <>)));

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
     (Parser           : in out LR.Parser.Parser;
      Verb             :    out All_Parse_Action_Verbs;
      Max_Shared_Token :    out Base_Token_Index;
      Zombie_Count     :    out SAL.Base_Peek_Type)
   is
      use all type SAL.Base_Peek_Type;

      Shift_Count       : SAL.Base_Peek_Type := 0;
      Shift_Local_Count : SAL.Base_Peek_Type := 0;
      Accept_Count      : SAL.Base_Peek_Type := 0;
      Error_Count       : SAL.Base_Peek_Type := 0;
   begin
      Max_Shared_Token := Base_Token_Index'First;
      Zombie_Count     := 0;

      for Parser_State of Parser.Parsers loop
         Max_Shared_Token := Token_Index'Max (Max_Shared_Token, Parser_State.Shared_Token);
      end loop;

      for Parser_State of Parser.Parsers loop
         case Parser_State.Verb is
         when Shift | Shift_Local_Lookahead =>
            if Parser_State.Local_Lookahead.Length > 0 or Parser_State.Current_Token_Is_Virtual then
               Shift_Local_Count := Shift_Local_Count + 1;
               Parser_State.Set_Verb (Shift_Local_Lookahead);

            elsif Max_Shared_Token /= Parser_State.Shared_Token then
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

      if Accept_Count > 0 and Parser.Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

      elsif Parser.Parsers.Count = Error_Count + Zombie_Count then
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
      --  WORKAROUND: Parsers could be 'in', but GNAT GPL 2016, 2017 requires 'in out'
      use all type SAL.Base_Peek_Type;

      function Compare
        (Stack_1 : in Parser_Stacks.Stack;
         Tree_1  : in Syntax_Trees.Branched.Tree;
         Stack_2 : in Parser_Stacks.Stack;
         Tree_2  : in Syntax_Trees.Branched.Tree)
        return Boolean
      is
      begin
         if Stack_1.Depth /= Stack_2.Depth then
            return False;
         else
            for I in reverse 1 .. Stack_1.Depth - 1 loop
               --  Assume they differ near the top; no point in comparing bottom
               --  item. The syntax trees will differ even if the tokens on the stack
               --  are the same, so compare the tokens.
               declare
                  Item_1 : Parser_Stack_Item renames Stack_1 (I);
                  Item_2 : Parser_Stack_Item renames Stack_2 (I);
               begin
                  if Item_1.State /= Item_2.State then
                     return False;
                  else
                     if Syntax_Trees.Branched.Same_Token (Tree_1, Item_1.Token, Tree_2, Item_2.Token) then
                        return False;
                     end if;
                  end if;
               end;
            end loop;
            return True;
         end if;
      end Compare;

   begin
      for Parser_State of Parsers loop
         if Parser_State.Label /= Current_Parser.Label and then
           Compare
             (Parser_State.Stack, Parser_State.Tree, Current_Parser.State_Ref.Stack, Current_Parser.State_Ref.Tree)
         then
            return True;
         end if;
      end loop;
      return False;
   end Duplicate_State;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out LR.Parser.Parser)
   is
      Action : Action_Node_Ptr;
      Temp_Action : Action_Node_Ptr;
      Parse_Action : Parse_Action_Node_Ptr;
      Temp_Parse_Action : Parse_Action_Node_Ptr;

      Got : Goto_Node_Ptr;
      Temp_Got : Goto_Node_Ptr;
   begin
      if Object.Table = null then
         return;
      end if;

      for State of Object.Table.States loop
         Action := State.Action_List;
         loop
            exit when Action = null;
            Parse_Action := Action.Action;
            loop
               exit when Parse_Action = null;
               Temp_Parse_Action := Parse_Action;
               Parse_Action := Parse_Action.Next;
               Free (Temp_Parse_Action);
            end loop;

            Temp_Action := Action;
            Action := Action.Next;
            Free (Temp_Action);
         end loop;

         Got := State.Goto_List;
         loop
            exit when Got = null;
            Temp_Got := Got;
            Got := Got.Next;
            Free (Temp_Got);
         end loop;
      end loop;

      Free (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser               :    out          LR.Parser.Parser;
      Trace                : not null access WisiToken.Trace'Class;
      Lexer                : in              WisiToken.Lexer.Handle;
      Table                : in              Parse_Table_Ptr;
      Semantic_Check_Fixes : in Semantic_Check_Fixes_Access;
      Max_Parallel         : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      First_Parser_Label   : in              Integer            := 1;
      Terminate_Same_State : in              Boolean            := True)
   is begin
      Parser.Lexer                   := Lexer;
      Parser.Trace                   := Trace;
      Parser.Table                   := Table;
      Parser.Semantic_Check_Fixes    := Semantic_Check_Fixes;
      Parser.Enable_McKenzie_Recover :=
        Table.McKenzie_Param.Cost_Limit /= WisiToken.LR.Default_McKenzie_Param.Cost_Limit;
      Parser.Max_Parallel            := Max_Parallel;
      Parser.First_Parser_Label      := First_Parser_Label;
      Parser.Terminate_Same_State    := Terminate_Same_State;
   end New_Parser;

   procedure Parse (Shared_Parser : in out LR.Parser.Parser)
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Current_Verb   : All_Parse_Action_Verbs;
      Current_Parser : Parser_Lists.Cursor;
      Action         : Parse_Action_Node_Ptr;

      Max_Shared_Token : Base_Token_Index;
      Zombie_Count     : SAL.Base_Peek_Type;

      procedure Terminate_Parser (Cur : in out Parser_Lists.Cursor)
      is begin
         if Trace_Parse > Outline then
            Trace.Put_Line
              (Integer'Image (Cur.Label) & ": terminate (" &
                 Int_Image (Integer (Shared_Parser.Parsers.Count) - 1) & " active)");
         end if;

         Cur.Free;

         if Shared_Parser.Parsers.Count = 1 then
            Shared_Parser.Parsers.First.State_Ref.Tree.Flush;
         end if;
      end Terminate_Parser;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input. This is how grammar conflicts
            --  are resolved when the input text is valid, in which case we should
            --  just terminate this parser. However, this may be due to invalid
            --  input text, so we keep the parser alive but suspended for a few
            --  tokens, to see if the other parsers also error, in which case they
            --  all participate in error recovery.

            --  We do not create zombie parsers during resume.
            if not Shared_Parser.Resume_Active then
               --  Parser is now a zombie
               if Trace_Parse > Detail then
                  Trace.Put_Line (Integer'Image (Check_Parser.Label) & ": zombie");
               end if;
               Check_Parser.Next;

            else
               if Shared_Parser.Parsers.Count = 1 then
                  if Trace_Parse > Detail then
                     Trace.Put_Line (Integer'Image (Check_Parser.Label) & ": error during resume");
                  end if;
                  raise Syntax_Error;
               else
                  Terminate_Parser (Check_Parser);
               end if;
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      --  The user must call Lexer.Reset_* to set the input text.
      Shared_Parser.Lexer.Errors.Clear;

      Shared_Parser.Semantic_State.Reset;
      Shared_Parser.Terminals.Clear;
      Shared_Parser.Shared_Tree.Clear;

      Shared_Parser.Parsers := Parser_Lists.New_List
        (First_Parser_Label => Shared_Parser.First_Parser_Label,
         Shared_Tree        => Shared_Parser.Shared_Tree'Unchecked_Access);

      Shared_Parser.Parsers.First.State_Ref.Stack.Push ((Shared_Parser.Table.State_First, others => <>));

      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Current_Verb, Max_Shared_Token, Zombie_Count);

         --  When parsing in the absense of errors, the current token for all
         --  parsers is at Shared_Parser.Shared_Lookahead(1).
         --
         --  When there are zombie parsers (ie, parsers that have encountered
         --  an error, but are not terminated yet), the current token for each
         --  parser is Shared_Parser.Terminals (Parsers(*).Shared_Token).
         --
         --  When resuming after error recovery, the shift verb is
         --  Shift_Local_Lookahead; the current token is at either
         --  Parsers(*).Local_Lookahead(1) (inserted during error recovery), or
         --  Shared_Parser.Terminals (Parsers(*).Shared_Token) (read ahead from
         --  Lexer during error recovery). Resuming is finished when all
         --  parsers are at the same current shared token.
         --
         --  Error recovery should ensure that the resume parsing can complete
         --  without error, so we cannot have zombie parsers while resuming.
         case Current_Verb is
         when Shift =>
            --  We just shifted a token; get the next token

            Shared_Parser.Resume_Active := False;

            for Parser_State of Shared_Parser.Parsers loop
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
                  Parser_State.Shared_Token  := Parser_State.Shared_Token + 1;
                  if Parser_State.Shared_Token <= Shared_Parser.Terminals.Last_Index then
                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Parser_State.Shared_Token, Shared_Parser.Terminals);
                  else
                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Next_Grammar_Token
                          (Shared_Parser.Terminals, Shared_Parser.Lexer, Shared_Parser.Semantic_State,
                           Shared_Parser.Trace.Descriptor.all),
                        Shared_Parser.Terminals);
                  end if;

                  Parser_State.Current_Token_Is_Virtual := False;
               end if;
            end loop;

         when Shift_Local_Lookahead =>
            --  Set Parsers(*).Current_Token

            --  Note that there cannot be any zombie parsers here.
            if Zombie_Count > 0 then
               raise Programmer_Error;
            end if;

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Shift_Local_Lookahead then
                  if Trace_Parse > Extra then
                     Trace.Put (Integer'Image (Parser_State.Label) & " lookahead: ");
                     Trace.Put (Parser_State.Tree.Image (Parser_State.Local_Lookahead, Trace.Descriptor.all));
                     Trace.Put (Token_Index'Image (Parser_State.Shared_Token));
                     Trace.New_Line;
                  end if;

                  if Parser_State.Local_Lookahead.Length > 0 then
                     --  These were inserted during error recovery.

                     Parser_State.Current_Token := Parser_State.Local_Lookahead.Get; -- Already in Tree

                     Parser_State.Current_Token_Is_Virtual := True; -- in case we transition to normal parsing.

                  elsif Parser_State.Shared_Token < Shared_Parser.Terminals.Last_Index or
                    (Parser_State.Current_Token_Is_Virtual and
                       Parser_State.Shared_Token = Shared_Parser.Terminals.Last_Index)
                  then
                     --  There are more tokens that were read from Lexer during error recovery.

                     if not Parser_State.Current_Token_Is_Virtual then
                        Parser_State.Shared_Token := Parser_State.Shared_Token + 1;
                     end if;

                     Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                       (Parser_State.Shared_Token, Shared_Parser.Terminals);

                     Parser_State.Current_Token_Is_Virtual := False; -- in case we transition to normal parsing.

                  else
                     --  Done with all lookaheads; waiting for other parsers to finish with
                     --  lookaheads, so do nothing this cycle.
                     Parser_State.Set_Verb (Shift);
                  end if;
               end if;
            end loop;

         when Accept_It =>
            --  All parsers accepted or are zombies.
            declare
               Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  if Trace_Parse > Outline then
                     Trace.Put_Line (Integer'Image (Shared_Parser.Parsers.First.Label) & ": succeed");
                  end if;
                  return;

               elsif Zombie_Count + 1 = Count then
                  --  All but one are zombies
                  Current_Parser := Shared_Parser.Parsers.First;
                  loop
                     if Current_Parser.Verb = Accept_It then
                        if Trace_Parse > Outline then
                           Trace.Put_Line (Integer'Image (Current_Parser.Label) & ": succeed with zombies");
                        end if;
                        Current_Parser.Next;
                     else
                        Terminate_Parser (Current_Parser);
                     end if;
                     exit when Current_Parser.Is_Done;
                  end loop;

                  return;

               else
                  --  More than one parser is active.
                  declare
                     Error_Parser_Count : Integer := (if Shared_Parser.Lexer.Errors.Length > 0 then 1 else 0);
                  begin
                     for Parser_State of Shared_Parser.Parsers loop
                        if Parser_State.Errors.Length > 0 then
                           Error_Parser_Count := Error_Parser_Count + 1;
                        end if;
                     end loop;

                     if Error_Parser_Count > 0 then
                        --  There was an error previously. We assume that caused the ambiguous
                        --  parse, and we pick the first parser arbitrarily to allow the parse
                        --  to succeed. We terminate the other parsers so the first parser
                        --  executes actions.
                        Current_Parser := Shared_Parser.Parsers.First;
                        Current_Parser.Next;
                        loop
                           Terminate_Parser (Current_Parser);
                           exit when Current_Parser.Is_Done;
                        end loop;

                        if Trace_Parse > Outline then
                           Trace.Put_Line ("ambiguous with error");
                        end if;

                        return;

                     else
                        --  There were no previous errors. We allow the parse to fail, on the
                        --  assumption that an otherwise correct input should not yeild an
                        --  ambiguous parse.
                        raise WisiToken.Parse_Error with Error_Message
                          ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                           "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                     end if;
                  end;
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
                  Recover_Result := McKenzie_Recover.Recover (Shared_Parser);
               end if;

               if Trace_Parse > Outline then
                  if Recover_Result in Success | Ignore then
                     if Shared_Parser.Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: succeed, parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     else
                        --  single parser
                        Trace.Put_Line ("recover: succeed");
                     end if;
                  else
                     if Shared_Parser.Parsers.Count > 1 then
                        Trace.Put_Line
                          ("recover: fail, parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     else
                        Trace.Put_Line ("recover: fail");
                     end if;
                  end if;
               end if;

               if Recover_Result in Success | Ignore then
                  Shared_Parser.Resume_Active := Recover_Result = Success;

                  declare
                     Shift_Local_Count : Integer := 0;
                  begin
                     for Parser_State of Shared_Parser.Parsers loop
                        if Trace_Parse > Outline then
                           Trace.Put_Line
                             (Integer'Image (Parser_State.Label) & ": Current_Token " &
                                Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) &
                                " Shared_Token " & Image
                                  (Shared_Parser.Terminals.Element (Parser_State.Shared_Token), Trace.Descriptor.all));
                           Trace.New_Line;
                        end if;
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
         Current_Parser := Shared_Parser.Parsers.First;
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
              (Current_Verb in Shift | Shift_Local_Lookahead and
                 Duplicate_State (Shared_Parser.Parsers, Current_Parser))
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
                     ID    => State.Tree.ID (State.Current_Token));
               end;

               if Action.Next /= null then
                  --  Conflict; spawn a new parser (before modifying Current_Parser
                  --  stack).

                  if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                     raise WisiToken.Parse_Error with Error_Message
                       ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                        ": too many parallel parsers required in grammar state" &
                          State_Index'Image (Current_Parser.State_Ref.Stack.Peek.State) &
                          "; simplify grammar, or increase max-parallel (" &
                          SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");

                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line
                          ("spawn parser from " & Int_Image (Current_Parser.Label) &
                             " (" & Int_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                     end if;

                     Shared_Parser.Parsers.Prepend_Copy (Current_Parser);
                     Do_Action (Action.Next.Item, Shared_Parser.Parsers.First, Shared_Parser);

                     --  We must terminate error parsers immediately in order to avoid
                     --  zombie parsers during recovery.
                     declare
                        Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                     begin
                        Check_Error (Temp);
                     end;
                  end if;
               end if;

               Do_Action (Action.Item, Current_Parser, Shared_Parser);
               Check_Error (Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
         end loop;
      end loop;
   end Parse;

   procedure Execute_Actions
     (Parser         : in out LR.Parser.Parser;
      User_Data      : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Compute_Indent : in     Boolean)
   is
      Descriptor : WisiToken.Descriptor'Class renames Parser.Trace.Descriptor.all;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Branched.Tree;
         Node : in     Syntax_Trees.Valid_Node_Index)
      is
         use all type Syntax_Trees.Node_Label;
         ID : Token_ID renames Tree.ID (Node);
      begin
         if ID < Descriptor.First_Nonterminal or Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            use all type Syntax_Trees.Semantic_Action;
            Tree_Children : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Node);
            Aug_Nonterm   : Semantic_State.Augmented_Token renames Tree.Augmented_Token_Ref
              (Parser.Semantic_State.Terminals, Node);
            Aug_Children  : constant Semantic_State.Augmented_Token_Access_Array := Tree.Augmented_Token_Array
              (Parser.Semantic_State.Terminals, Tree_Children);
         begin
            Semantic_State.Reduce (Aug_Nonterm, Aug_Children, Descriptor, Compute_Indent);

            if Trace_Action > Detail then
               declare
                  Action_Name : constant String :=
                    (if Tree.Action (Node) /= null
                     then Ada.Characters.Handling.To_Lower
                       (Image (Aug_Nonterm.ID, Descriptor)) & "_" &
                        Int_Image (Tree.Name_Index (Node)) & ": "
                     else "");
               begin
                  Parser.Trace.Put_Line
                    (Action_Name & Aug_Nonterm.Image (Descriptor) & " <= " &
                       Semantic_State.Image (Aug_Children, Descriptor));
               end;
            end if;

            if Tree.Action (Node) /= null then
               Tree.Action (Node) (User_Data, Parser.Semantic_State, Tree, Node, Tree_Children);
            end if;
         end;
      end Process_Node;

   begin
      for Parser_State of Parser.Parsers loop
         Parser_State.Tree.Process_Tree (Process_Node'Access);
      end loop;
   end Execute_Actions;

end WisiToken.LR.Parser;
