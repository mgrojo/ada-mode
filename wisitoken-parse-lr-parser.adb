--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2020 Free Software Foundation, Inc.
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

with Ada.Calendar.Formatting;
with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
with WisiToken.Parse.LR.McKenzie_Recover;
package body WisiToken.Parse.LR.Parser is

   function Reduce_Stack_1
     (Shared_Parser  : in out Parser;
      Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      New_State      : in     State_Index;
      Lexer          : in     WisiToken.Lexer.Handle;
      Trace          : in out WisiToken.Trace'Class)
     return WisiToken.Semantic_Checks.Check_Status_Label
   is
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      use all type Semantic_Checks.Check_Status_Label;
      use all type Semantic_Checks.Semantic_Check;

      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;

      Nonterm : constant Syntax_Trees.Stream_Index := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, Action.Action, New_State,
         Default_Virtual => False);
   begin
      if Trace_Parse > Detail then
         Trace.Put_Line (Shared_Parser.Tree.Image (Nonterm, Trace.Descriptor.all, Include_Children => True));
      end if;

      if Action.Check = null then
         return Ok;

      else
         --  We have to call the semantic action even when Resume_Active,
         --  because it might do other things than return a status.
         declare
            Nonterm_Token : Syntax_Trees.Recover_Token :=
              Shared_Parser.Tree.Get_Recover_Token (Parser_State.Stream, Nonterm);

            Children_Token : constant Syntax_Trees.Recover_Token_Array :=
              Shared_Parser.Tree.Children_Recover_Tokens (Parser_State.Stream, Nonterm);
            Status         : Semantic_Checks.Check_Status;
         begin
            Status := Action.Check (Lexer, Nonterm_Token, Children_Token, Recover_Active => False);

            if Nonterm_Token.Name /= Null_Buffer_Region then
               Shared_Parser.Tree.Set_Name_Region (Shared_Parser.Tree.Get_Node (Nonterm), Nonterm_Token.Name);
            end if;

            if Trace_Parse > Detail then
               Trace.Put_Line ("semantic check " & Semantic_Checks.Image (Status, Trace.Descriptor.all));
            end if;

            case Status.Label is
            when Ok =>
               return Ok;

            when Semantic_Checks.Error =>
               if Parser_State.Resume_Active then
                  --  Ignore this error; that's how McKenzie_Recover decided to fix it
                  return Ok;

               else
                  Parser_State.Errors.Append
                    ((Label          => Check,
                      First_Terminal => Trace.Descriptor.First_Terminal,
                      Last_Terminal  => Trace.Descriptor.Last_Terminal,
                      Check_Status   => Status,
                      Recover        => (others => <>)));
                  return Status.Label;
               end if;
            end case;
         end;
      end if;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in     Parse_Action_Rec;
      Current_Parser : in     Parser_Lists.Cursor;
      Shared_Parser  : in out LR.Parser.Parser)
   is
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Status       : Semantic_Checks.Check_Status_Label;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": " &
              (if Trace_Parse_No_State_Numbers
               then "-- : "
               else Trimmed_Image (Shared_Parser.Tree.State (Current_Parser.Stream)) & ": ") &
              Shared_Parser.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) & " : ");
         Put (Trace, Action);
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Shift);
         Shared_Parser.Tree.Shift (Parser_State.Stream, Action.State, Parser_State.Current_Token);

      when Reduce =>
         declare
            New_State : constant Unknown_State_Index := Goto_For
              (Table => Shared_Parser.Table.all,
               State => Shared_Parser.Tree.State
                 (Parser_State.Stream, Shared_Parser.Tree.Peek
                    (Parser_State.Stream, SAL.Base_Peek_Type (Action.Token_Count) + 1)),
               ID    => Action.Production.LHS);
         begin
            if New_State = Unknown_State then
               --  This is due to a bug in the LALR parser generator (see
               --  lalr_generator_bug_01.wy); we treat it as a syntax error.
               Current_Parser.Set_Verb (Error);
               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... error");
               end if;

            else
               Status := Reduce_Stack_1 (Shared_Parser, Current_Parser, Action, New_State, Shared_Parser.Lexer, Trace);

               case Status is
               when Ok =>
                  Current_Parser.Set_Verb (Reduce);

                  if Trace_Parse > Detail then
                     Trace.Put_Line
                       (" ... goto state " &
                          (if Trace_Parse_No_State_Numbers
                           then "--"
                           else Trimmed_Image (New_State)));
                  end if;

               when Semantic_Checks.Error =>
                  Current_Parser.Set_Verb (Error);
                  Parser_State.Zombie_Token_Count := 1;
               end case;
            end if;
         end;

      when Accept_It =>
         case Reduce_Stack_1
           (Shared_Parser, Current_Parser,
            (Reduce, Action.Production, Action.Action, Action.Check, Action.Token_Count),
            0, Shared_Parser.Lexer, Trace)
         is
         when Ok =>
            Current_Parser.Set_Verb (Action.Verb);

         when Semantic_Checks.Error =>
            Current_Parser.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Current_Parser.Set_Verb (Action.Verb);

         Parser_State.Zombie_Token_Count := 1;

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Shared_Parser.Tree.State (Parser_State.Stream));
         begin
            Parser_State.Errors.Append
              ((Label          => LR.Action,
                First_Terminal => Expecting'First,
                Last_Terminal  => Expecting'Last,
                Error_Token    => Parser_State.Current_Token,
                Expecting      => Expecting,
                Recover        => (others => <>)));

            if Trace_Parse > Outline then
               Put
                 (Trace,
                  Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": " &
                    (if Trace_Parse_No_State_Numbers
                     then "--"
                     else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream))) & ": expecting: " &
                    Image (Expecting, Trace.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Ins_Del     : Vector renames Parser_State.Recover_Insert_Delete;
      Ins_Del_Cur : Extended_Index renames Parser_State.Recover_Insert_Delete_Current;
   begin
      loop
         exit when Ins_Del_Cur = Recover_Op_Arrays.No_Index;
         declare
            Op : Recover_Op renames Constant_Ref (Ins_Del, Ins_Del_Cur);
         begin
            if Op.Op = Delete and then
              Op.Del_Token_Index =
              (if Parser_State.Inc_Shared_Token
               then Shared_Parser.Tree.Stream_Next (Parser_State.Stream, Parser_State.Shared_Token)
               else Parser_State.Shared_Token)
            then
               Parser_State.Shared_Token := Shared_Parser.Tree.Stream_Next
                 (Parser_State.Stream, Parser_State.Shared_Token);
               --  We don't reset Inc_Shared_Token here; only after the next token is
               --  actually used.
               Ins_Del_Cur := Ins_Del_Cur + 1;
               if Ins_Del_Cur > Last_Index (Ins_Del)  then
                  Ins_Del_Cur := No_Index;
               end if;
            else
               exit;
            end if;
         end;
      end loop;

      if Trace_Parse > Extra then
         Shared_Parser.Trace.Put_Line
           (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": (do_deletes) shared_token:" &
              Shared_Parser.Tree.Get_Element_Index (Parser_State.Shared_Token)'Image &
              " inc_shared_token: " & Parser_State.Inc_Shared_Token'Image &
              " recover_insert_delete:" &
              (if Parser_State.Recover_Insert_Delete_Current = No_Index
               then ""
               else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                  Image
                    (Constant_Ref (Parser_State.Recover_Insert_Delete, Parser_State.Recover_Insert_Delete_Current),
                     Shared_Parser.Trace.Descriptor.all)));
      end if;
   end Do_Deletes;

   --  Verb: the type of parser cycle to execute;
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : some Parsers.Verb return Shift, all with the same current
   --  token in Shared_Parser.Terminals.
   --
   --  Pause : Resume is active, and this parser has reached Resume_Goal,
   --  so it is waiting for the others to catch up.
   --
   --  Reduce : some Parsers.Verb return Reduce.
   --
   --  Error : all Parsers.Verb return Error.
   --
   --  Zombie_Count: count of parsers in Error state
   procedure Parse_Verb
     (Shared_Parser : in out LR.Parser.Parser;
      Verb          :    out All_Parse_Action_Verbs;
      Zombie_Count  :    out SAL.Base_Peek_Type)
   is
      Shift_Count   : SAL.Base_Peek_Type := 0;
      Accept_Count  : SAL.Base_Peek_Type := 0;
      Error_Count   : SAL.Base_Peek_Type := 0;
      Resume_Active : Boolean            := False;
   begin
      Zombie_Count := 0;

      for Parser_State of Shared_Parser.Parsers loop
         case Parser_State.Verb is
         when Pause | Shift =>
            Do_Deletes (Shared_Parser, Parser_State);

            Shift_Count := Shift_Count + 1;
            Parser_State.Set_Verb (Shift);

            if Parser_State.Resume_Active then
               --  There may still be ops left in Recover_Insert_Delete after we get
               --  to Resume_Token_Goal, probably from a Language_Fix or string quote
               --  fix that deletes a lot of tokens. FIXME: that's a bug!
               if Parser_State.Resume_Token_Goal <= Shared_Parser.Tree.Get_Element_Index (Parser_State.Shared_Token) and
                 Parser_State.Recover_Insert_Delete_Current = Recover_Op_Arrays.No_Index
               then
                  Parser_State.Resume_Active := False;
                  if Trace_Parse > Detail then
                     Shared_Parser.Trace.Put_Line
                       (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": resume_active: False");
                  end if;
               else
                  Resume_Active := True;
               end if;
            end if;

         when Reduce =>
            Verb := Reduce;
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            if Shared_Parser.Enable_McKenzie_Recover then
               --  This parser is waiting for others to error; they can continue
               --  parsing.
               Zombie_Count := Zombie_Count + 1;
            else
               Error_Count := Error_Count + 1;
            end if;
         end case;
      end loop;

      if Accept_Count > 0 and Shared_Parser.Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

      elsif Shared_Parser.Parsers.Count = Error_Count + Zombie_Count then
         Verb := Error;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;

      if Resume_Active then
         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift and not Parser_State.Resume_Active then
               Parser_State.Set_Verb (Pause);
            end if;
         end loop;
      end if;
   end Parse_Verb;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out LR.Parser.Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser                         :    out          LR.Parser.Parser;
      Trace                          : not null access WisiToken.Trace'Class;
      Lexer                          : in              WisiToken.Lexer.Handle;
      Table                          : in              Parse_Table_Ptr;
      Language_Fixes                 : in              Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in              Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in              Language_String_ID_Set_Access;
      User_Data                      : in              WisiToken.Syntax_Trees.User_Data_Access;
      Max_Parallel                   : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      Terminate_Same_State           : in              Boolean            := True)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Trace     := Trace;
      Parser.Lexer     := Lexer;
      Parser.User_Data := User_Data;

      --  In Base_Parser; Tree, Line_Begin_Token, Last_Grammar_Node are default initialized.

      Parser.Table                          := Table;
      Parser.Language_Fixes                 := Language_Fixes;
      Parser.Language_Matching_Begin_Tokens := Language_Matching_Begin_Tokens;
      Parser.Language_String_ID_Set         := Language_String_ID_Set;

      Parser.Max_Parallel            := Max_Parallel;
      Parser.Terminate_Same_State    := Terminate_Same_State;
      Parser.Enable_McKenzie_Recover := not McKenzie_Defaulted (Table.all);

      --  In Parser; String_Quote_Checked, Post_Recover, Parsers are default
      --  initialized. Recover_Log_File, Partial_Parse_Active are set by
      --  user after this.

      if User_Data /= null then
         User_Data.Set_Lexer (Lexer);
      end if;
   end New_Parser;

   overriding procedure Parse (Shared_Parser : aliased in out LR.Parser.Parser)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type Syntax_Trees.User_Data_Access;
      use all type Ada.Containers.Count_Type;

      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Current_Verb   : All_Parse_Action_Verbs;
      Action         : Parse_Action_Node_Ptr;
      Zombie_Count   : SAL.Base_Peek_Type;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is
         procedure Report_Error
         is begin
            Shared_Parser.Parsers.First_State_Ref.Errors.Append
              ((Label          => LR.Message,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Recover        => <>,
                Msg            => +"error during resume"));
            if Debug_Mode then
               raise SAL.Programmer_Error with Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) &
                 ": error during resume";
            else
               raise Syntax_Error;
            end if;
         end Report_Error;

      begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input. This is how grammar conflicts
            --  are resolved when the input text is valid, in which case we should
            --  just terminate this parser. However, this may be due to invalid
            --  input text, so we keep the parser alive but suspended for a few
            --  tokens, to see if the other parsers also error, in which case they
            --  all participate in error recovery.

            --  We do not create zombie parsers during resume.
            if not Check_Parser.State_Ref.Resume_Active then
               --  Parser is now a zombie
               if Trace_Parse > Detail then
                  Trace.Put_Line (Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) & ": zombie");
               end if;
               Check_Parser.Next;

            else
               if Shared_Parser.Parsers.Count = 1 then
                  Report_Error;

               else
                  --  This is ok if a conflict occured during resume - we assume this is
                  --  a branch that failed during recover as well. Otherwise it's a
                  --  programmer error.
                  if Check_Parser.State_Ref.Conflict_During_Resume then
                     Shared_Parser.Parsers.Terminate_Parser
                       (Check_Parser, Shared_Parser.Tree, "error in conflict during resume", Shared_Parser.Trace.all);
                  else
                     Report_Error;
                  end if;
               end if;
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      if Debug_Mode then
         Trace.Put_Clock ("start");
      end if;

      if Shared_Parser.User_Data /= null then
         Shared_Parser.User_Data.Reset;
      end if;

      Shared_Parser.String_Quote_Checked := Invalid_Line_Number;
      Shared_Parser.Tree.Clear;
      Shared_Parser.Parsers              := Parser_Lists.New_List (Shared_Parser.Tree);

      Shared_Parser.Lex_All;

      Shared_Parser.Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

      Main_Loop :
      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Current_Verb, Zombie_Count);

         if Trace_Parse > Extra then
            Trace.Put_Line ("cycle start; current_verb: " & Parse_Action_Verbs'Image (Current_Verb));
         end if;

         case Current_Verb is
         when Pause =>
            null;

         when Shift =>
            --  We just shifted a token; get the next token from
            --  Shared_Parser.Terminals.

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Error then
                  if Shared_Parser.Enable_McKenzie_Recover then
                     Parser_State.Zombie_Token_Count := Parser_State.Zombie_Token_Count + 1;
                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": zombie (" &
                             Syntax_Trees.Element_Index'Image
                               (Shared_Parser.Table.McKenzie_Param.Check_Limit - Parser_State.Zombie_Token_Count) &
                             " tokens remaining)");
                     end if;
                  end if;

               elsif Parser_State.Verb = Shift then
                  declare
                     use Recover_Op_Arrays, Recover_Op_Array_Refs;
                     use all type WisiToken.Syntax_Trees.Stream_Index;

                     function Insert_Virtual return Boolean
                     is
                        Ins_Del     : Vector renames Parser_State.Recover_Insert_Delete;
                        Ins_Del_Cur : Extended_Index renames Parser_State.Recover_Insert_Delete_Current;
                        Result      : Boolean := False;
                     begin
                        if Ins_Del_Cur /= No_Index then
                           declare
                              Op : Recover_Op renames Variable_Ref (Ins_Del, Ins_Del_Cur);
                           begin
                              if Op.Op = Insert and then
                                Op.Ins_Before =
                                (if Parser_State.Inc_Shared_Token
                                 then Shared_Parser.Tree.Stream_Next (Parser_State.Stream, Parser_State.Shared_Token)
                                 else Parser_State.Shared_Token)
                              then
                                 Result := True;

                                 Parser_State.Current_Token := Shared_Parser.Tree.Add_Terminal
                                   (Parser_State.Stream, Op.Ins_ID, Before => Op.Ins_Before);

                                 Op.Ins_Tree_Node := Shared_Parser.Tree.Get_Node (Parser_State.Current_Token);

                                 Ins_Del_Cur := Ins_Del_Cur + 1;
                                 if Ins_Del_Cur > Last_Index (Ins_Del) then
                                    Ins_Del_Cur := No_Index;
                                 end if;
                              end if;
                           end;
                        end if;
                        return Result;
                     end Insert_Virtual;
                  begin
                     if Insert_Virtual then
                        null;

                     elsif (if Parser_State.Inc_Shared_Token
                            then Shared_Parser.Tree.Stream_Next
                                (Shared_Parser.Tree.Terminal_Stream, Parser_State.Shared_Token)
                            else Parser_State.Shared_Token) /= Syntax_Trees.Invalid_Stream_Index
                     then
                        if Parser_State.Inc_Shared_Token then
                           --  Inc_Shared_Token is only set False by McKenzie_Recover; see there
                           --  for when/why. Don't increment past wisi_eoi (happens when input
                           --  buffer is empty; test_mckenzie_recover.adb Empty_Comments).
                           if Parser_State.Shared_Token = Syntax_Trees.Invalid_Stream_Index then
                              Parser_State.Shared_Token := Shared_Parser.Tree.Stream_First
                                (Shared_Parser.Tree.Terminal_Stream);
                           else
                              Parser_State.Shared_Token := Shared_Parser.Tree.Stream_Next
                                (Shared_Parser.Tree.Terminal_Stream, Parser_State.Shared_Token);
                           end if;
                        else
                           Parser_State.Inc_Shared_Token := True;
                        end if;

                        Parser_State.Current_Token := Parser_State.Shared_Token;

                     end if;

                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                             ": current_token " & Shared_Parser.Tree.Image
                             (Parser_State.Current_Token, Trace.Descriptor.all) &
                             " recover_insert_delete:" &
                             (if Parser_State.Recover_Insert_Delete_Current = No_Index
                              then ""
                              else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                                 Image (Parser_State.Recover_Insert_Delete, Shared_Parser.Trace.Descriptor.all)));
                     end if;
                  end;
               end if;
            end loop;

         when Accept_It =>
            --  All parsers accepted or are zombies.
            declare
               Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
               Current_Parser : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  exit Main_Loop;

               elsif Zombie_Count + 1 = Count then
                  --  All but one are zombies
                  loop
                     if Current_Parser.Verb = Accept_It then
                        Current_Parser.Next;
                     else
                        declare
                           Temp  : Parser_Lists.Cursor := Current_Parser;
                        begin
                           Current_Parser.Next;
                           Shared_Parser.Parsers.Terminate_Parser
                             (Temp, Shared_Parser.Tree, "zombie", Shared_Parser.Trace.all);
                        end;
                     end if;
                     exit when Current_Parser.Is_Done;
                  end loop;

                  exit Main_Loop;

               else
                  --  More than one parser is active.
                  declare
                     use all type Parser_Lists.Cursor;
                     Error_Parser_Count : Integer := (if Shared_Parser.Lexer.Errors.Length > 0 then 1 else 0);

                     Recover_Cost           : Integer;
                     Min_Recover_Cost       : Integer                   := Integer'Last;
                     Recover_Ops_Length     : Ada.Containers.Count_Type;
                     Min_Recover_Ops_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
                     Recover_Cur            : Parser_Lists.Cursor       := Current_Parser;
                  begin
                     Current_Parser := Shared_Parser.Parsers.First;
                     loop
                        if Current_Parser.Verb = Accept_It then
                           if Current_Parser.State_Ref.Errors.Length > 0 then
                              Error_Parser_Count := Error_Parser_Count + 1;
                           end if;
                           Current_Parser.Next;
                        else
                           declare
                              Temp  : Parser_Lists.Cursor := Current_Parser;
                           begin
                              Current_Parser.Next;
                              Shared_Parser.Parsers.Terminate_Parser
                                (Temp, Shared_Parser.Tree, "zombie", Shared_Parser.Trace.all);
                           end;
                        end if;
                        exit when Current_Parser.Is_Done;
                     end loop;

                     if Error_Parser_Count > 0 then
                        --  There was at least one error. We assume that caused the ambiguous
                        --  parse, and we pick the parser with the minimum cost and minimum
                        --  recover ops length (consistent with Duplicate_State) to allow the
                        --  parse to succeed. We terminate the other parsers so the remaining
                        --  parser can do Execute_Actions.
                        --
                        --  If there are multiple errors, this metric is not very meaningful.
                        --
                        --  Note all surviving parsers must have the same error count.
                        Current_Parser := Shared_Parser.Parsers.First;
                        loop
                           Recover_Cost := Current_Parser.Min_Recover_Cost;
                           if Recover_Cost < Min_Recover_Cost then
                              Min_Recover_Cost       := Recover_Cost;
                              Min_Recover_Ops_Length := Current_Parser.Max_Recover_Ops_Length;
                              Recover_Cur            := Current_Parser;

                           elsif Recover_Cost = Min_Recover_Cost then
                              Recover_Ops_Length := Current_Parser.Max_Recover_Ops_Length;
                              if Recover_Ops_Length < Min_Recover_Ops_Length then
                                 Min_Recover_Ops_Length := Recover_Ops_Length;
                                 Recover_Cur    := Current_Parser;
                              end if;
                           end if;
                           Current_Parser.Next;
                           exit when Current_Parser.Is_Done;
                        end loop;

                        Current_Parser := Shared_Parser.Parsers.First;
                        loop
                           if Current_Parser = Recover_Cur then
                              Current_Parser.Next;
                           else
                              declare
                                 Temp  : Parser_Lists.Cursor := Current_Parser;
                              begin
                                 Current_Parser.Next;
                                 Shared_Parser.Parsers.Terminate_Parser
                                   (Temp, Shared_Parser.Tree,
                                    (if Recover_Cost = Min_Recover_Cost and then
                                       Recover_Ops_Length = Min_Recover_Ops_Length
                                     then "random"
                                     else "recover cost/length"),
                                    Shared_Parser.Trace.all);
                              end;
                           end if;
                           exit when Current_Parser.Is_Done;
                        end loop;

                        exit Main_Loop;

                     else
                        --  There were no previous errors. We allow the parse to fail, on the
                        --  assumption that an otherwise correct input should not yield an
                        --  ambiguous parse.
                        declare
                           Token : Base_Token renames Shared_Parser.Tree.Base_Token
                             (Shared_Parser.Tree.Get_Node (Shared_Parser.Tree.Stream_Last (Current_Parser.Stream)));
                        begin
                           raise WisiToken.Parse_Error with Error_Message
                             (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                              "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
                        end;
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

               Recover_Result : McKenzie_Recover.Recover_Status := McKenzie_Recover.Recover_Status'First;

               Pre_Recover_Parser_Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
               Start : Ada.Calendar.Time;
            begin
               --  Recover algorithms expect current token at
               --  Parsers(*).Current_Token, will set
               --  Parsers(*).Recover_Insert_Delete with new input tokens and
               --  deletions, adjust Parsers(*).Stack, and set
               --  Parsers(*).Current_Token and Parsers(*).Verb.

               if Shared_Parser.Enable_McKenzie_Recover then
                  if Debug_Mode then
                     Trace.Put_Clock ("pre-recover" & Shared_Parser.Parsers.Count'Img & " active");
                     Start := Ada.Calendar.Clock;
                  end if;
                  Recover_Result := McKenzie_Recover.Recover (Shared_Parser);
                  if Debug_Mode then
                     declare
                        use Ada.Calendar;
                        Recover_Duration : constant Duration := Clock - Start;
                     begin
                        Trace.Put_Clock
                          ("post-recover" & Shared_Parser.Parsers.Count'Img & " active," & Recover_Duration'Image);
                     end;
                  end if;

                  if Trace_Parse > Outline then
                     if Recover_Result = Success  then
                        Trace.New_Line;
                        Trace.Put_Line
                          ("recover: succeed, parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     else
                        Trace.Put_Line
                          ("recover: fail " & McKenzie_Recover.Recover_Status'Image (Recover_Result) &
                             ", parser count" & SAL.Base_Peek_Type'Image (Shared_Parser.Parsers.Count));
                     end if;
                  end if;

                  if Ada.Text_IO.Is_Open (Shared_Parser.Recover_Log_File) then
                     declare
                        use Ada.Text_IO;
                     begin
                        Put
                          (Shared_Parser.Recover_Log_File,
                           Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " " &
                             Shared_Parser.Partial_Parse_Active'Image & " " &
                             Recover_Result'Image & " " &
                             Pre_Recover_Parser_Count'Image & " '" &
                             Shared_Parser.Lexer.File_Name & "'");

                        Put (Shared_Parser.Recover_Log_File, '(');
                        for Parser of Shared_Parser.Parsers loop
                           if Parser.Recover.Results.Count > 0 then
                              --  Count can be 0 when error recovery fails
                              Put (Shared_Parser.Recover_Log_File, Image (Parser.Recover.Results.Peek.Strategy_Counts));
                           end if;
                           Put
                             (Shared_Parser.Recover_Log_File,
                              Integer'Image (Parser.Recover.Enqueue_Count) &
                                Integer'Image (Parser.Recover.Check_Count) & " " &
                                Boolean'Image (Parser.Recover.Success));
                        end loop;
                        Put (Shared_Parser.Recover_Log_File, ')');

                        New_Line (Shared_Parser.Recover_Log_File);
                        Flush (Shared_Parser.Recover_Log_File);
                     exception
                     when others =>
                        New_Line (Shared_Parser.Recover_Log_File);
                        Flush (Shared_Parser.Recover_Log_File);
                     end;
                  end if;
               else
                  if Trace_Parse > Outline or Trace_McKenzie > Outline then
                     Trace.Put_Line ("recover disabled");
                  end if;
               end if;

               if Recover_Result = Success then
                  for Parser_State of Shared_Parser.Parsers loop
                     Parser_State.Resume_Active          := True;
                     Parser_State.Conflict_During_Resume := False;

                     if Trace_Parse > Outline then
                        Trace.Put_Line
                          (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": Current_Token " &
                             Shared_Parser.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all) &
                             " Shared_Token " & Shared_Parser.Tree.Image
                               (Parser_State.Shared_Token, Trace.Descriptor.all) &
                             " recover_insert_delete:" &
                             (if Parser_State.Recover_Insert_Delete_Current = Recover_Op_Arrays.No_Index
                              then ""
                              else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                                 Image (Parser_State.Recover_Insert_Delete, Shared_Parser.Trace.Descriptor.all)));

                        if Trace_Parse > Detail then
                           Shared_Parser.Trace.Put_Line
                             (Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                                ": resume_active: True, token goal" &
                                Parser_State.Resume_Token_Goal'Image &
                                ", inc_shared_token: " & Parser_State.Inc_Shared_Token'Image);
                        end if;
                     end if;

                     Parser_State.Zombie_Token_Count := 0;

                     case Parser_State.Verb is
                     when Error =>
                        --  Force this parser to be terminated.
                        if Shared_Parser.Enable_McKenzie_Recover then
                           Parser_State.Zombie_Token_Count := Shared_Parser.Table.McKenzie_Param.Check_Limit + 1;
                        end if;

                     when Shift =>
                        null;

                     when Reduce | Pause | Accept_It =>
                        raise SAL.Programmer_Error;
                     end case;
                  end loop;

                  if Trace_Parse > Detail then
                     Shared_Parser.Trace.New_Line;
                  end if;

               else
                  --  Terminate with error. Parser_State has all the required info on
                  --  the original error (recorded by Error in Do_Action); report reason
                  --  recover failed.
                  for Parser_State of Shared_Parser.Parsers loop
                     Parser_State.Errors.Append
                       ((Label          => LR.Message,
                         First_Terminal => Trace.Descriptor.First_Terminal,
                         Last_Terminal  => Trace.Descriptor.Last_Terminal,
                         Recover        => <>,
                         Msg            =>
                           (if Shared_Parser.Enable_McKenzie_Recover
                            then +"recover: fail " & McKenzie_Recover.Recover_Status'Image (Recover_Result)
                            else +"recover disabled")));
                  end loop;
                  raise WisiToken.Syntax_Error;
               end if;

               --  Recover sets Parser.Verb to Shift for all active parsers, to
               --  indicate it no longer has an error. Set Current_Verb to reflect
               --  that.
               Current_Verb := Shift;
            end;
         end case;

         --  We don't use 'for Parser_State of Parsers loop' here,
         --  because terminate on error and spawn on conflict require
         --  changing the parser list.
         declare
            Current_Parser : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
         begin
            Action_Loop :
            loop
               exit Action_Loop when Current_Parser.Is_Done;

               --  We don't check duplicate state during resume, because the tokens
               --  inserted/deleted by error recover may cause initially duplicate
               --  states to diverge.
               if not Current_Parser.State_Ref.Resume_Active and
                 Shared_Parser.Terminate_Same_State and
                 Current_Verb = Shift
               then
                  Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Tree, Shared_Parser.Trace.all);
                  --  If Duplicate_State terminated Current_Parser, Current_Parser now
                  --  points to the next parser. Otherwise it is unchanged.
               end if;

               exit Action_Loop when Current_Parser.Is_Done;

               if Trace_Parse > Extra then
                  Trace.Put_Line
                    ("current_verb: " & Parse_Action_Verbs'Image (Current_Verb) &
                       "," & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) &
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
                        Trace.Put_Line (Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": zombie");
                     end if;

                     Current_Parser.Next;
                  else
                     Shared_Parser.Parsers.Terminate_Parser
                       (Current_Parser, Shared_Parser.Tree, "zombie", Shared_Parser.Trace.all);
                  end if;

               elsif Current_Parser.Verb = Current_Verb then

                  if Trace_Parse > Extra then
                     Trace.Put (Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": stack: ");
                     Trace.Put_Line
                       (Parser_Lists.Image (Current_Parser.Stream, Trace.Descriptor.all, Shared_Parser.Tree));
                  end if;

                  declare
                     Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
                  begin
                     Action := Action_For
                       (Table => Shared_Parser.Table.all,
                        State => Shared_Parser.Tree.State (Parser_State.Stream),
                        ID    => Shared_Parser.Tree.ID (Parser_State.Current_Token));
                  end;

                  declare
                     Conflict : Parse_Action_Node_Ptr := Action.Next;
                  begin
                     loop
                        exit when Conflict = null;
                        --  Spawn a new parser (before modifying Current_Parser stack).

                        Current_Parser.State_Ref.Conflict_During_Resume := Current_Parser.State_Ref.Resume_Active;

                        if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                           --  If errors were recovered, terminate a parser that used the
                           --  highest cost solution.
                           declare
                              use all type WisiToken.Parse.LR.Parser_Lists.Cursor;
                              Max_Recover_Cost : Integer             := 0;
                              Cur              : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                              Max_Parser       : Parser_Lists.Cursor := Cur;
                           begin
                              loop
                                 exit when Cur.Is_Done;
                                 if Cur.Total_Recover_Cost > Max_Recover_Cost then
                                    Max_Parser       := Cur;
                                    Max_Recover_Cost := Cur.Total_Recover_Cost;
                                 end if;
                                 Cur.Next;
                              end loop;

                              if Max_Recover_Cost > 0 then
                                 if Max_Parser = Current_Parser then
                                    Current_Parser.Next;

                                    Shared_Parser.Parsers.Terminate_Parser
                                      (Max_Parser, Shared_Parser.Tree, "too many parsers; max error repair cost",
                                       Trace);

                                    --  We changed Current_Parser, so start over
                                    goto Continue_Action_Loop;
                                 else
                                    Shared_Parser.Parsers.Terminate_Parser
                                      (Max_Parser, Shared_Parser.Tree, "too many parsers; max error repair cost",
                                       Trace);
                                 end if;
                              end if;
                           end;
                        end if;

                        if Shared_Parser.Parsers.Count = Shared_Parser.Max_Parallel then
                           declare
                              Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                              Token : constant Base_Token := Shared_Parser.Tree.Base_Token
                                (Shared_Parser.Tree.Get_Node (Parser_State.Shared_Token));
                           begin
                              raise WisiToken.Parse_Error with Error_Message
                                (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                                 "too many parallel parsers required in grammar state" &
                                   Shared_Parser.Tree.State (Parser_State.Stream)'Image &
                                   "; simplify grammar, or increase max-parallel (" &
                                   SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");
                           end;

                        else
                           --  FIXME: this is still a useful check; parallel parsers should not last very long.
                           --  Need to count nodes created by a stream.
                           --
                           --  if Debug_Mode then
                           --     if .Tree.Branched_Tree_Node_Count >
                           --       Shared_Parser.Branched_Tree_Limit
                           --     then
                           --        declare
                           --           Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                           --           Token : Base_Token renames Shared_Parser.Terminals (Parser_State.Shared_Token);
                           --        begin
                           --           raise WisiToken.Parse_Error with Error_Message
                           --             (Shared_Parser.Lexer.File_Name, Token.Line, Token.Column,
                           --              "branched syntax tree growing too large (>" &
                           --                Shared_Parser.Branched_Tree_Limit'Image &
                           --                "), last shared node " &
                           --                Shared_Parser.Tree.Image
                           --                  (Shared_Parser.Tree.Max_Shared_Node, Trace.Descriptor.all) &
                           --                "; improve grammar or increase Parser.Branched_Tree_Limit.");
                           --        end;
                           --     end if;
                           --  end if;

                           if Trace_Parse > Outline then
                              declare
                                 Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                              begin
                                 Trace.Put_Line
                                   (Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": " &
                                      Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": " &
                                      Shared_Parser.Tree.Image
                                        (Parser_State.Current_Token, Trace.Descriptor.all) & " : " &
                                      "spawn" & Shared_Parser.Tree.Next_Stream_ID_Image & ", (" &
                                      Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                                 if Debug_Mode then
                                    Trace.Put_Line ("tree size: " & Shared_Parser.Tree.Tree_Size_Image);
                                 end if;
                              end;
                           end if;

                           Shared_Parser.Parsers.Prepend_Copy (Current_Parser, Shared_Parser.Tree, Trace);
                           Do_Action (Conflict.Item, Shared_Parser.Parsers.First, Shared_Parser);

                           --  We must terminate error parsers immediately in order to avoid
                           --  zombie parsers during recovery.
                           declare
                              Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                           begin
                              Check_Error (Temp);
                           end;
                        end if;

                        Conflict := Conflict.Next;
                     end loop;
                  end;
                  Do_Action (Action.Item, Current_Parser, Shared_Parser);
                  Check_Error (Current_Parser);

               else
                  --  Current parser is waiting for others to catch up
                  Current_Parser.Next;
               end if;
               <<Continue_Action_Loop>>
            end loop Action_Loop;
         end;
      end loop Main_Loop;

      if Trace_Parse > Outline then
         Trace.Put_Line (Shared_Parser.Tree.Trimmed_Image (Shared_Parser.Parsers.First.Stream) & ": succeed");
      end if;

      if Debug_Mode then
         Trace.Put_Clock ("finish parse");
      end if;

      --  We don't raise Syntax_Error for lexer errors, since they are all
      --  recovered, either by inserting a quote, or by ignoring the
      --  character.
   exception
   when Syntax_Error | WisiToken.Parse_Error | Partial_Parse =>
      if Debug_Mode then
         Trace.Put_Clock ("finish - error");
      end if;
      raise;

   when E : others =>
      declare
         Msg : constant String := Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E);
      begin
         if Shared_Parser.Parsers.Count > 0 then
            --  Emacs displays errors in the *syntax-errors* buffer
            Shared_Parser.Parsers.First_State_Ref.Errors.Append
              ((Label          => LR.Message,
                First_Terminal => Trace.Descriptor.First_Terminal,
                Last_Terminal  => Trace.Descriptor.Last_Terminal,
                Recover        => <>,
                Msg            => +Msg));
         end if;

         if Debug_Mode then
            Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
            Trace.New_Line;
         end if;

         --  Emacs displays the exception message in the echo area; easy to miss
         raise WisiToken.Parse_Error with Msg;
      end;
   end Parse;

   overriding
   procedure Execute_Actions
     (Parser          : in out LR.Parser.Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null)
   is
      use all type Syntax_Trees.User_Data_Access;
      use all type WisiToken.Syntax_Trees.Semantic_Action;

      Descriptor : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use all type Syntax_Trees.Node_Label;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         declare
            Tree_Children : constant Syntax_Trees.Node_Access_Array := Tree.Children (Node);
         begin
            Parser.User_Data.Reduce (Tree, Node, Tree_Children);
            if Tree.Action (Node) /= null then
               begin
                  Tree.Action (Node)
                    (Parser.User_Data.all, Tree, Node, Syntax_Trees.To_Valid_Node_Access (Tree_Children));
               exception
               when E : others =>
                  declare
                     Token : Base_Token renames Tree.Base_Token (Node);
                  begin
                     raise WisiToken.Parse_Error with Error_Message
                       (Parser.Lexer.File_Name, Token.Line, Token.Column,
                        "action raised exception " & Ada.Exceptions.Exception_Name (E) & ": " &
                          Ada.Exceptions.Exception_Message (E));
                  end;
               end;
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data /= null then
         if Parser.Parsers.Count > 1 then
            raise Syntax_Error with "ambiguous parse; can't execute actions";
         end if;

         declare
            use Recover_Op_Arrays, Recover_Op_Array_Refs;
            Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;
         begin
            pragma Assert (Parser.Tree.Fully_Parsed);

            Parser.Tree.Set_Parents;

            if Trace_Action > Outline then
               if Trace_Action > Extra then
                  Parser.Tree.Print_Tree (Descriptor, Parser.Tree.Root, Image_Augmented);
                  Parser.Trace.New_Line;
               end if;
               Parser.Trace.Put_Line
                 (Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": root node: " & Parser.Tree.Image
                    (Parser.Tree.Root, Descriptor));
            end if;

            for I in First_Index (Parser_State.Recover_Insert_Delete) ..
              Last_Index (Parser_State.Recover_Insert_Delete)
            loop
               declare
                  Op : Recover_Op renames Constant_Ref (Parser_State.Recover_Insert_Delete, I);
               begin
                  case Op.Op is
                  when Insert =>
                     Parser.User_Data.Insert_Token (Parser.Tree, Op.Ins_Tree_Node);
                  when Delete =>
                     Parser.User_Data.Delete_Token (Parser.Tree, Parser.Tree.Get_Node (Op.Del_Token_Index));
                  end case;
               end;
            end loop;

            Parser.User_Data.Initialize_Actions (Parser.Tree);
            Parser.Tree.Process_Tree (Process_Node'Access);
         end;
      end if;
   end Execute_Actions;

   overriding function Any_Errors (Parser : in LR.Parser.Parser) return Boolean
   is
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
   begin
      return Parser.Parsers.Count > 1 or Parser_State.Errors.Length > 0 or Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in LR.Parser.Parser)
   is
      use Ada.Text_IO;

      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
      Descriptor   : WisiToken.Descriptor renames Parser.Trace.Descriptor.all;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      for Item of Parser_State.Errors loop
         case Item.Label is
         when Action =>
            if Parser.Tree.Is_Virtual (Parser.Tree.Get_Node (Item.Error_Token)) then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Parser.Lexer.File_Name, 1, 0,
                     "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                       ", found " & Image (Parser.Tree.ID (Item.Error_Token), Descriptor)));
            else
               declare
                  Token : constant Base_Token := Parser.Tree.Base_Token (Parser.Tree.Get_Node (Item.Error_Token));
               begin
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Parser.Lexer.File_Name, Token.Line, Token.Column,
                        "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                          ", found '" & Parser.Lexer.Buffer_Text (Token.Byte_Region) & "'"));
               end;
            end if;

         when Check =>
            Put_Line
              (Current_Error,
               Parser.Lexer.File_Name & ":1:0: semantic check error: " &
                 Semantic_Checks.Image (Item.Check_Status, Descriptor));

         when Message =>
            Put_Line (Current_Error, -Item.Msg);
         end case;

         if Item.Recover.Stack.Depth /= 0 then
            Put_Line (Current_Error, "   recovered: " & Image (Item.Recover.Ops, Descriptor));
         end if;
      end loop;
   end Put_Errors;

end WisiToken.Parse.LR.Parser;
