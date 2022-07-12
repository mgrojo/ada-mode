--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2022 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Calendar;
with WisiToken.Parse.LR.McKenzie_Recover;
with WisiToken.Parse.LR.Parser;
separate (WisiToken.Parse.Parser)
procedure LR_Core_Parse
  (Shared_Parser : in out WisiToken.Parse.Parser.Parser'Class;
   Log_File      : in     Ada.Text_IO.File_Type;
   Recover_Only  : in     Boolean)
is
   use WisiToken.Parse.LR.Parser;
   use WisiToken.Parse.LR;
   use WisiToken.Syntax_Trees;
   use all type Ada.Containers.Count_Type;
   use all type SAL.Base_Peek_Type;

   Trace : WisiToken.Trace'Class renames Shared_Parser.Tree.Lexer.Trace.all;

   Current_Verb : All_Parse_Action_Verbs;
   Zombie_Count : SAL.Base_Peek_Type;

begin
   Main_Loop :
   loop
      --  exit on Accept_It action, or Resume_Active false if Recover_Only,
      --  or unrecovered syntax error.

      Parse_Verb (Shared_Parser, Recover_Only, Current_Verb, Zombie_Count);

      exit Main_Loop when Recover_Only and then not Shared_Parser.Resume_Active;

      if Trace_Parse > Extra then
         Trace.Put_Line ("cycle start; current_verb: " & Image (Current_Verb));
      end if;

      case Current_Verb is
      when Pause =>
         null;

      when Shift =>
         --  We just shifted a token; get the next token from
         --  Tree.Shared_Stream, Tree parse stream input, or Parser
         --  Insert_Delete.

         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift then
               --  Handle inserting from Parser_State.Recover_Insert_Delete;
               --  otherwise, Tree.Current_Token is correct.

               declare
                  Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
               begin
                  if Parser_State.Current_Recover_Op /= No_Insert_Delete then
                     declare
                        Error_Ref : constant Syntax_Trees.Stream_Error_Ref := Parser_State.Current_Error_Ref (Tree);
                        Err : Error_Data'Class := Syntax_Trees.Error (Error_Ref);
                        Op  : Recover_Op_Nodes renames Recover_Op_Array_Var_Ref (Err)(Parser_State.Current_Recover_Op);
                     begin
                        if Op.Op = Insert then
                           declare
                              Next_Sequential_Terminal : constant Syntax_Trees.Terminal_Ref :=
                                Tree.First_Sequential_Terminal (Tree.Current_Token (Parser_State.Stream));
                           begin
                              if Op.Ins_Before = Tree.Get_Sequential_Index (Next_Sequential_Terminal.Node) then
                                 --  We know Next_Sequential_Terminal is the first terminal of
                                 --  Current_Token, and therefore we can insert before it. If it was
                                 --  embedded in a nonterm, that nonterm would have been broken down in
                                 --  order to shift the previous terminals.
                                 Op.Ins_Node := Tree.Insert_Virtual_Terminal (Parser_State.Stream, Op.Ins_ID).Node;

                                 Parser_State.Next_Recover_Op (Tree);

                                 Parser_State.Update_Error
                                   (Tree, Err,
                                    Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));
                              end if;
                           end;
                        end if;
                     end;
                  end if;

                  if Trace_Parse > Extra then
                     Trace.Put_Line
                       (" " & Tree.Trimmed_Image (Parser_State.Stream) &
                          ": current_token " & Tree.Image
                            (Shared_Parser.Tree.Current_Token (Parser_State.Stream), First_Terminal => True));
                     Trace.Put_Line
                       ("    shared_token " & Tree.Image (Shared_Parser.Tree.Shared_Token (Parser_State.Stream)));
                     if Tree.Has_Input (Parser_State.Stream) then
                        Trace.Put_Line
                          ("    stream input " & Tree.Image (Parser_State.Stream, Stack => False, Input => True));
                     end if;
                     if Parser_State.Current_Recover_Op /= No_Insert_Delete then
                        Trace.Put_Line
                          ("    recover_insert_delete:" & Parser_State.Recover_Image (Tree, Current_Only => True));
                     end if;
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
                          (Temp, Shared_Parser.Tree, "zombie", Trace);
                     end;
                  end if;
                  exit when Current_Parser.Is_Done;
               end loop;

               exit Main_Loop;

            else
               --  More than one parser is active.
               declare
                  use all type Parser_Lists.Cursor;
                  Error_Parser_Count : Integer := (if Shared_Parser.Tree.Lexer.Errors.Length > 0 then 1 else 0);

                  Recover_Cost           : Integer;
                  Min_Recover_Cost       : Integer                   := Integer'Last;
                  Recover_Ops_Length     : Ada.Containers.Count_Type;
                  Min_Recover_Ops_Length : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
                  Recover_Cur            : Parser_Lists.Cursor       := Current_Parser;
               begin
                  Current_Parser := Shared_Parser.Parsers.First;
                  loop
                     if Current_Parser.Verb = Accept_It then
                        if Current_Parser.State_Ref.Error_Count > 0 then
                           Error_Parser_Count := Error_Parser_Count + 1;
                        end if;
                        Current_Parser.Next;
                     else
                        declare
                           Temp  : Parser_Lists.Cursor := Current_Parser;
                        begin
                           Current_Parser.Next;
                           Shared_Parser.Parsers.Terminate_Parser
                             (Temp, Shared_Parser.Tree, "zombie", Trace);
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
                        Recover_Cost := Current_Parser.State_Ref.Total_Recover_Cost;
                        if Recover_Cost < Min_Recover_Cost then
                           Min_Recover_Cost       := Recover_Cost;
                           Min_Recover_Ops_Length := Current_Parser.State_Ref.Max_Recover_Ops_Length;
                           Recover_Cur            := Current_Parser;

                        elsif Recover_Cost = Min_Recover_Cost then
                           Recover_Ops_Length := Current_Parser.State_Ref.Max_Recover_Ops_Length;
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
                                  else "recover cost/min length"),
                                 Trace);
                           end;
                        end if;
                        exit when Current_Parser.Is_Done;
                     end loop;

                     exit Main_Loop;

                  else
                     --  There were no previous errors. We allow the parse to fail, on the
                     --  assumption that an otherwise correct input should not yield an
                     --  ambiguous parse.
                     Current_Parser := Shared_Parser.Parsers.First;
                     raise WisiToken.Parse_Error with Shared_Parser.Tree.Error_Message
                       (Shared_Parser.Tree.Current_Token (Current_Parser.Stream),
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

            Recover_Result : McKenzie_Recover.Recover_Status := Fail_Check_Delta;

            Pre_Recover_Parser_Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
            Start : Ada.Calendar.Time;
         begin
            --  Recover algorithms expect current token at
            --  Parsers(*).Current_Token, will set
            --  Parsers(*).Recover_Insert_Delete with new input tokens and
            --  deletions, adjust Parsers(*).Stack, and set
            --  Parsers(*).Current_Token and Parsers(*).Verb.

            if Trace_Time then
               Trace.Put_Clock ("pre-recover" & Shared_Parser.Parsers.Count'Img & " active");
               Start := Ada.Calendar.Clock;
            end if;

            if not Shared_Parser.Table.Error_Recover_Enabled then
               if Trace_Parse > Outline or Trace_McKenzie > Outline then
                  Trace.Put_Line ("recover disabled");
               end if;
            else
               Recover_Result := McKenzie_Recover.Recover (Shared_Parser);
               if Trace_Time then
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

               if Ada.Text_IO.Is_Open (Log_File) then
                  Recover_To_Log (Shared_Parser, Log_File, Recover_Result, Pre_Recover_Parser_Count);
               end if;
            end if;

            if Recover_Result = Success then
               Shared_Parser.Resume_Active := True;

               if Trace_Packrat_McKenzie > Detail then
                  Trace.Put_Line (" shared_stream:");
                  Trace.Put_Line (Shared_Parser.Tree.Image (Shared_Parser.Tree.Shared_Stream, Node_Numbers => True));
               end if;

               for Parser_State of Shared_Parser.Parsers loop
                  Parser_State.Resume_Active          := True;
                  Parser_State.Conflict_During_Resume := False;

                  case Parser_State.Verb is
                  when Error =>
                     --  Force this parser to be terminated.
                     Parser_State.Zombie_Token_Count := Shared_Parser.Table.McKenzie_Param.Zombie_Limit + 1;

                     if Trace_Parse > Outline and Trace_McKenzie <= Extra then
                        Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": fail:");
                     end if;

                  when Shift =>
                     Parser_State.Zombie_Token_Count := 0;
                     if (Trace_Parse > Detail and Trace_McKenzie <= Extra) or Trace_Packrat_McKenzie > Detail then
                        Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": stack/stream:");
                        Trace.Put_Line
                          (Shared_Parser.Tree.Image
                             (Parser_State.Stream, Stack => True, Input => True, Shared => True,
                              Node_Numbers => not Trace_Parse_No_State_Numbers,
                              Children => Trace_Parse > Detail));
                        Trace.Put_Line
                          ("    Current_Token: " & Shared_Parser.Tree.Image
                             (Shared_Parser.Tree.Current_Token (Parser_State.Stream), Terminal_Node_Numbers => True));
                        Trace.Put_Line
                          ("    Shared_Token: " & Shared_Parser.Tree.Image
                             (Shared_Parser.Tree.Shared_Token (Parser_State.Stream), Terminal_Node_Numbers => True));
                        Trace.Put_Line
                          ("    recover_insert_delete:" &
                             (if Parser_State.Current_Recover_Op = No_Insert_Delete
                              then ""
                              else Parser_State.Recover_Image (Shared_Parser.Tree, Current_Only => True)));

                        if Trace_Parse > Detail then
                           Trace.Put_Line
                             ("    resume_active: True, token goal" & Parser_State.Resume_Token_Goal'Image);
                        end if;
                     end if;

                  when Reduce | Pause | Accept_It =>
                     raise SAL.Programmer_Error;
                  end case;
               end loop;

               if Trace_Parse > Detail or Trace_Packrat_McKenzie > Detail then
                  Trace.New_Line;
               end if;

            else
               --  Terminate with error (parse_error because user expects parse to
               --  succeed on Syntax_Error). Parser_State has all the required info
               --  on the original error (recorded by Error in Do_Action).
               McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
               raise WisiToken.Parse_Error with "recover fail: " & Recover_Result'Image;
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
            if not Shared_Parser.Resume_Active and Current_Verb = Shift then
               Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Tree, Trace);
               --  If Duplicate_State terminated Current_Parser, Current_Parser now
               --  points to the next parser. Otherwise it is unchanged.
            end if;

            exit Action_Loop when Current_Parser.Is_Done;

            if Trace_Parse > Extra then
               declare
                  Parser_State : Parser_Lists.Parser_State renames Shared_Parser.Parsers
                    (Parser_Lists.To_Parser_Node_Access (Current_Parser));
               begin
                  Trace.Put_Line
                    (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                       ".verb: " & Image (Parser_State.Verb));
                  Trace.Put_Line
                    (" ... stack/stream: " &
                       Shared_Parser.Tree.Image
                         (Parser_State.Stream, Stack => True, Input => True, Shared => True, Children => False,
                          State_Numbers => not Trace_Parse_No_State_Numbers));
                  if Parser_State.Current_Recover_Op /= No_Insert_Delete then
                     Trace.Put_Line
                       (" ... recover_insert_delete:" & Parser_State.Recover_Image
                          (Shared_Parser.Tree, Current_Only => True));
                  end if;
               end;
            end if;

            --  Each branch of the following 'if' calls either Current_Parser.Free
            --  (which advances to the next parser) or Current_Parser.Next.

            if Current_Parser.Verb = Error then
               --  This parser is a zombie; see Check_Error.
               --
               --  Check to see if it is time to terminate it
               if Current_Parser.State_Ref.Zombie_Token_Count <= Shared_Parser.Table.McKenzie_Param.Zombie_Limit
               then
                  if Trace_Parse > Detail then
                     Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": zombie");
                  end if;

                  Current_Parser.Next;
               else
                  Shared_Parser.Parsers.Terminate_Parser
                    (Current_Parser, Shared_Parser.Tree, "zombie", Trace);
               end if;

            elsif Current_Parser.Verb = Current_Verb then

               declare
                  Action_Cur : Parse_Action_Node_Ptr;
                  Action     : Parse_Action_Rec;
                  Conflict   : Parse_Action_Node_Ptr;
               begin
                  Get_Action (Shared_Parser, Current_Parser.State_Ref, Action_Cur, Action);

                  Conflict := (if Action_Cur = null then null else Action_Cur.Next);

                  if Conflict /= null then
                     loop
                        exit when Conflict = null;
                        --  Spawn a new parser (before modifying Current_Parser stack).

                        Current_Parser.State_Ref.Conflict_During_Resume := Current_Parser.State_Ref.Resume_Active;

                        if Shared_Parser.Parsers.Count = Shared_Parser.Table.Max_Parallel then
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
                                 if Cur.State_Ref.Total_Recover_Cost > Max_Recover_Cost then
                                    Max_Parser       := Cur;
                                    Max_Recover_Cost := Cur.State_Ref.Total_Recover_Cost;
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

                        if Shared_Parser.Parsers.Count = Shared_Parser.Table.Max_Parallel then
                           declare
                              Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                           begin
                              raise WisiToken.Parse_Error with Shared_Parser.Tree.Error_Message
                                (Shared_Parser.Tree.Shared_Token (Parser_State.Stream),
                                 "too many parallel parsers required in grammar state" &
                                   Shared_Parser.Tree.State (Current_Parser.Stream)'Image &
                                   "; simplify grammar, or increase max-parallel (" &
                                   SAL.Base_Peek_Type'Image (Shared_Parser.Table.Max_Parallel) & ")");
                           end;

                        else
                           if Trace_Parse > Outline then
                              declare
                                 Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                              begin
                                 Trace.Put_Line
                                   (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": " &
                                      (if Trace_Parse_No_State_Numbers
                                       then "--"
                                       else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream))) & ": " &
                                      Shared_Parser.Tree.Image
                                        (Shared_Parser.Tree.Current_Token (Parser_State.Stream),
                                         Terminal_Node_Numbers => True) & " : " &
                                      "spawn " & Shared_Parser.Tree.Next_Stream_ID_Trimmed_Image & ", (" &
                                      Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                                 if Debug_Mode then
                                    Trace.Put_Line ("tree size: " & Shared_Parser.Tree.Tree_Size_Image);
                                 end if;
                              end;
                           end if;

                           Shared_Parser.Parsers.Prepend_Copy (Current_Parser, Shared_Parser.Tree);
                           Do_Action (Conflict.Item, Shared_Parser.Parsers.First, Shared_Parser);

                           --  We must terminate error parsers immediately in order to avoid
                           --  zombie parsers during recovery.
                           declare
                              Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                           begin
                              Check_Error (Shared_Parser, Temp);
                           end;
                        end if;

                        Conflict := Conflict.Next;
                     end loop;
                  end if;

                  Do_Action (Action, Current_Parser, Shared_Parser);
               end;
               Check_Error (Shared_Parser, Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
            <<Continue_Action_Loop>>
         end loop Action_Loop;
      end;
   end loop Main_Loop;

end LR_Core_Parse;
