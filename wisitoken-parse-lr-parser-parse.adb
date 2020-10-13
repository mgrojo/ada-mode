--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2020 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

separate (WisiToken.Parse.LR.Parser)
overriding procedure Parse (Shared_Parser : in out LR.Parser.Parser)
is
   use all type Ada.Strings.Unbounded.Unbounded_String;
   use all type Syntax_Trees.User_Data_Access;
   use all type Ada.Containers.Count_Type;

   Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

   Current_Verb   : All_Parse_Action_Verbs;
   Action         : Parse_Action_Node_Ptr;
   Zombie_Count   : SAL.Base_Peek_Type;

begin
   if Trace_Time then
      Trace.Put_Clock ("start");
   end if;

   --  Reset parser for new parse; in Base_Parser, Parser order. Lexer
   --  done by caller to set input text.
   Shared_Parser.Tree.Clear;
   if Shared_Parser.User_Data /= null then
      Shared_Parser.User_Data.Reset;
   end if;
   Shared_Parser.Wrapped_Lexer_Errors.Clear;

   --  Line_Begin_Token, Last_Grammar_Node done by Lex_All
   Shared_Parser.String_Quote_Checked := Invalid_Line_Number;

   Shared_Parser.Parsers := Parser_Lists.New_List (Shared_Parser.Tree);

   Shared_Parser.Lex_All;

   Shared_Parser.Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

   Main_Loop :
   loop
      --  exit on Accept_It action or syntax error.

      Parse_Verb (Shared_Parser, Current_Verb, Zombie_Count);

      if Trace_Parse > Extra then
         Trace.Put_Line ("cycle start; current_verb: " & Image (Current_Verb));
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
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": zombie (" &
                          Syntax_Trees.Element_Index'Image
                            (Shared_Parser.Table.McKenzie_Param.Zombie_Limit - Parser_State.Zombie_Token_Count) &
                          " tokens remaining)");
                  end if;
               end if;

            elsif Parser_State.Verb = Shift then
               declare
                  use Recover_Op_Arrays;
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
                              then Shared_Parser.Tree.Stream_Next (Parser_State.Shared_Token)
                              else Parser_State.Shared_Token)
                           then
                              Result := True;

                              Parser_State.Current_Token := Shared_Parser.Tree.Insert_Virtual_Terminal
                                (Parser_State.Stream, Op.Ins_ID, Op.Ins_Before);

                              Op.Ins_Node := Shared_Parser.Tree.Get_Node
                                (Parser_State.Stream, Parser_State.Current_Token);

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

                  elsif Parser_State.Shared_Token = Syntax_Trees.Invalid_Stream_Index or else
                    (if Parser_State.Inc_Shared_Token
                     then Shared_Parser.Tree.Stream_Next (Parser_State.Shared_Token)
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
                           Parser_State.Shared_Token := Shared_Parser.Tree.Stream_Next (Parser_State.Shared_Token);
                        end if;
                     else
                        Parser_State.Inc_Shared_Token := True;
                     end if;

                     Parser_State.Current_Token := Parser_State.Shared_Token;
                  end if;

                  if Trace_Parse > Extra then
                     Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                          ": current_token " & Shared_Parser.Tree.Image (Parser_State.Current_Token) &
                          " recover_insert_delete:" &
                          (if Parser_State.Recover_Insert_Delete_Current = No_Index
                           then ""
                           else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                             Image (Parser_State.Recover_Insert_Delete, Shared_Parser.Tree)));
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
                          (Shared_Parser.Tree.Get_Node
                             (Current_Parser.Stream, Shared_Parser.Tree.Stream_Last (Current_Parser.Stream)));
                     begin
                        raise WisiToken.Parse_Error with Error_Message
                          (Shared_Parser.Lexer.File_Name, Token.Line,
                           Column (Token, Shared_Parser.Line_Begin_Char_Pos),
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
               if Trace_Time then
                  Trace.Put_Clock ("pre-recover" & Shared_Parser.Parsers.Count'Img & " active");
                  Start := Ada.Calendar.Clock;
               end if;
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
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": Current_Token " &
                          Shared_Parser.Tree.Image (Parser_State.Current_Token, Terminal_Node_Numbers => True) &
                          " Shared_Token " & Shared_Parser.Tree.Image
                            (Parser_State.Shared_Token, Terminal_Node_Numbers => True) &
                          " recover_insert_delete:" &
                          (if Parser_State.Recover_Insert_Delete_Current = Recover_Op_Arrays.No_Index
                           then ""
                           else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                             Image (Parser_State.Recover_Insert_Delete, Shared_Parser.Tree)));

                     if Trace_Parse > Detail then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
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
                        Parser_State.Zombie_Token_Count := Shared_Parser.Table.McKenzie_Param.Zombie_Limit + 1;
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
                      First_Terminal => Shared_Parser.Descriptor.First_Terminal,
                      Last_Terminal  => Shared_Parser.Descriptor.Last_Terminal,
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
              Current_Verb = Shift
            then
               Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Tree, Shared_Parser.Trace.all);
               --  If Duplicate_State terminated Current_Parser, Current_Parser now
               --  points to the next parser. Otherwise it is unchanged.
            end if;

            exit Action_Loop when Current_Parser.Is_Done;

            if Trace_Parse > Extra then
               Trace.Put_Line
                 ("current_verb: " & Image (Current_Verb) &
                    ", " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) &
                    ".verb: " & Image (Current_Parser.Verb));
            end if;

            --  Each branch of the following 'if' calls either Current_Parser.Free
            --  (which advances to the next parser) or Current_Parser.Next.

            if Current_Parser.Verb = Error then
               --  This parser is a zombie; see Check_Error.
               --
               --  Check to see if it is time to terminate it
               if Shared_Parser.Enable_McKenzie_Recover and then
                 Current_Parser.State_Ref.Zombie_Token_Count <= Shared_Parser.Table.McKenzie_Param.Zombie_Limit
               then
                  if Trace_Parse > Detail then
                     Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": zombie");
                  end if;

                  Current_Parser.Next;
               else
                  Shared_Parser.Parsers.Terminate_Parser
                    (Current_Parser, Shared_Parser.Tree, "zombie", Shared_Parser.Trace.all);
               end if;

            elsif Current_Parser.Verb = Current_Verb then

               if Trace_Parse > Extra then
                  Trace.Put (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": stack: ");
                  Trace.Put_Line (Parser_Lists.Image (Current_Parser.Stream, Shared_Parser.Tree));
               end if;

               declare
                  Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
               begin
                  Action := Action_For
                    (Table => Shared_Parser.Table.all,
                     State => Shared_Parser.Tree.State (Parser_State.Stream),
                     ID    => Shared_Parser.Tree.ID (Parser_State.Stream, Parser_State.Current_Token));
               end;

               declare
                  Conflict : Parse_Action_Node_Ptr := Action.Next;
               begin
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

                     if Shared_Parser.Parsers.Count = Shared_Parser.Table.Max_Parallel then
                        declare
                           Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                           Token : constant Base_Token := Shared_Parser.Tree.Base_Token
                             (Shared_Parser.Tree.Get_Node (Parser_State.Shared_Token));
                        begin
                           raise WisiToken.Parse_Error with Error_Message
                             (Shared_Parser.Lexer.File_Name, Token.Line,
                              Column (Token, Shared_Parser.Line_Begin_Char_Pos),
                              "too many parallel parsers required in grammar state" &
                                Shared_Parser.Tree.State (Parser_State.Stream)'Image &
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
                                   Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": " &
                                   Shared_Parser.Tree.Image
                                     (Parser_State.Current_Token, Terminal_Node_Numbers => True) & " : " &
                                   "spawn " & Shared_Parser.Tree.Next_Stream_ID_Trimmed_Image & ", (" &
                                   Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                              if Debug_Mode then
                                 Trace.Put_Line ("tree size: " & Shared_Parser.Tree.Tree_Size_Image);
                              end if;
                           end;
                        end if;

                        Shared_Parser.Parsers.Prepend_Copy
                          (Current_Parser, Shared_Parser.Tree, Shared_Parser.User_Data, Trace);
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
               end;
               Do_Action (Action.Item, Current_Parser, Shared_Parser);
               Check_Error (Shared_Parser, Current_Parser);

            else
               --  Current parser is waiting for others to catch up
               Current_Parser.Next;
            end if;
            <<Continue_Action_Loop>>
         end loop Action_Loop;
      end;
   end loop Main_Loop;

   if Trace_Parse > Outline then
      Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (Shared_Parser.Parsers.First.Stream) & ": succeed");
   end if;

   if Trace_Time then
      Trace.Put_Clock ("finish parse");
   end if;

   --  We don't raise Syntax_Error for lexer errors, since they are all
   --  recovered, either by inserting a quote, or by ignoring the
   --  character.
exception
when Syntax_Error | WisiToken.Parse_Error | Partial_Parse =>
   if Trace_Time then
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
             First_Terminal => Shared_Parser.Descriptor.First_Terminal,
             Last_Terminal  => Shared_Parser.Descriptor.Last_Terminal,
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
