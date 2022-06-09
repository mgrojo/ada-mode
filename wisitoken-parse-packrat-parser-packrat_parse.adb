--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
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
with WisiToken.Parse.LR.McKenzie_Recover;
with WisiToken.Parse.LR.Parser;
separate (WisiToken.Parse.Parser)
procedure Packrat_Parse
  (Shared_Parser : in out Parser;
   Log_File      : in     Ada.Text_IO.File_Type)
is
   use WisiToken.Syntax_Trees;
   Derivs     : WisiToken.Parse.Packrat.Derivs renames Shared_Parser.Derivs;
   Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
   Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

   Recovered        : Boolean    := False;
   Max_Examined_Pos : Node_Index := 0;

   function To_Node_Index (Pos : in Stream_Index) return Node_Index
   is begin
      return Tree.Get_Node_Index (Tree.Shared_Stream, Pos);
   end To_Node_Index;

   function To_Stream_Index (Pos : in Node_Index) return Stream_Index
   is
      --  FIXME: build a map for this once.
      I : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
   begin
      if Pos = 0 then
         return Invalid_Stream_Index;
      end if;

      loop
         exit when Tree.Get_Node_Index (Tree.Get_Node (Tree.Shared_Stream, I)) = Pos;
         I := Tree.Stream_Next (Tree.Shared_Stream, I);
      end loop;
      return I;
   end To_Stream_Index;

   function Find_Shared_Stream_Index (Node : in Valid_Node_Access) return Stream_Index
   with Post => Tree.Contains (Tree.Shared_Stream, Find_Shared_Stream_Index'Result)
   --  Same as To_Stream_Index, but handles terminals copied to add
   --  errors.
   is
      --  FIXME: use node_index map to do binary search.
      I : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => False);
      Ref_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node, Trailing_Non_Grammar => False);
   begin
      loop
         exit when Tree.Byte_Region (Tree.Get_Node (Tree.Shared_Stream, I), Trailing_Non_Grammar => False) =
           Ref_Byte_Region;
         I := Tree.Stream_Next (Tree.Shared_Stream, I);
      end loop;
      return I;
   end Find_Shared_Stream_Index;

   procedure Derivs_To_Parse_Streams
     (Max_Examined_Pos : in out Node_Index;
      Max_Examined_Stream_Index : in out Stream_Index)
   --  Build Tree parse streams from Derivs
   is
      Pos : Node_Index := 1;

      procedure Find_Nodes
        (Pos                 : in     Node_Index;
         Terminal_Node       : in out Node_Access;
         Empty_Nonterm_Nodes : in out Valid_Node_Access_Lists.List;
         Nonterm_Nodes       : in out Valid_Node_Access_Lists.List)
      is
         use all type WisiToken.Parse.Packrat.Memo_State;
         Found_Nonterm_Node_Index : Node_Index := 0;
         Found_Nonterm            : Token_ID   := Invalid_Token_ID;
      begin
         for Nonterm in Derivs'Range loop
            if Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index and then
              Derivs (Nonterm)(Pos).State = Parse.Packrat.Success
            then
               declare
                  Node  : constant Valid_Node_Access := Derivs (Nonterm)(Pos).Result;
                  Index : constant Node_Index        := Tree.Get_Node_Index (Node);
               begin
                  if Tree.Is_Empty_Nonterm (Node) then
                     Empty_Nonterm_Nodes.Append (Node);

                  else
                     --  FIXME: could be more than one root nonterm here. need test case.
                     --  parents are not set; check if byte_region includes current entries?
                     --  build partial parent map?
                     if Index < Found_Nonterm_Node_Index then
                        Found_Nonterm_Node_Index := Index;
                        Found_Nonterm := Nonterm;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         if Found_Nonterm /= Invalid_Token_ID then
            Nonterm_Nodes.Append (Derivs (Found_Nonterm)(Pos).Result);
            return;
         end if;

         --  No nonterm, or only empty nonterm, at Pos; return the terminal
         --  from Tree.Shared_Stream.
         Terminal_Node := Tree.Get_Node (Tree.Shared_Stream, To_Stream_Index (Pos));
      end Find_Nodes;
   begin
      --  Find Max_Examined_Pos.
      Max_Examined_Pos := 0;
      for Nonterm in Derivs'Range loop
         for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop

            case Derivs (Nonterm)(Pos).State is
            when Parse.Packrat.No_Result =>
               null;

            when Parse.Packrat.Failure | Parse.Packrat.Success =>
               declare
                  Max_Node : constant Valid_Node_Access := Tree.Get_Node
                    (Tree.Shared_Stream, Derivs (Nonterm)(Pos).Max_Examined_Pos);
                  Max_Pos : constant Node_Index := Tree.Get_Node_Index (Max_Node);
               begin
                  if Max_Examined_Pos < Max_Pos then
                     Max_Examined_Pos  := Max_Pos;
                     Max_Examined_Stream_Index := Derivs (Nonterm)(Pos).Max_Examined_Pos;
                  end if;
               end;
            end case;
         end loop;
      end loop;

      if Trace_Packrat_McKenzie > Outline then
         Trace.Put_Line ("max_examined_pos:" & Max_Examined_Pos'Image);
      end if;

      if Max_Examined_Pos <= 1 then
         --  Source text is empty or only whitespace or comments. Nothing else to do.
         --  test_mckenzie_recover.adb Empty_Comments
         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("empty source text");
         end if;
         return;
      end if;

      loop
         --  Exit when reach Max_Examined_Pos.
         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("pos:" & Pos'Image);
         end if;

         declare
            use all type Ada.Containers.Count_Type;
            use WisiToken.Parse.LR;
            Stream : Syntax_Trees.Stream_ID renames Shared_Parser.Parsers.First.State_Ref.Stream;

            Terminal_Node       : Node_Access;
            Empty_Nonterm_Nodes : Valid_Node_Access_Lists.List;
            Nonterm_Nodes       : Valid_Node_Access_Lists.List;
            Prev_State          : State_Index := Tree.State (Stream);
            State               : Unknown_State_Index;
            Table               : Parse.LR.Parse_Table renames Shared_Parser.Table.all;
         begin
            Find_Nodes (Pos, Terminal_Node, Empty_Nonterm_Nodes, Nonterm_Nodes);

            --  FIXME: could be more than one nonterm valid; spawn parsers. need test case.
            if Nonterm_Nodes.Length > 0 then
               declare
                  Nonterm_Node : constant Valid_Node_Access := Nonterm_Nodes (Nonterm_Nodes.First);
               begin
                  State := Goto_For (Table, Prev_State, Tree.ID (Nonterm_Node));
                  Tree.Push (Stream, Nonterm_Node, State);

                  if Trace_Packrat_McKenzie > Detail then
                     Trace.Put_Line
                       ("state" & Prev_State'Image & " push " & Tree.Image (Nonterm_Node, Node_Numbers => True));
                  end if;

                  Pos := Tree.Get_Node_Index (Tree.Last_Terminal (Nonterm_Node)) + 1;
               end;

            else
               loop -- Handle empty nonterms.
                  pragma Assert (Terminal_Node /= Invalid_Node_Access);

                  declare
                     Action : constant Parse_Action_Node_Ptr := Action_For (Table, Prev_State, Tree.ID (Terminal_Node));
                  begin
                     if Action.Next /= null then
                        --  FIXME: spawn parser
                        raise SAL.Not_Implemented with "conflict in Derivs_To_Parse_Streams";
                     end if;

                     case Action.Item.Verb is
                     when Shift =>
                        State := Shift_State (Action);
                        Tree.Push (Stream, Terminal_Node, State);

                        Ada.Text_IO.Put_Line
                          ("state" & Prev_State'Image & " push " & Tree.Image (Terminal_Node, Node_Numbers => True));

                        Pos := @ + 1;
                        exit;

                     when Reduce =>
                        if Empty_Nonterm_Nodes.Length > 0 then
                           declare
                              Cur         : Valid_Node_Access_Lists.Cursor := Empty_Nonterm_Nodes.First;
                              To_Delete   : Valid_Node_Access_Lists.Cursor;
                              Shift_Count : Integer                        := 0;

                           begin
                              loop
                                 declare
                                    Empty_Nonterm_Node : constant Valid_Node_Access := Empty_Nonterm_Nodes (Cur);
                                 begin
                                    State := Goto_For (Table, Prev_State, Tree.ID (Empty_Nonterm_Node));
                                    if State /= Unknown_State then
                                       To_Delete := Cur;
                                       Valid_Node_Access_Lists.Next (Cur);
                                       Empty_Nonterm_Nodes.Delete (To_Delete);

                                       Tree.Push (Stream, Empty_Nonterm_Node, State);
                                       Shift_Count := @ + 1;
                                       Ada.Text_IO.Put_Line
                                         ("state" & Prev_State'Image & " push " &
                                            Tree.Image (Empty_Nonterm_Node, Node_Numbers => True));
                                       Prev_State := State;

                                    else
                                       Valid_Node_Access_Lists.Next (Cur);
                                    end if;
                                 end;
                                 exit when not Valid_Node_Access_Lists.Has_Element (Cur);
                              end loop;

                              if Shift_Count = 0 then
                                 --  FIXME: spawn parser?
                                 Empty_Nonterm_Nodes.Clear;
                              end if;
                           end;
                        else
                           raise SAL.Programmer_Error with "FIXME: non-empty reduce";
                        end if;

                     when Error =>
                        --  This is from look-ahead after a completed nonterm; see
                        --  test_mckenzie_recover.adb Error_2. The parser is trying to
                        --  complete a handled_sequence_of_statements for the body of Proc;
                        --  'block_1 ... end;' is in a sequence_of_statements, but 'if ... end
                        --  Block_2;' fails. sequence_of_statements is recursive, so the
                        --  parser is in a recursion loop; it finishes by completing
                        --  handled_sequence_of_statements containing just 'block_1 ... end;'
                        --  (it assumes the following tokens are 'end Proc;' or equivalent).
                        --
                        --  Terminal_Node is 'if'; the solution is to Undo_Reduce to a point
                        --  where the LR parser can shift 'if'.
                        if Trace_Packrat_McKenzie > Detail then
                           Trace.Put_Line
                             ("state" & Prev_State'Image & ": " & Tree.Image (Terminal_Node) &
                                " error; undo_reduce");
                           Trace.Put (" ... " & Tree.Image (Tree.Peek (Stream), State => True));
                        end if;
                        Undo_Reduce (Tree, Table, Stream, Shared_Parser.User_Data);
                        Prev_State := Tree.State (Stream);

                        if Trace_Packrat_McKenzie > Detail then
                           Trace.Put (" => " & Tree.Image (Tree.Peek (Stream), State => True),
                                      Prefix => False);
                           Trace.New_Line;
                        end if;

                     when others =>
                        raise SAL.Not_Implemented with "FIXME: action " & Action.Item.Verb'Image;
                     end case;
                  end;
               end loop;
            end if;

            exit when Pos >= Max_Examined_Pos;
         end;
      end loop;
   end Derivs_To_Parse_Streams;

   procedure Parse_Streams_To_Derivs
   is
      use all type WisiToken.Parse.Packrat.Memo_State;
      Stream        : Stream_ID  := Tree.First_Parse_Stream;
      Last_Edit_Pos : Node_Index := 0;
      Min_Seq_Stream_Index : Stream_Index;
      Max_Seq_Stream_Index : Stream_Index;
      --  Pos in Shared_Stream occupied by Parser.Min/Max_Sequential_Index after LR recover.

      procedure Set_Min_Max_Seq_Stream_Index
      is
         I : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
      begin
         --  FIXME: use node_index map to do binary search.
         loop
            declare
               Node : constant Valid_Node_Access := Tree.Get_Node (Tree.Shared_Stream, I);
               Node_Seq_Index : constant Base_Sequential_Index := Tree.Get_Sequential_Index (Node);
            begin
               if Node_Seq_Index /= Invalid_Sequential_Index and then
                 Node_Seq_Index > Shared_Parser.Min_Sequential_Index
               then
                  Min_Seq_Stream_Index := I;
                  exit;
               end if;

               if Tree.ID (Node) = Descriptor.EOI_ID then
                  --  Source text is empty or all comments
                  Min_Seq_Stream_Index := I;
                  Max_Seq_Stream_Index := I;
                  return;
               end if;
            end;

            Tree.Stream_Next (Tree.Shared_Stream, I);
         end loop;

         loop
            Tree.Stream_Next (Tree.Shared_Stream, I);
            declare
               Node : constant Valid_Node_Access := Tree.Get_Node (Tree.Shared_Stream, I);
               Node_Seq_Index : constant Base_Sequential_Index := Tree.Get_Sequential_Index (Node);
            begin
               if Node = Tree.EOI then
                  Max_Seq_Stream_Index := I;
                  exit;

               elsif Node_Seq_Index = Invalid_Sequential_Index then
                  Max_Seq_Stream_Index := Tree.Stream_Prev (Tree.Shared_Stream, I);
                  exit;

               elsif Node_Seq_Index > Shared_Parser.Max_Sequential_Index then
                  Max_Seq_Stream_Index := I;
                  exit;
               end if;
            end;
         end loop;
      end Set_Min_Max_Seq_Stream_Index;

      function Find_Node_Index (Seq_Index : in Sequential_Index) return Node_Index
      is
         I : Stream_Index := Min_Seq_Stream_Index;
      begin
         --  FIXME: use node_index map to do binary search.
         loop
            declare
               Node : constant Valid_Node_Access := Tree.Get_Node (Tree.Shared_Stream, I);
               Node_Seq_Index : constant Base_Sequential_Index := Tree.Get_Sequential_Index (Node);
            begin
               --  Seq_Index is not guarranteed to be in the Shared_Stream; it might
               --  be on a virtual terminal inserted by a previous error recover
               --  session.
               if Node_Seq_Index /= Invalid_Sequential_Index and then
                 Node_Seq_Index > Seq_Index
               then
                  return Tree.Get_Node_Index (Node);
               end if;
            end;
            Tree.Stream_Next (Tree.Shared_Stream, I);
         end loop;
      end Find_Node_Index;

   begin
      --  Clear Derivs that are affected by the recover ops.
      Set_Min_Max_Seq_Stream_Index;

      if Min_Seq_Stream_Index = Max_Seq_Stream_Index then
         --  Empty source text; all Derivs failed. Set all to No_Result.
         for Nonterm in Derivs'Range loop
            for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop
               Derivs (Nonterm).Replace_Element (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
            end loop;
         end loop;

      else
         for Parser_State of Shared_Parser.Parsers loop
            if Trace_Packrat_McKenzie > Detail then
               Trace.Put_Line
                 (Tree.Trimmed_Image (Parser_State.Stream) & ": recover_insert_delete: " & WisiToken.Parse.LR.Image
                    (Parser_State.Recover_Insert_Delete, Tree));
            end if;

            --  First pass to clear left-recursive Derivs that can be
            --  extended; they may be fixed by this edit.
            for Nonterm in Derivs'Range loop
               if Shared_Parser.Direct_Left_Recursive (Nonterm) /= null then
                  for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop
                     declare
                        Memo  : WisiToken.Parse.Packrat.Memo_Entry renames Derivs (Nonterm)(Pos);
                        Clear : Boolean := False;
                     begin
                        case Memo.State is
                        when No_Result | Failure =>
                           null;

                        when Success =>
                           if Shared_Parser.Direct_Left_Recursive (Nonterm)
                             (Tree.ID (Tree.Stream_Next (Tree.Shared_Stream, Memo.Last_Pos)))
                           then
                              --  The recursive nonterm can be extended. IMPROVEME: that means it
                              --  failed because of some error; maybe not this one!
                              Clear := True;
                           end if;
                        end case;

                        if Clear then
                           if Trace_Packrat_McKenzie > Detail then
                              Trace.Put_Line
                                ("clear recursive deriv" & Packrat.Image (Memo, Nonterm, Pos, Tree));
                           end if;

                           Derivs (Nonterm).Replace_Element (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
                        end if;
                     end;
                  end loop;
               end if;
            end loop;

            --  Second pass to clear all Derivs affected by the edits in this
            --  error recover.
            for Op of Parser_State.Recover_Insert_Delete loop
               declare
                  Edit_Pos : constant Node_Index :=
                    (case Op.Op is
                     when Insert => Find_Node_Index (Op.Ins_Before),
                     when Delete =>
                        --  FIXME: if Op.Del_Node was copied to add an error, this is wrong.
                        --  Need test case.
                        Tree.Get_Node_Index (Op.Del_Node));
               begin
                  if Edit_Pos /= Last_Edit_Pos then
                     Last_Edit_Pos := Edit_Pos;

                     if Trace_Packrat_McKenzie > Detail then
                        Trace.Put_Line ("edit_pos:" & Edit_Pos'Image);
                     end if;

                     --  IMPROVEME: use red_black tree sorted on max_examined_pos to
                     --  make this more efficient?


                     for Nonterm in Derivs'Range loop
                        for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop
                           declare
                              Memo  : WisiToken.Parse.Packrat.Memo_Entry renames Derivs (Nonterm)(Pos);
                              Clear : Boolean := False;
                           begin
                              case Memo.State is
                              when No_Result =>
                                 null;

                              when Failure =>
                                 if Edit_Pos <= To_Node_Index (Memo.Max_Examined_Pos) then
                                    Clear := True;
                                 end if;

                              when Success =>
                                 if Edit_Pos in Tree.Get_Node_Index (Tree.First_Terminal (Memo.Result)) ..
                                   To_Node_Index (Memo.Max_Examined_Pos)
                                 then
                                    Clear := True;
                                 end if;
                              end case;

                              if Clear then
                                 if Trace_Packrat_McKenzie > Detail then
                                    Trace.Put_Line ("clear deriv" & Packrat.Image (Memo, Nonterm, Pos, Tree));
                                 end if;

                                 Derivs (Nonterm).Replace_Element (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
                              end if;
                           end;
                        end loop;
                     end loop;
                  end if;
               end;
            end loop;
         end loop;
         WisiToken.Parse.LR.McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
      end if;

      --  Enter each Stream element into Derivs. We process the stack top
      --  first, and pop the stack to get the next element, to allow doing
      --  Undo_Reduce for recursive productions.
      --
      --  First check Stream input for terminals with errors; copy them back
      --  to Shared_Stream. There should be no other tokens in the stream input.
      --  test_mckenzie_recover.adb Empty_Comment.
      if Tree.Has_Input (Stream) then
         declare
            I : Stream_Index := Tree.Stream_Last (Stream, Skip_EOI => False);
         begin
            loop
               if Tree.Has_Error (Tree.Get_Node (Stream, I)) then
                  declare
                     Node : constant Valid_Node_Access := Tree.Get_Node (Stream, I);
                     pragma Assert (Tree.Label (Node) = Source_Terminal);
                     Shared_Stream_Index : constant Stream_Index      := Find_Shared_Stream_Index (Node);
                     Shared_Node         : constant Valid_Node_Access := Tree.Get_Node
                       (Tree.Shared_Stream, Shared_Stream_Index);
                  begin
                     Tree.Add_Errors (Tree.Shared_Stream, Shared_Node, Tree.Error_List (Node));
                  end;
               else
                  raise SAL.Programmer_Error with "non-error in stream input";
               end if;
               I := Tree.Stream_Prev (Stream, I);
               exit when I = Tree.Stack_Top (Stream);
            end loop;
         end;
      end if;

      loop
         loop
            case Tree.Label (Tree.Peek (Stream)) is
            when Source_Terminal =>
               --  Packrat_Parse does not expect terminals in Derivs; all terminals
               --  must be in Shared_Stream. Copy any errors back to Shared_Stream
               if Tree.Has_Error (Tree.Get_Node (Stream, Tree.Peek (Stream))) then
                  declare
                     Node : constant Valid_Node_Access := Tree.Get_Node (Stream, Tree.Peek (Stream));
                     Shared_Stream_Index : constant Stream_Index      := Find_Shared_Stream_Index (Node);
                     Shared_Node         : constant Valid_Node_Access := Tree.Get_Node
                       (Tree.Shared_Stream, Shared_Stream_Index);
                  begin
                     Tree.Add_Errors (Tree.Shared_Stream, Shared_Node, Tree.Error_List (Node));
                  end;
               end if;

            when Virtual_Terminal =>
               --  This is a bug in error recover; all inserted virtual terminals
               --  should be consumed by a nonterm, not left in the parse stream.
               if Debug_Mode then
                  raise SAL.Programmer_Error with "virtual_terminal in parse stream";
               else
                  raise WisiToken.Parse_Error  with "virtual_terminal in parse stream";
               end if;

            when Virtual_Identifier =>
               raise SAL.Programmer_Error;

            when Nonterm =>
               if Shared_Parser.Direct_Left_Recursive (Tree.ID (Tree.Peek (Stream))) /= null then
                  declare
                     Next_Terminal : constant Valid_Node_Access := Tree.First_Terminal
                       (Tree.To_Rooted_Ref (Stream, Tree.Stream_Next (Stream, Tree.Peek (Stream)))).Node;
                  begin
                     if Shared_Parser.Direct_Left_Recursive (Tree.ID (Tree.Peek (Stream))) (Tree.ID (Next_Terminal))
                     then
                        --  The next token extends the recursion in Nonterm. The packrat
                        --  parser will not extend this recursive production (because it first
                        --  just checks if Nonterm succeeds), so add the contents instead.
                        --  test_mckenzie_recover.adb Error_2.
                        LR.Undo_Reduce (Tree, Shared_Parser.Table.all, Stream, Shared_Parser.User_Data);
                     end if;
                  end;
               end if;

               declare
                  Result_Node : constant Valid_Node_Access := Tree.Get_Node (Stream, Tree.Peek (Stream));
                  First_Term  : Stream_Node_Parents        := Tree.To_Stream_Node_Parents
                    ((Stream, Tree.Peek (Stream), Result_Node));
                  Last_Term   : Stream_Node_Parents        := First_Term;
               begin
                  Tree.First_Source_Terminal (First_Term, Trailing_Non_Grammar => False);
                  Tree.Last_Source_Terminal (Last_Term, Trailing_Non_Grammar => False, Parse_Stream => Stream);

                  declare
                     Last_Pos : constant Stream_Index := Find_Shared_Stream_Index (Last_Term.Ref.Node);
                     Pos      : constant Node_Index   := To_Node_Index
                       (Find_Shared_Stream_Index (First_Term.Ref.Node));

                     Memo     : constant Packrat.Memo_Entry :=
                       (WisiToken.Parse.Packrat.Success,
                        Max_Examined_Pos => Last_Pos,
                        Result           => Result_Node,
                        Last_Pos         => Last_Pos);
                  begin
                     if Trace_Packrat_McKenzie > Detail then
                        Trace.Put_Line ("set deriv" & Packrat.Image (Memo, Tree.ID (Result_Node), Pos, Tree));
                     end if;

                     WisiToken.Parse.Packrat.Set_Deriv
                       (Derivs,
                        Nonterm => Tree.ID (Result_Node),
                        Pos     => Pos,
                        Memo    => Memo);
                  end;
               end;
            end case;

            Tree.Push_Back (Stream);
            exit when Tree.ID (Tree.Peek (Stream)) = Descriptor.SOI_ID;
         end loop;

         Tree.Next_Parse_Stream (Stream);
         exit when Stream = Invalid_Stream_ID;
      end loop;

      Tree.Clear_Parse_Streams;
   end Parse_Streams_To_Derivs;

begin
   loop -- One error recover session per loop
      begin
         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("packrat parse" & (if Recovered then " resume:" else ":"));
         end if;
         WisiToken.Parse.Parser.Parser'Class (Shared_Parser).Packrat_Parse_No_Recover (Resume => Recovered);
         Recovered := False;
      exception
      when WisiToken.Parse_Error =>
         if Trace_Packrat_McKenzie > Outline then
            Trace.New_Line;
            Trace.Put_Line ("pre recover derivs:");
            Shared_Parser.Print_Derivs;
            Trace.New_Line;
         end if;

         Shared_Parser.Parsers := WisiToken.Parse.LR.Parser_Lists.New_List (Tree);

         Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

         declare
            Max_Examined_Stream_Index : Stream_Index        := Invalid_Stream_Index;
            Last_Max_Examined_Pos     : constant Node_Index := Max_Examined_Pos;
         begin
            if Trace_Packrat_McKenzie > Outline then
               Trace.Put_Line ("derivs to parse stream:");
            end if;
            Derivs_To_Parse_Streams (Max_Examined_Pos, Max_Examined_Stream_Index);

            if Max_Examined_Pos /= Invalid_Node_Index and then Max_Examined_Pos <= Last_Max_Examined_Pos then
               raise WisiToken.Parse_Error with "error recover failed to make progress";
            end if;

            --  Ensure LR_Core_Parse immediately enters recover.
            Shared_Parser.Resume_Active := True;
            for Parser_State of Shared_Parser.Parsers loop
               Parser_State.Set_Verb (WisiToken.Parse.LR.Error);
               --  FIXME: parsers may not all have the same error token.
               if Max_Examined_Stream_Index /= Invalid_Stream_Index then
                  --  Invalid when source text is empty or only comments. Just fail.
                  Tree.Set_Shared_Link (Parser_State.Stream, Max_Examined_Stream_Index);
               end if;
               Tree.Add_Error_To_Input
                 (Parser_State.Stream,
                  WisiToken.Parse.Parse_Error' --  FIXME: build expecting?
                    (First_Terminal => 1,
                     Last_Terminal  => 0,
                     Expecting      => (1 .. 0 => False),
                     Recover_Ops    => WisiToken.Parse.Recover_Op_Arrays.Empty_Vector,
                     Recover_Cost   => 0),
                  Shared_Parser.User_Data);
            end loop;
         end;

         if Trace_Packrat_McKenzie > Outline then
            Trace.New_Line;
            Trace.Put_Line ("LR parse recover:");
         end if;

         Shared_Parser.LR_Core_Parse (Log_File, Recover_Only => True);
         --  Raises Parse_Error if recover fails.
         Recovered := True;

         if Trace_Packrat_McKenzie > Outline then
            Trace.New_Line;
            Trace.Put_Line ("post recover streams:");
            Tree.Print_Streams (Children => Trace_Packrat_McKenzie > Detail);
            Trace.Put_Line ("parse streams to derivs:");
         end if;

         Parse_Streams_To_Derivs;

         if Trace_Packrat_McKenzie > Outline then
            Trace.New_Line;
            Trace.Put_Line ("post recover derivs:");
            Shared_Parser.Print_Derivs;
            Trace.New_Line;
         end if;
      end;

      exit when not Recovered;
   end loop;
end Packrat_Parse;
