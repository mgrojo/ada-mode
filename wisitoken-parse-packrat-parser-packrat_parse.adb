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
with WisiToken.Parse.LR.Parser_Lists;
separate (WisiToken.Parse.Packrat.Parser)
procedure Packrat_Parse
  (Shared_Parser : in out Parser;
   Log_File      : in     Ada.Text_IO.File_Type)
is
   use WisiToken.Syntax_Trees;
   Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
   Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

   Recovered : Boolean := False;

   function To_Node_Index (Pos : in Stream_Index) return Node_Index
   is begin
      return Tree.Get_Node_Index (Tree.Shared_Stream, Pos);
   end To_Node_Index;

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

   procedure Parse_Streams_To_Derivs
   is
      Last_Edit_Pos        : Node_Index := 0;
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
                 Node_Seq_Index >= Seq_Index
               then
                  return Tree.Get_Node_Index (Node);
               end if;
            end;
            Tree.Stream_Next (Tree.Shared_Stream, I);
         end loop;
      end Find_Node_Index;

   begin
      Set_Min_Max_Seq_Stream_Index;

      --  Delete packrat parsers whose corresponding LR stream was terminated.
      declare
         use Packrat.Parser.Parser_State_Lists;
         Cur : Cursor := Shared_Parser.Packrat_Parsers.First;
         To_Delete : Cursor;
      begin
         loop
            if (for some LR_Parser_State of Shared_Parser.Parsers =>
                  Syntax_Trees.Label (LR_Parser_State.Stream) = Shared_Parser.Packrat_Parsers (Cur).LR_Stream_Label)
            then
               Next (Cur);
            else
               To_Delete := Cur;
               Next (Cur);
               Shared_Parser.Packrat_Parsers.Delete (To_Delete);
            end if;
            exit when Cur = No_Element;
         end loop;
      end;

      --  Clear Derivs that are affected by the recover ops.
      if Min_Seq_Stream_Index = Max_Seq_Stream_Index then
         --  Empty source text; all Derivs failed. Set all to No_Result.
         for Parser_State of Shared_Parser.Packrat_Parsers loop
            for Nonterm in Parser_State.Derivs'Range loop
               for Pos in Parser_State.Derivs (Nonterm).First_Index .. Parser_State.Derivs (Nonterm).Last_Index loop
                  Parser_State.Derivs (Nonterm).Replace_Element (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
               end loop;
            end loop;
         end loop;

      else
         for Packrat_Parser_State of Shared_Parser.Packrat_Parsers loop
            declare
               LR_Parser_State : LR.Parser_Lists.Parser_State renames Shared_Parser.Parsers
                 (Shared_Parser.LR_Parser (Packrat_Parser_State.LR_Stream_Label));

               Packrat_Label : constant String := "Packrat" & Packrat_Parser_State.Packrat_Label'Image;
            begin
               --  First pass to clear left-recursive Derivs that can be
               --  extended; they may be fixed by this edit.
               for Nonterm in Packrat_Parser_State.Derivs'Range loop
                  if Shared_Parser.Direct_Left_Recursive (Nonterm) /= null then
                     for Pos in Packrat_Parser_State.Derivs (Nonterm).First_Index ..
                       Packrat_Parser_State.Derivs (Nonterm).Last_Index
                     loop
                        declare
                           Memo  : WisiToken.Parse.Packrat.Memo_Entry renames Packrat_Parser_State.Derivs
                             (Nonterm)(Pos);
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
                                 --  failed because of _some_ error; maybe not _this_ error!
                                 Clear := True;
                              end if;
                           end case;

                           if Clear then
                              if Trace_Packrat_McKenzie > Detail then
                                 Trace.Put_Line
                                   (Packrat_Label & " clear recursive deriv" &
                                      Packrat.Image (Memo, Nonterm, Pos, Tree));
                              end if;

                              Packrat_Parser_State.Derivs (Nonterm).Replace_Element
                                (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
                           end if;
                        end;
                     end loop;
                  end if;
               end loop;

               --  Second pass to clear all Derivs affected by the edits in this
               --  error recover. Also enter LR_Parser_State.Recover_Insert_Delete into
               --  Parser.Insert_Delete.
               for Error_Node of LR_Parser_State.Recover_Insert_Delete loop
                  for Error of Tree.Error_List (Error_Node) loop
                     for Op of Recover_Op_Array_Const_Ref (Error) loop
                        declare
                           Edit_Pos : constant Node_Index :=
                             (case Op.Op is
                              when Insert => Find_Node_Index (Op.Ins_Before),
                              when Delete => Tree.Get_Node_Index (Op.Del_Node));
                        begin
                           declare
                              Packrat_Op : Packrat.Parser.Recover_Op_Nodes :=
                                (Op        => Op.Op,
                                 ID        => (case Op.Op is when Insert => Op.Ins_ID, when Delete => Op.Del_ID),
                                 Node      => (case Op.Op is when Insert => Op.Ins_Node, when Delete => Op.Del_Node),
                                 Pos       => Edit_Pos);

                              Cur : constant Recover_Op_Nodes_Trees.Cursor :=
                                Packrat_Parser_State.Insert_Delete.Find (Edit_Pos);
                           begin
                              if Op.Op = Insert and then Packrat_Op.Node = Invalid_Node_Access then
                                 Packrat_Op.Node := Tree.New_Virtual_Terminal (Packrat_Op.ID);
                              end if;

                              if Recover_Op_Nodes_Trees.Has_Element (Cur) then
                                 declare
                                    Op_List : Recover_Op_Nodes_Lists.List renames
                                      Packrat_Parser_State.Insert_Delete.Variable_Ref (Cur);
                                 begin
                                    case Op.Op is
                                    when Insert      =>
                                       Op_List.Append (Packrat_Op);
                                    when Delete =>
                                       Op_List.Prepend (Packrat_Op);
                                    end case;
                                 end;
                              else
                                 Packrat_Parser_State.Insert_Delete.Insert
                                   (Recover_Op_Nodes_Lists.To_List (Packrat_Op));
                              end if;
                           end;

                           if Edit_Pos /= Last_Edit_Pos then
                              Last_Edit_Pos := Edit_Pos;

                              if Trace_Packrat_McKenzie > Detail then
                                 Trace.Put_Line (Packrat_Label & " edit_pos:" & Edit_Pos'Image);
                              end if;

                              --  IMPROVEME: change Derivs to red_black tree sorted on
                              --  max_examined_pos to make this more efficient?

                              for Nonterm in Packrat_Parser_State.Derivs'Range loop
                                 for Pos in Packrat_Parser_State.Derivs (Nonterm).First_Index ..
                                   Packrat_Parser_State.Derivs (Nonterm).Last_Index
                                 loop
                                    declare
                                       Memo  : WisiToken.Parse.Packrat.Memo_Entry renames
                                         Packrat_Parser_State.Derivs (Nonterm)(Pos);
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
                                             Trace.Put_Line
                                               (Packrat_Label & " clear deriv" & Packrat.Image
                                                  (Memo, Nonterm, Pos, Tree));
                                          end if;

                                          Packrat_Parser_State.Derivs (Nonterm).Replace_Element
                                            (Pos, WisiToken.Parse.Packrat.No_Result_Memo);
                                       end if;
                                    end;
                                 end loop;
                              end loop;
                           end if;
                        end;
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
         WisiToken.Parse.LR.McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
      end if;

      --  Set Derivs to match LR parse streams
      for LR_Parser_State of Shared_Parser.Parsers loop
         declare
            Packrat_Parser_State : Packrat.Parser.Parser_State renames Shared_Parser.Packrat_Parsers
              (Shared_Parser.Packrat_Parser (LR_Parser_State.Stream));

            Packrat_Label : constant String := "Packrat" & Packrat_Parser_State.Packrat_Label'Image;
         begin
            Packrat_Parser_State.Last_Recover_Enqueue_Count := LR_Parser_State.Recover.Enqueue_Count;
            Packrat_Parser_State.Last_Recover_Check_Count   := LR_Parser_State.Recover.Check_Count;
            Packrat_Parser_State.Total_Recover_Cost         := LR_Parser_State.Total_Recover_Cost;
            Packrat_Parser_State.Max_Recover_Ops_Length     := LR_Parser_State.Max_Recover_Ops_Length;

            --  Enter each LR_Parser_State.Stream element into corresponding
            --  Packrat_Parser_State.Derivs. We process the stack top first, and
            --  pop the stack to get the next element, to allow doing Undo_Reduce
            --  for recursive productions.
            --
            --  First check LR_Parser_State.Stream input for terminals with
            --  errors; copy them back to Shared_Stream. There should be no other
            --  tokens in the stream input. test_mckenzie_recover.adb
            --  Empty_Comment.
            if Tree.Has_Input (LR_Parser_State.Stream) then
               declare
                  I : Stream_Index := Tree.Stream_Last (LR_Parser_State.Stream, Skip_EOI => False);
               begin
                  loop
                     if Tree.Has_Error (Tree.Get_Node (LR_Parser_State.Stream, I)) then
                        declare
                           Node : constant Valid_Node_Access := Tree.Get_Node (LR_Parser_State.Stream, I);
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
                     I := Tree.Stream_Prev (LR_Parser_State.Stream, I);
                     exit when I = Tree.Stack_Top (LR_Parser_State.Stream);
                  end loop;
               end;
            end if;

            loop --  over LR_Parser_State.Stream.Stack
               case Tree.Label (Tree.Peek (LR_Parser_State.Stream)) is
               when Source_Terminal =>
                  --  Packrat_Parse does not expect terminals in Derivs; all terminals
                  --  must be in Shared_Stream. Copy any errors back to Shared_Stream
                  if Tree.Has_Error (Tree.Get_Node (LR_Parser_State.Stream, Tree.Peek (LR_Parser_State.Stream))) then
                     declare
                        Node : constant Valid_Node_Access := Tree.Get_Node
                          (LR_Parser_State.Stream, Tree.Peek (LR_Parser_State.Stream));
                        Shared_Stream_Index : constant Stream_Index      := Find_Shared_Stream_Index (Node);
                        Shared_Node         : constant Valid_Node_Access := Tree.Get_Node
                          (Tree.Shared_Stream, Shared_Stream_Index);
                     begin
                        Tree.Add_Errors (Tree.Shared_Stream, Shared_Node, Tree.Error_List (Node));
                     end;
                  end if;

               when Virtual_Terminal =>
                  --  Some production containing a virtual terminal inserted by error
                  --  recover has not been reduced when resume is done. We can ignore
                  --  this here; it is handled by Parser.Insert_Delete.
                  --  test_mckenzie_recover.adb Error_3
                  null;

               when Virtual_Identifier =>
                  raise SAL.Programmer_Error;

               when Nonterm =>
                  if Shared_Parser.Direct_Left_Recursive (Tree.ID (Tree.Peek (LR_Parser_State.Stream))) /= null then
                     declare
                        Next_Terminal : constant Valid_Node_Access := Tree.First_Terminal
                          (Tree.To_Rooted_Ref
                             (LR_Parser_State.Stream,
                              Tree.Stream_Next (LR_Parser_State.Stream, Tree.Peek (LR_Parser_State.Stream)))).Node;
                     begin
                        if Shared_Parser.Direct_Left_Recursive (Tree.ID (Tree.Peek (LR_Parser_State.Stream)))
                          (Tree.ID (Next_Terminal))
                        then
                           --  The next token extends the recursion in Nonterm. The packrat
                           --  parser will not extend this recursive production (because it first
                           --  just checks if Nonterm succeeds), so add the contents instead.
                           --  test_mckenzie_recover.adb Error_2.
                           LR.Undo_Reduce
                             (Tree, Shared_Parser.Table.all, LR_Parser_State.Stream,
                              Syntax_Trees.User_Data_Access_Constant (Shared_Parser.User_Data));
                        end if;
                     end;
                  end if;

                  declare
                     Result_Node : constant Valid_Node_Access := Tree.Get_Node
                       (LR_Parser_State.Stream, Tree.Peek (LR_Parser_State.Stream));
                     First_Term  : Stream_Node_Parents        := Tree.To_Stream_Node_Parents
                       ((LR_Parser_State.Stream, Tree.Peek (LR_Parser_State.Stream), Result_Node));
                     Last_Term   : Stream_Node_Parents        := First_Term;
                  begin
                     Tree.First_Source_Terminal (First_Term, Trailing_Non_Grammar => False);
                     Tree.Last_Source_Terminal
                       (Last_Term, Trailing_Non_Grammar => False, Parse_Stream => LR_Parser_State.Stream);

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
                           Trace.Put_Line
                             (Packrat_Label & " set deriv" & Packrat.Image (Memo, Tree.ID (Result_Node), Pos, Tree));
                        end if;

                        WisiToken.Parse.Packrat.Set_Deriv
                          (Packrat_Parser_State.Derivs,
                           Nonterm => Tree.ID (Result_Node),
                           Pos     => Pos,
                           Memo    => Memo);
                     end;
                  end;
               end case;

               Tree.Push_Back (LR_Parser_State.Stream);
               exit when Tree.ID (Tree.Peek (LR_Parser_State.Stream)) = Descriptor.SOI_ID;
            end loop;
         end;
      end loop;

      Shared_Parser.Parsers.Clear;
      Tree.Clear_Parse_Streams;

   end Parse_Streams_To_Derivs;

   Last_Max_Examined_Pos : Syntax_Trees.Node_Index := -1;
begin
   Shared_Parser.String_Quote_Checked := Invalid_Line_Number;
   Shared_Parser.Parsers.Clear;

   loop -- One error recover session per loop
      begin
         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("packrat parse" & (if Recovered then " resume:" else ":"));
         end if;
         WisiToken.Parse.Packrat.Parser.Parser'Class (Shared_Parser).Packrat_Parse_No_Recover (Resume => Recovered);
         Recovered := False;
      exception
      when WisiToken.Parse_Error =>
         Shared_Parser.Error := True;

         if Trace_Packrat_McKenzie > Detail then
            Trace.New_Line;
            Trace.Put_Line ("pre recover derivs:");
            Shared_Parser.Print_Derivs;
         end if;

         if Trace_Packrat_McKenzie > Detail then
            Trace.Put_Line ("derivs to parse stream:");
         end if;

         for Packrat_Parser_State of Shared_Parser.Packrat_Parsers loop
            New_LR_Parse_Stream (Shared_Parser, Packrat_Parser_State);
            declare
               LR_Parser_State : WisiToken.Parse.LR.Parser_Lists.Parser_State renames
                 Shared_Parser.Parsers.First.State_Ref;
            begin
               Derivs_To_Parse_Streams (Shared_Parser, Packrat_Parser_State, LR_Parser_State);
            end;
         end loop;

         if Shared_Parser.Max_Examined_Pos <= Last_Max_Examined_Pos then
            raise WisiToken.Parse_Error with "error recover failed to make progress";
         end if;

         Last_Max_Examined_Pos := Shared_Parser.Max_Examined_Pos;

         if Trace_Packrat_McKenzie > Outline then
            if Trace_Packrat_McKenzie > Detail then
               Trace.New_Line;
               Trace.Put_Line ("pre recover streams");
               Tree.Print_Streams;
            end if;
            Trace.New_Line;
            Trace.Put_Line ("LR parse recover:");
         end if;

         Shared_Parser.LR_Core_Parse (Log_File, Recover_Only => True);
         --  Raises Parse_Error if recover fails.
         Recovered := True;

         if Trace_Packrat_McKenzie > Detail then
            Trace.New_Line;
            Trace.Put_Line ("post recover streams:");
            Tree.Print_Streams (Children => Trace_Packrat_McKenzie > Detail);
            Trace.New_Line;
            Trace.Put_Line ("parse streams to derivs:");
         end if;

         Parse_Streams_To_Derivs;

         if Trace_Packrat_McKenzie > Detail then
            Trace.New_Line;
            Trace.Put_Line ("post recover derivs:");
            Shared_Parser.Print_Derivs;
         end if;
      end;

      exit when not Recovered;
   end loop;
end Packrat_Parse;
