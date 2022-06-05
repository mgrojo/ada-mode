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
with WisiToken.Parse.LR.Parser;
separate (WisiToken.Parse.Parser)
procedure Packrat_Parse
  (Shared_Parser : in out Parser;
   Log_File      : in     Ada.Text_IO.File_Type)
is
   use WisiToken.Syntax_Trees;
   Derivs : WisiToken.Parse.Packrat.Derivs renames Shared_Parser.Derivs;
   Tree   : Syntax_Trees.Tree renames Shared_Parser.Tree;
   Trace  : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

   Recovered : Boolean := False;

   function To_Stream_Index (Pos : in Node_Index) return Stream_Index
   is
      --  FIXME: build a map for this once.
      I    : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
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
   is
      --  FIXME: use node_index map to do binary search.
      I : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
      Ref_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node, Trailing_Non_Grammar => False);
   begin
      loop
         exit when Tree.Byte_Region (Tree.Get_Node (Tree.Shared_Stream, I), Trailing_Non_Grammar => False) =
           Ref_Byte_Region;
         I := Tree.Stream_Next (Tree.Shared_Stream, I);
      end loop;
      return I;
   end Find_Shared_Stream_Index;

   procedure Derivs_To_Parse_Streams (Max_Examined_Stream_Index : in out Stream_Index)
   --  Build Tree parse streams from Derivs
   is
      Pos              : Node_Index := 1;
      Max_Examined_Pos : Node_Index := 0;

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
            Trace.Put_Line ("empty source text; packrat to LR done");
         end if;
         return;
      end if;

      loop
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

                  Ada.Text_IO.Put_Line
                    ("state " & Prev_State'Image & " push " & Tree.Image (Nonterm_Node, Node_Numbers => True));

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
                          ("state " & Prev_State'Image & " push " & Tree.Image (Terminal_Node, Node_Numbers => True));

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
                                         ("state " & Prev_State'Image & " push " &
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
                        --  This is the error token; we're not supposed to get here.
                        raise SAL.Programmer_Error;

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
      EOI_Node_Index : constant Node_Index := Tree.Get_Node_Index (Tree.EOI);

      Stream : Stream_ID := Tree.First_Parse_Stream;
   begin
      loop
         declare
            El : Stream_Index := Tree.Stream_First (Stream, Skip_SOI => True);
         begin
            loop
               case Tree.Label (Tree.Get_Node (Stream, El)) is
               when Source_Terminal =>
                  --  Packrat_Parse does not expect terminals in Derivs; all terminals
                  --  must be in Shared_Stream. Copy any errors back to Shared_Stream
                  if Tree.Has_Error (Tree.Get_Node (Stream, El)) then
                     declare
                        Node                : constant Valid_Node_Access := Tree.Get_Node (Stream, El);
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
                  declare
                     Result_Node    : constant Valid_Node_Access := Tree.Get_Node (Stream, El);
                     First_Term_Ref : Stream_Node_Ref            := Tree.First_Source_Terminal
                       (Stream, El, Trailing_Non_Grammar => False);

                     Last_Node : constant Node_Access := Tree.Last_Source_Terminal
                       (Result_Node, Trailing_Non_Grammar => False);

                     Last_Pos : constant Stream_Index :=
                       (if Last_Node = Invalid_Node_Access then
                          (if First_Term_Ref.Stream /= Tree.Shared_Stream
                           --  First_Term_Ref was copied to add an error.
                           then Find_Shared_Stream_Index (First_Term_Ref.Node)
                           else First_Term_Ref.Element)
                        else
                          (if Tree.Get_Node_Index (Last_Node) > EOI_Node_Index
                           --  Last_Node was copied to add an error.
                           then Find_Shared_Stream_Index (Last_Node)
                           else To_Stream_Index (Tree.Get_Node_Index (Last_Node))));
                  begin
                     pragma Assert (Tree.Contains (Tree.Shared_Stream, Last_Pos));

                     WisiToken.Parse.Packrat.Set_Deriv
                       (Derivs,
                        Nonterm             => Tree.ID (Stream, El),
                        Pos                 => Tree.Get_Node_Index
                          (if First_Term_Ref.Stream /= Tree.Shared_Stream
                           --  First_Term_Ref was copied to add an error.
                           then Tree.Get_Node (Tree.Shared_Stream, Find_Shared_Stream_Index (First_Term_Ref.Node))
                           else First_Term_Ref.Node),
                        Memo                =>
                          (WisiToken.Parse.Packrat.Success,
                           Max_Examined_Pos => Last_Pos,
                           Result           => Result_Node,
                           Last_Pos         => Last_Pos));
                  end;

               end case;

               Tree.Stream_Next (Stream, El);
               exit when El = Invalid_Stream_Index;
            end loop;
         end;

         Tree.Next_Parse_Stream (Stream);
         exit when Stream = Invalid_Stream_ID;
      end loop;

      Tree.Clear_Parse_Streams;
   end Parse_Streams_To_Derivs;

begin
   loop -- One error recover session per loop
      begin
         WisiToken.Parse.Parser.Parser'Class (Shared_Parser).Packrat_Parse_No_Recover (Resume => Recovered);
         Recovered := False;
      exception
      when WisiToken.Parse_Error =>
         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("pre recover derivs:");
            Shared_Parser.Print_Derivs;
         end if;

         Shared_Parser.Parsers := WisiToken.Parse.LR.Parser_Lists.New_List (Tree);

         Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

         declare
            Max_Examined_Stream_Index : Stream_Index := Invalid_Stream_Index;
         begin
            Derivs_To_Parse_Streams (Max_Examined_Stream_Index);

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

         Shared_Parser.LR_Core_Parse (Log_File, Recover_Only => True);
         --  Raises Parse_Error if recover fails.
         Recovered := True;

         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("post recover parse streams:");
            Tree.Print_Streams;
         end if;

         Parse_Streams_To_Derivs;

         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line ("post recover derivs:");
            Shared_Parser.Print_Derivs;
         end if;
      end;

      exit when not Recovered;
   end loop;
end Packrat_Parse;
