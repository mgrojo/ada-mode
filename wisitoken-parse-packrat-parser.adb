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

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Parse.Packrat.Parser is

   function Image (Item : in Recover_Op_Nodes; Tree : in Syntax_Trees.Tree) return String
   is
      use WisiToken.Syntax_Trees;
   begin
      return
        "(" & Image (Item.Op) & ", " &
        Image (Item.ID, Tree.Lexer.Descriptor.all) & "," &
        Item.Pos'Image & "," &
        Item.Error_Pos'Image & ")";
   end Image;

   overriding procedure Finalize (Object : in out Parser)
   is begin
      --  Derivs holds Stream_Index references to Tree, so Derivs must be
      --  cleared before Tree is finalized.
      for Parser_State of Object.Packrat_Parsers loop
         WisiToken.Parse.Packrat.Clear (Parser_State.Derivs);
         Parser_State.Result := No_Result_Memo;
      end loop;

      WisiToken.Parse.Parser.Finalize (WisiToken.Parse.Parser.Parser (Object));
   end Finalize;

   function Packrat_Parser
     (Parser       : in out Packrat.Parser.Parser;
      LR_Stream_ID : in     Syntax_Trees.Stream_ID)
     return Parser_State_Lists.Cursor
   is
      use Parser_State_Lists;

      Descriptor : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
      Result     : Cursor;
   begin
      for Cur in Parser.Packrat_Parsers.Iterate loop
         if Parser.Packrat_Parsers (Cur).LR_Stream_Label = Syntax_Trees.Label (LR_Stream_ID) then
            Result := Cur;
         end if;
      end loop;

      if Result = No_Element then
         Result := Parser.Packrat_Parsers.Append
           ((First_Nonterminal => Descriptor.First_Nonterminal,
             Last_Nonterminal  => Descriptor.Last_Nonterminal,
             LR_Stream_Label   => Syntax_Trees.Label (LR_Stream_ID),
             Packrat_Label     => Parser.Next_Packrat_Label,
             Derivs            => (Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal => <>),
             others            => <>));

         Parser.Next_Packrat_Label := @ + 1;
      end if;

      return Result;
   end Packrat_Parser;

   function LR_Parser
     (Parser          : in out Packrat.Parser.Parser;
      LR_Stream_Label : in     Syntax_Trees.Stream_Label)
     return LR.Parser_Lists.Parser_Node_Access
   is
      use LR.Parser_Lists;
   begin
      for Cur in Parser.Parsers.Iterate loop
         if Syntax_Trees.Label (Parser.Parsers (Cur).Stream) = LR_Stream_Label then
            return Cur;
         end if;
      end loop;

      return No_Element;
   end LR_Parser;

   function Delete_Valid
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index)
     return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser_State.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser_State.Insert_Delete.Constant_Ref (Found) => Op.Op = Delete);
   end Delete_Valid;

   function Has_Input
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index)
     return Boolean
   is
      use Recover_Op_Nodes_Trees;
      Found : constant Cursor := Parser_State.Insert_Delete.Find
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));
   begin
      return Found /= No_Element and then
        (for some Op of Parser_State.Insert_Delete.Constant_Ref (Found) => Op.Op = Insert);
   end Has_Input;

   function Input_Op
     (Parser       : in Packrat.Parser.Parser;
      Parser_State : in Packrat.Parser.Parser_State;
      Pos          : in Syntax_Trees.Stream_Index;
      Prev_Node    : in Syntax_Trees.Node_Access)
     return ID_Node_Type
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      use Recover_Op_Nodes_Lists;

      List : Recover_Op_Nodes_Lists.List renames Parser_State.Insert_Delete.Constant_Ref
        (Parser.Tree.Get_Node_Index (Parser.Tree.Shared_Stream, Pos));

      Cur      : Cursor := List.Last;
      Prev_Cur : Cursor := Previous (Cur);
   begin
      return Result : ID_Node_Type do
         if List (Cur).Node = Prev_Node then
            return;

         else
            loop
               if Prev_Cur = No_Element or else
                 (List (Prev_Cur).Op = Delete or else
                    List (Prev_Cur).Node = Prev_Node)
               then
                  declare
                     Op : Recover_Op_Nodes renames Constant_Ref (Cur);
                  begin
                     Result := (ID => Op.ID, Node => Op.Node);
                  end;
                  exit;

               end if;
               Cur      := Prev_Cur;
               Prev_Cur := Previous (Cur);
            end loop;
         end if;
      end return;
   end Input_Op;

   function Max_Examined_Pos (Shared_Parser : in out Parser) return Syntax_Trees.Node_Index
   is
      use Syntax_Trees;
   begin
      return Result : Node_Index := 0 -- SOI
      do
         for Parser_State of Shared_Parser.Packrat_Parsers loop
            if Parser_State.Max_Examined_Pos = Invalid_Node_Index then
               null;
            elsif Parser_State.Max_Examined_Pos > Result then
               Result := Parser_State.Max_Examined_Pos;
            end if;
         end loop;
      end return;
   end Max_Examined_Pos;

   procedure New_LR_Parse_Stream
     (Shared_Parser        : in out Packrat.Parser.Parser'Class;
      Packrat_Parser_State : in out Packrat.Parser.Parser_State)
   is
      use all type SAL.Base_Peek_Type;
   begin
      if Shared_Parser.Parsers.Count = 0 then
         Shared_Parser.Parsers := WisiToken.Parse.LR.Parser_Lists.New_List (Shared_Parser.Tree);
      else
         Shared_Parser.Parsers.Prepend_Empty (Shared_Parser.Tree);
      end if;

      declare
         use all type WisiToken.Parse.LR.Parse_Table_Ptr;
         Stream : constant Syntax_Trees.Stream_ID := Shared_Parser.Parsers.First.State_Ref.Stream;
      begin
         Packrat_Parser_State.LR_Stream_Label := Syntax_Trees.Label (Stream);
         Shared_Parser.Tree.Start_Parse
           (Stream,
            State =>
              (if Shared_Parser.Table = null
               then State_Index'First
               else Shared_Parser.Table.State_First));
      end;
   end New_LR_Parse_Stream;

   procedure Derivs_To_Parse_Streams
     (Shared_Parser        : in out Packrat.Parser.Parser'Class;
      Packrat_Parser_State : in out Packrat.Parser.Parser_State;
      LR_Parser_State      : in out WisiToken.Parse.LR.Parser_Lists.Parser_State)
   --  Build Tree parse streams from Parser_State, initialize LR_Parser_State.
   is
      use WisiToken.Syntax_Trees;

      Tree       : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Trace      : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Stream     : Syntax_Trees.Stream_ID renames LR_Parser_State.Stream;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
      Derivs     : Packrat.Derivs renames Packrat_Parser_State.Derivs;

      Packrat_Label : constant String := "Packrat" & Packrat_Parser_State.Packrat_Label'Image;

      Pos : Node_Index := 1;

      Max_Examined_Stream_Index : Syntax_Trees.Stream_Index := Syntax_Trees.Invalid_Stream_Index;

      Set_Error : Boolean := True;

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

      procedure Find_Nodes
        (Pos                 : in     Node_Index;
         Terminal_Node       : in out Node_Access;
         Empty_Nonterm_Nodes : in out Valid_Node_Access_Lists.List;
         Nonterm_Nodes       : in out Valid_Node_Access_Lists.List)
      is
         Found_Nonterm_Node_Index : Node_Index := 0;
         Found_Nonterm            : Token_ID   := Invalid_Token_ID;
      begin
         --  First handle an empty or all comments source text; error
         --  correction builds a tree that is at pos 0 or 1.
         --  test_mckenzie_recover.adb Empty_Comments.
         for Pos in Derivs (Descriptor.Accept_ID).First_Index .. Derivs (Descriptor.Accept_ID).Last_Index loop
            if Derivs (Descriptor.Accept_ID)(Pos).State = Parse.Packrat.Success then
               Nonterm_Nodes.Append (Derivs (Descriptor.Accept_ID)(Pos).Result);
               return;
            end if;
         end loop;

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
      Packrat_Parser_State.Max_Examined_Pos := 0;
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
                  if Packrat_Parser_State.Max_Examined_Pos < Max_Pos then
                     Packrat_Parser_State.Max_Examined_Pos  := Max_Pos;
                     Max_Examined_Stream_Index := Derivs (Nonterm)(Pos).Max_Examined_Pos;
                  end if;
               end;
            end case;
         end loop;
      end loop;

      if Trace_Packrat_McKenzie > Outline then
         Trace.Put_Line (Packrat_Label & " max_examined_pos:" & Packrat_Parser_State.Max_Examined_Pos'Image);
      end if;

      loop
         --  Exit when reach Packrat_Parser_State.Max_Examined_Pos.
         if Trace_Packrat_McKenzie > Detail then
            Trace.Put_Line ("pos:" & Pos'Image);
         end if;

         declare
            use all type Ada.Containers.Count_Type;
            use WisiToken.Parse.LR;

            Terminal_Node       : Node_Access;
            Empty_Nonterm_Nodes : Valid_Node_Access_Lists.List;
            Nonterm_Nodes       : Valid_Node_Access_Lists.List;
            Prev_State          : State_Index := Tree.State (Stream);
            State               : Unknown_State_Index;
         begin
            Find_Nodes (Pos, Terminal_Node, Empty_Nonterm_Nodes, Nonterm_Nodes);

            --  FIXME: could be more than one nonterm valid; spawn parsers. need test case.
            if Nonterm_Nodes.Length > 0 then
               declare
                  Nonterm_Node : constant Valid_Node_Access := Nonterm_Nodes (Nonterm_Nodes.First);
               begin
                  if Tree.ID (Nonterm_Node) = Descriptor.Accept_ID then
                     --  We get here when called from Finish_Parse.Succeed_Parse
                     Tree.Push (Stream, Nonterm_Node, Accept_State);
                     Set_Error := False;
                  else
                     State := Goto_For (Shared_Parser.Table.all, Prev_State, Tree.ID (Nonterm_Node));
                     Tree.Push (Stream, Nonterm_Node, State);
                  end if;

                  if Trace_Packrat_McKenzie > Detail then
                     Trace.Put_Line
                       ("state" & Prev_State'Image & " push " & Tree.Image (Nonterm_Node, Node_Numbers => True));
                  end if;

                  Pos := Tree.Get_Node_Index
                    (Tree.Last_Source_Terminal (Nonterm_Node, Trailing_Non_Grammar => False)) + 1;
               end;

            else
               loop -- Handle empty nonterms.
                  pragma Assert (Terminal_Node /= Invalid_Node_Access);

                  declare
                     Action : constant Parse_Action_Node_Ptr := Action_For
                       (Shared_Parser.Table.all, Prev_State, Tree.ID (Terminal_Node));
                  begin
                     if Action.Next /= null then
                        --  FIXME: spawn parser
                        raise SAL.Not_Implemented with "conflict in Derivs_To_Parse_Streams";
                     end if;

                     case Action.Item.Verb is
                     when Shift =>
                        State := Shift_State (Action);
                        Tree.Push (Stream, Terminal_Node, State);

                        if Trace_Packrat_McKenzie > Detail then
                           Trace.Put_Line
                             ("state" & Prev_State'Image & " push " &
                                Tree.Image (Terminal_Node, Node_Numbers => True));
                        end if;

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
                                    State := Goto_For
                                      (Shared_Parser.Table.all, Prev_State, Tree.ID (Empty_Nonterm_Node));
                                    if State /= Unknown_State then
                                       To_Delete := Cur;
                                       Valid_Node_Access_Lists.Next (Cur);
                                       Empty_Nonterm_Nodes.Delete (To_Delete);

                                       Tree.Push (Stream, Empty_Nonterm_Node, State);
                                       Shift_Count := @ + 1;
                                       if Trace_Packrat_McKenzie > Detail then
                                          Trace.Put_Line
                                            ("state" & Prev_State'Image & " push " &
                                               Tree.Image (Empty_Nonterm_Node, Node_Numbers => True));
                                       end if;
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
                        if Tree.Label (Get_Node (Tree.Peek (Stream))) = Syntax_Trees.Nonterm then
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
                           Undo_Reduce (Tree, Shared_Parser.Table.all, Stream, Shared_Parser.User_Data);
                           Prev_State := Tree.State (Stream);

                           if Trace_Packrat_McKenzie > Detail then
                              Trace.Put (" => " & Tree.Image (Tree.Peek (Stream), State => True),
                                         Prefix => False);
                              Trace.New_Line;
                           end if;

                        else
                           --  Stack_Top is a terminal
                           if Tree.ID (Terminal_Node) in Descriptor.EOI_ID | Descriptor.SOI_ID then
                              --  Source text is empty or all comments; nothing else to do.
                              exit;
                           else
                              raise SAL.Not_Implemented with "FIXME: need test case";
                           end if;
                        end if;

                     when others =>
                        raise SAL.Not_Implemented with "FIXME: action " & Action.Item.Verb'Image;
                     end case;
                  end;
               end loop;
            end if;

            exit when Pos >= Packrat_Parser_State.Max_Examined_Pos;
         end;
      end loop;

      --  Ensure LR_Core_Parse immediately enters recover.
      Shared_Parser.Resume_Active := True;
      LR_Parser_State.Set_Verb (WisiToken.Parse.LR.Error);
      if Max_Examined_Stream_Index /= Invalid_Stream_Index then
         --  Invalid when source text is empty or only comments; share_link is ok.
         Tree.Set_Shared_Link (LR_Parser_State.Stream, Max_Examined_Stream_Index);
      end if;

      if Set_Error then
         Tree.Add_Error_To_Input
           (LR_Parser_State.Stream,
            WisiToken.Parse.Parse_Error' --  FIXME: build expecting?
              (First_Terminal => 1,
               Last_Terminal  => 0,
               Expecting      => (1 .. 0 => False),
               Recover_Ops    => WisiToken.Parse.Recover_Op_Arrays.Empty_Vector,
               Recover_Cost   => 0),
            Shared_Parser.User_Data);
      end if;
   end Derivs_To_Parse_Streams;

   procedure Packrat_Parse
     (Shared_Parser : in out Parser;
      Log_File      : in     Ada.Text_IO.File_Type)
   is separate;

   procedure Finish_Parse (Parser : in out WisiToken.Parse.Packrat.Parser.Parser'Class)
   is
      use all type Parser_State_Lists.Cursor;
      use all type Ada.Containers.Count_Type;
      use WisiToken.Syntax_Trees;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Trace : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;

      Success_Count : Integer := 0;

      Current_Parser : Parser_State_Lists.Cursor := Parser.Packrat_Parsers.First;

      procedure Terminate_Parser (Message : in String)
      --  Delete Current_Parser; leave Current_Parser at Next (Current_Parser).
      is
         To_Delete : Parser_State_Lists.Cursor := Current_Parser;
      begin
         Parser_State_Lists.Next (Current_Parser);
         if Trace_Parse > Outline then
            declare
               Parser_State : Packrat.Parser.Parser_State renames Parser.Packrat_Parsers (To_Delete);
            begin
               if Parser_State.Result.State = Failure then
                  Trace.Put_Line
                    (Parser_State.Packrat_Label'Image & ": delete; failed @" &
                       Image_Pos (Tree, Parser_State.Result.Max_Examined_Pos));
               else
                  Trace.Put_Line (Parser_State.Packrat_Label'Image & ": delete; " & Message);
               end if;
            end;
         end if;
         Parser.Packrat_Parsers.Delete (To_Delete);
      end Terminate_Parser;

      procedure Succeed_Parser
      --  Process Current_Parser as the succeeding parser.
      is
         Parser_State : Packrat.Parser.Parser_State renames Parser.Packrat_Parsers (Current_Parser);
      begin
         --  Copy recover counts for unit test.
         Parser.Recover_Enqueue_Count := Parser_State.Last_Recover_Enqueue_Count;
         Parser.Recover_Check_Count   := Parser_State.Last_Recover_Check_Count;

         New_LR_Parse_Stream (Parser, Parser_State);
         declare
            LR_Parser_State : WisiToken.Parse.LR.Parser_Lists.Parser_State renames
              Parser.Parsers.First.State_Ref;
         begin
            Derivs_To_Parse_Streams (Parser, Parser_State, LR_Parser_State);
         end;

         Parser_State.Result := No_Result_Memo;
         Clear (Parser_State.Derivs);

         Tree.Finish_Parse;

         if Trace_Packrat_McKenzie > Outline then
            Trace.Put_Line (Parser_State.Packrat_Label'Image & ": packrat parse succeed");
         end if;
      end Succeed_Parser;

   begin
      --  First count successful parsers. We don't delete parsers that did
      --  not succeed here; we may need them for error recover.
      for Parser_State of Parser.Packrat_Parsers loop
         if Parser_State.Result.State = Packrat.Success then
            Success_Count := @ + 1;
         end if;
      end loop;

      if Trace_Packrat_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("succeed:" & Success_Count'Image & " error:" &
              Integer'Image (Integer (Parser.Packrat_Parsers.Length) - Success_Count));
      end if;

      if Success_Count = 0 then
         --  preserve all Parser_States for error recover
         for Parser_State of Parser.Packrat_Parsers loop
            declare
               Msg    : constant String := Tree.Error_Message
                 (Ref     => (Tree.Shared_Stream,
                              Parser_State.Result.Max_Examined_Pos,
                              Tree.Get_Node (Tree.Shared_Stream, Parser_State.Result.Max_Examined_Pos)),
                  Message => "parse failed");
            begin
               if Trace_Parse > Outline then
                  Tree.Lexer.Trace.Put_Line (Msg);
               end if;
            end;
         end loop;

         raise WisiToken.Parse_Error with "all parsers failed";

      elsif Success_Count = 1 then
         for Parser_State of Parser.Packrat_Parsers loop
            if Parser_State.Result.State = Packrat.Success then
               Succeed_Parser;
            else
               Terminate_Parser ("");
            end if;
         end loop;

      else
         --  Multiple parsers succeeded.
         --  test_mckenzie_recover.adb Packrat_Proc Error_3
         declare
            Min_Recover_Cost        : Integer                   := Integer'Last;
            Min_Recover_Ops_Length  : Ada.Containers.Count_Type := Ada.Containers.Count_Type'Last;
            Recover_Cur             : Parser_State_Lists.Cursor := Parser.Packrat_Parsers.First;
            Equiv_Count             : Integer                   := 0;
         begin
            --  Delete parsers that did not succeed.
            loop
               if Parser.Packrat_Parsers (Current_Parser).Result.State = Success then
                  Parser_State_Lists.Next (Current_Parser);
               else
                  Terminate_Parser ("");
               end if;
               exit when Current_Parser = Parser_State_Lists.No_Element;
            end loop;

            if Parser.Error then
               --  There was at least one error. We assume that caused the ambiguous
               --  parse, and we pick the parser with the minimum recover cost,
               --  recover ops length to allow the parse to succeed.
               Current_Parser := Parser.Packrat_Parsers.First;
               loop
                  declare
                     Parser_State : Packrat.Parser.Parser_State renames Parser.Packrat_Parsers (Current_Parser);
                  begin
                     if Parser_State.Total_Recover_Cost <= Min_Recover_Cost then
                        Min_Recover_Cost := Parser_State.Total_Recover_Cost;
                        Recover_Cur      := Current_Parser;

                        if Parser_State.Max_Recover_Ops_Length <= Min_Recover_Ops_Length then
                           Min_Recover_Ops_Length := Parser_State.Max_Recover_Ops_Length;
                           Recover_Cur            := Current_Parser;
                        end if;
                     end if;
                  end;
                  Parser_State_Lists.Next (Current_Parser);
                  exit when Current_Parser = Parser_State_Lists.No_Element;
               end loop;

               --  Set Equiv_Count
               Current_Parser := Parser.Packrat_Parsers.First;
               loop
                  declare
                     Parser_State : Packrat.Parser.Parser_State renames Parser.Packrat_Parsers (Current_Parser);
                  begin
                     if Parser_State.Total_Recover_Cost = Min_Recover_Cost and then
                       Parser_State.Max_Recover_Ops_Length = Min_Recover_Ops_Length
                     then
                        Equiv_Count := @ + 1;
                     end if;
                  end;
                  Parser_State_Lists.Next (Current_Parser);
                  exit when Current_Parser = Parser_State_Lists.No_Element;
               end loop;

               Current_Parser := Parser.Packrat_Parsers.First;
               loop
                  if Current_Parser = Recover_Cur then
                     Parser_State_Lists.Next (Current_Parser);
                  else
                     Terminate_Parser
                       ((if Equiv_Count > 1
                         then "random"
                         else "recover count/cost/min length"));
                  end if;
                  exit when Current_Parser = Parser_State_Lists.No_Element;
               end loop;

               Current_Parser := Recover_Cur;
               Succeed_Parser;

            else
               --  There were no errors. We allow the parse to fail, on the
               --  assumption that an otherwise correct input should not yield an
               --  ambiguous parse.

               for Packrat_Parser_State of Parser.Packrat_Parsers loop
                  New_LR_Parse_Stream (Parser, Packrat_Parser_State);
                  declare
                     LR_Parser_State : WisiToken.Parse.LR.Parser_Lists.Parser_State renames
                       Parser.Parsers.First.State_Ref;
                  begin
                     Derivs_To_Parse_Streams (Parser, Packrat_Parser_State, LR_Parser_State);
                  end;
               end loop;

               raise WisiToken.Parse_Error with "Ambiguous parse:" & Parser.Packrat_Parsers.Length'Image &
                 " parsers active.";
            end if;
         end;
      end if;
   end Finish_Parse;

   procedure Print_Derivs
     (Derivs : in WisiToken.Parse.Packrat.Derivs;
      Tree   : in WisiToken.Syntax_Trees.Tree)
   is
      Trace  : WisiToken.Trace'Class renames Tree.Lexer.Trace.all;
      Count  : Integer := 0;
   begin
      for Nonterm in Derivs'Range loop
         for Pos in Derivs (Nonterm).First_Index .. Derivs (Nonterm).Last_Index loop

            case Derivs (Nonterm)(Pos).State is
            when No_Result =>
               null;

            when Failure | Success =>
               Count := @ + 1;
               Trace.Put_Line (Packrat.Image (Derivs (Nonterm)(Pos), Nonterm, Pos, Tree));

            end case;
         end loop;
      end loop;
      Trace.Put_Line ("... failed + success count:" & Count'Image);
   end Print_Derivs;

   procedure Print_Derivs (Parser : in WisiToken.Parse.Packrat.Parser.Parser)
   is
      Trace : WisiToken.Trace'Class renames Parser.Tree.Lexer.Trace.all;
   begin
      for Parser_State of Parser.Packrat_Parsers loop
         Trace.Put_Line ("packrat" & Parser_State.Packrat_Label'Image);
         Print_Derivs (Parser_State.Derivs, Parser.Tree);
         Trace.Put ("insert_delete: ");
         for Op_List of Parser_State.Insert_Delete loop
            for Op of Op_List loop
               Trace.Put (", " & Image (Op, Parser.Tree));
            end loop;
         end loop;
         Trace.New_Line;
         Trace.New_Line;
      end loop;
   end Print_Derivs;

end WisiToken.Parse.Packrat.Parser;
