--  Abstract :
--
--  Experiment with building a parse stream with LR states from a packrat parse that hits an error.
--
--  Copyright (C) 2022 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Lite_LALR_Main;
with Ada_Lite_Packrat_Proc_Main;
with GNAT.Traceback.Symbolic;
with SAL.Gen_Definite_Doubly_Linked_Lists;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.Packrat.Procedural;
with WisiToken.Parse.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
procedure Debug_Packrat_To_LR
is
   use WisiToken;
   use WisiToken.Syntax_Trees;

   Trace         : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File      : Ada.Text_IO.File_Type;
   Shared_Parser : WisiToken.Parse.Parser.Parser'Class := Ada_Lite_Packrat_Proc_Main.Create_Parser
     (Trace'Unrestricted_Access, null);

   Text : constant String := "procedure A is b : int; begin null end A  ;";
   --  bytes:                 |1       |10       |20       |30       |40
   --  node_Index:            1         2 3  4 5 6  7 8     9    10  11 12

   type Node_Range is record
      Node      : Node_Access;
      Pos_First : Node_Index;
      Pos_Last  : Node_Index;
   end record;

   package Node_Range_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Node_Range);

   Possible_Elements         : Node_Range_Lists.List;
   Max_Examined_Pos          : Node_Index := 0;
   Max_Examined_Stream_Index : Stream_Index;

   procedure Find_Nodes
     (Pos                 : in     Node_Index;
      Terminal_Node       : in out Node_Access;
      Empty_Nonterm_Nodes : in out Valid_Node_Access_Lists.List;
      Nonterm_Nodes       : in out Valid_Node_Access_Lists.List)
   is
      use all type WisiToken.Parse.Packrat.Memo_State;
      Tree                     : Syntax_Trees.Tree renames Shared_Parser.Tree;
      Found_Nonterm_Node_Index : Node_Index := 0;
      Found_Nonterm            : Token_ID   := Invalid_Token_ID;
   begin
      for Nonterm in Shared_Parser.Derivs'Range loop
         if Pos in Shared_Parser.Derivs (Nonterm).First_Index .. Shared_Parser.Derivs (Nonterm).Last_Index and then
           Shared_Parser.Derivs (Nonterm)(Pos).State = Parse.Packrat.Success
         then
            declare
               Node  : constant Valid_Node_Access := Shared_Parser.Derivs (Nonterm)(Pos).Result;
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
         Nonterm_Nodes.Append (Shared_Parser.Derivs (Found_Nonterm)(Pos).Result);
         return;
      end if;

      --  No nonterm, or only empty nonterm, at Pos; return the terminal
      --  from Tree.Shared_Stream. FIXME: build a map for this once.
      declare
         I : Stream_Index := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
      begin
         loop
            exit when Tree.Get_Node_Index (Tree.Get_Node (Tree.Shared_Stream, I)) = Pos;
            I := Tree.Stream_Next (Tree.Shared_Stream, I);
         end loop;
         Terminal_Node := Tree.Get_Node (Tree.Shared_Stream, I);
      end;
   end Find_Nodes;

   procedure Print_Tree (Label : in String)
   is
      Stream : Stream_ID := Shared_Parser.Tree.First_Parse_Stream;
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (Label);
      Shared_Parser.Tree.Print_Streams (Children => True);
      loop
         Shared_Parser.Put_Errors (Stream);
         Shared_Parser.Tree.Next_Parse_Stream (Stream);
         exit when Stream = Invalid_Stream_ID;
      end loop;
   end Print_Tree;

begin
   WisiToken.Trace_Parse    := 2;
   WisiToken.Trace_McKenzie := 1;
   WisiToken.Debug_Mode     := True;

   WisiToken.Parse.LR.Parser.New_Parser
     (Shared_Parser, Ada_Lite_LALR_Main.Create_Lexer (Trace'Unchecked_Access), Ada_Lite_LALR_Main.Create_Parse_Table,
      Ada_Lite_LALR_Main.Create_Productions,
      WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
      WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
      WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
      null);

   Shared_Parser.Tree.Lexer.Reset_With_String (Text);

   begin
      WisiToken.Parse.Packrat.Procedural.Packrat_Parse_No_Recover
        (WisiToken.Parse.Packrat.Procedural.Parser (Shared_Parser));
   exception
   when Parse_Error =>
      null;
   end;

   Shared_Parser.Parsers := WisiToken.Parse.LR.Parser_Lists.New_List (Shared_Parser.Tree);

   Shared_Parser.Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

   --  Find Max_Examined_Pos. Also, just for visualization, find all the
   --  top-level nonterms in Shared_Parser.Derivs; they are the possible parse
   --  stream elements.
   for Nonterm in Shared_Parser.Derivs'Range loop
      for Pos in Shared_Parser.Derivs (Nonterm).First_Index .. Shared_Parser.Derivs (Nonterm).Last_Index loop

         case Shared_Parser.Derivs (Nonterm)(Pos).State is
         when Parse.Packrat.No_Result =>
            null;

         when Parse.Packrat.Failure | Parse.Packrat.Success =>
            declare
               Max_Node : constant Valid_Node_Access := Shared_Parser.Tree.Get_Node
                    (Shared_Parser.Tree.Shared_Stream, Shared_Parser.Derivs (Nonterm)(Pos).Max_Examined_Pos);
               Max_Pos : constant Node_Index := Shared_Parser.Tree.Get_Node_Index (Max_Node);
            begin
               if Max_Examined_Pos < Max_Pos then
                  Max_Examined_Pos  := Max_Pos;
                  Max_Examined_Stream_Index := Shared_Parser.Derivs (Nonterm)(Pos).Max_Examined_Pos;
               end if;
            end;
         end case;

         case Shared_Parser.Derivs (Nonterm)(Pos).State is
         when Parse.Packrat.No_Result | Parse.Packrat.Failure =>
            null;

         when Parse.Packrat.Success =>
            declare
               Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;
               Node : constant Valid_Node_Access := Shared_Parser.Derivs (Nonterm)(Pos).Result;
            begin
               if Shared_Parser.Tree.Is_Empty_Nonterm (Node) then
                  Possible_Elements.Append
                    ((Node      => Node,
                      Pos_First => Pos,
                      Pos_Last  => Pos - 1));
               else
                  Possible_Elements.Append
                    ((Node      => Node,
                      Pos_First => Tree.Get_Node_Index (Tree.First_Terminal (Node)),
                      Pos_Last  => Tree.Get_Node_Index (Tree.Last_Terminal (Node))));
               end if;
            end;
         end case;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line ("max_examined_pos:" & Max_Examined_Pos'Image);
   Ada.Text_IO.Put_Line ("possible elements:");
   for El of Possible_Elements loop
      Ada.Text_IO.Put_Line
        (Shared_Parser.Tree.Image (El.Node, Node_Numbers => True) & El.Pos_First'Image & " .." & El.Pos_Last'Image);
   end loop;

   --  Build the parse stream from Derivs
   declare
      Pos : Node_Index := 1;
   begin
      loop
         --  Ada.Text_IO.Put_Line ("pos:" & Pos'Image);
         Ada.Text_IO.Put_Line ("pos:" & Pos'Image);

         declare
            use all type Ada.Containers.Count_Type;
            use WisiToken.Parse.LR;
            Tree   : Syntax_Trees.Tree renames Shared_Parser.Tree;
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
                        raise SAL.Not_Implemented;
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

                     when others =>
                        raise SAL.Not_Implemented with "FIXME: action " & Action.Item.Verb'Image;
                     end case;
                  end;
               end loop;
            end if;

            exit when Pos >= Max_Examined_Pos;
         end;
      end loop;
   end;

   --  Run LR_Core_Parse to do error recover thru Resume done.
   --  First ensure LR_Core_Parse immediately enters recover.
   Shared_Parser.Resume_Active := True;
   for Parser_State of Shared_Parser.Parsers loop
      Parser_State.Set_Verb (WisiToken.Parse.LR.Error);
      --  FIXME: parsers may not all have the same error token.
      Shared_Parser.Tree.Set_Shared_Link (Parser_State.Stream, Max_Examined_Stream_Index);
      Shared_Parser.Tree.Add_Error_To_Input
        (Parser_State.Stream,
         WisiToken.Parse.Parse_Error' --  FIXME: build expecting?
           (First_Terminal => 1,
            Last_Terminal  => 0,
            Expecting      => (1 .. 0 => False),
            Recover_Ops    => WisiToken.Parse.Recover_Op_Arrays.Empty_Vector,
            Recover_Cost   => 0),
         Shared_Parser.User_Data);
   end loop;

   Print_Tree ("pre-recover parse streams:");

   WisiToken.Parse.Parser.LR_Core_Parse (Shared_Parser, Log_File, Recover_Only => True);

   Print_Tree ("post-recover parse streams:");

   --  Convert LR Parse_Streams to Packrat Derivs

exception
when E : others =>
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Debug_Packrat_To_LR;
