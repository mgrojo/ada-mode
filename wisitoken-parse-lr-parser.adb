--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2022 Free Software Foundation, Inc.
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
      New_State      : in     State_Index)
     return WisiToken.In_Parse_Actions.Status_Label
   is
      --  We treat semantic check errors as parse errors here, to allow
      --  error recovery to take better advantage of them. One recovery
      --  strategy is to fix things so the semantic check passes.

      use all type In_Parse_Actions.Status_Label;
      use all type In_Parse_Actions.In_Parse_Action;

      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;

      Nonterm : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, Action.Post_Parse_Action, New_State);
   begin
      if Trace_Parse > Detail then
         Shared_Parser.Trace.Put_Line
           (Shared_Parser.Tree.Image
              (Nonterm.Node,
               Children              => True,
               Terminal_Node_Numbers => True,
               RHS_Index             => True));
      end if;

      if Action.In_Parse_Action = null then
         return Ok;

      else
         --  We have to call the semantic action even when Resume_Active,
         --  because it might do other things than return a status (ie
         --  Propagate_Name).
         declare
            Nonterm_Token : Syntax_Trees.Recover_Token := Shared_Parser.Tree.Get_Recover_Token (Nonterm);

            Children_Token : constant Syntax_Trees.Recover_Token_Array :=
              Shared_Parser.Tree.Children_Recover_Tokens (Parser_State.Stream, Nonterm.Element);
            Status         : In_Parse_Actions.Status;
         begin
            Status := Action.In_Parse_Action
              (Shared_Parser.Tree, Nonterm_Token, Children_Token, Recover_Active => False);

            if Trace_Parse > Detail then
               Shared_Parser.Trace.Put_Line
                 ("in_parse_action " & In_Parse_Actions.Image (Status, Shared_Parser.Tree, Nonterm.Node));
            end if;

            case Status.Label is
            when Ok =>
               return Ok;

            when In_Parse_Actions.Error =>
               if Parser_State.Resume_Active then
                  --  Ignore this error; that's how McKenzie_Recover decided to fix it
                  return Ok;

               else
                  Shared_Parser.Tree.Add_Error_To_Stack_Top
                    (Parser_State.Stream,
                     In_Parse_Action_Error'
                       (Status       => Status,
                        Recover_Ops  => Recover_Op_Arrays.Empty_Vector,
                        Recover_Cost => 0),
                     Shared_Parser.User_Data);

                  return Status.Label;
               end if;
            end case;
         end;
      end if;
   end Reduce_Stack_1;

   procedure Get_Action
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State;
      Action_Cur    :    out Parse_Action_Node_Ptr;
      Action        :    out Parse_Action_Rec)
   is
      --  Same logic as in McKenzie_Recover.Parse.Get_Action, but this
      --  operates on Parser_State.
      use Syntax_Trees;

      Table : Parse_Table renames Shared_Parser.Table.all;
      Tree  : Syntax_Trees.Tree renames Shared_Parser.Tree;
   begin
      loop -- handle delete empty nonterm, undo_reduce
         declare
            Current_State : constant State_Index := Tree.State (Parser_State.Stream);

            function Handle_Error return Boolean
            --  Return True if should return immediately; False if Undo_Reduce was done.
            is begin
               if Tree.Label (Tree.Peek (Parser_State.Stream)) in Terminal_Label then
                  return True;

               else
                  --  [Wagner Graham 1998] has Right_Breakdown here, but that is often
                  --  overkill; we only need Undo_Reduce until Current_Token is
                  --  shiftable. ada_mode-interactive_03.adb
                  --
                  --  IMPROVEME: if error recovery is correct here, we do more
                  --  Undo_Reduce than necessary (and it will never happen in
                  --  Error_Recovery).

                  if Parser_State.Last_Action.Verb = Reduce then
                     --  We are in an erroneous branch of a conflict, or there is a real error.
                     --  ada_mode-incremental_01.adb
                     return True;
                  end if;

                  if Trace_Parse > Detail then
                     Shared_Parser.Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                          Trimmed_Image (Current_State) & ": " & Tree.Image
                            (Shared_Parser.Tree.Current_Token (Parser_State.Stream), First_Terminal => True) &
                          " error; undo_reduce");
                     Shared_Parser.Trace.Put
                       (" ... " & Tree.Image (Tree.Peek (Parser_State.Stream), State => True));
                  end if;
                  Undo_Reduce (Tree, Table, Parser_State.Stream, Shared_Parser.User_Data);

                  if Trace_Parse > Detail then
                     Shared_Parser.Trace.Put
                       (" => " & Tree.Image (Tree.Peek (Parser_State.Stream), State => True),
                        Prefix => False);
                     Shared_Parser.Trace.New_Line;
                  end if;
                  return False;
               end if;
            end Handle_Error;

            Current_Node : constant Valid_Node_Access := Shared_Parser.Tree.Current_Token (Parser_State.Stream).Node;

         begin
            if Tree.Label (Current_Node) in Terminal_Label then
               Action_Cur := Action_For (Table, Current_State, Tree.ID (Current_Node));
               Action     := Action_Cur.Item;

               case Action.Verb is
               when Shift | Accept_It | Reduce =>
                  return;
               when Error =>
                  if Handle_Error then
                     return;
                  end if;
               end case;
            else
               declare
                  New_State : constant Unknown_State_Index := Goto_For
                    (Table, Current_State, Tree.ID (Current_Node));
               begin
                  if New_State /= Unknown_State then
                     Action_Cur := null;
                     Action     :=
                       (Verb       => Shift,
                        Production => Invalid_Production_ID,
                        State      => New_State);
                     return;
                  else
                     declare
                        Checking_Next : Boolean := False;

                        procedure Delete_Empty
                        is begin
                           if Trace_Parse > Detail then
                              Shared_Parser.Trace.Put_Line
                                (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                                   (if Trace_Parse_No_State_Numbers
                                    then "-- : "
                                    else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
                                   ": delete empty nonterm " &
                                   Tree.Image
                                     (Shared_Parser.Tree.Current_Token (Parser_State.Stream), First_Terminal => True));
                           end if;

                           Tree.Delete_Current_Token (Parser_State.Stream);
                        end Delete_Empty;

                        function Get_First_Terminal return Valid_Node_Access
                        is
                           Temp : Node_Access := Tree.First_Terminal (Current_Node);
                        begin
                           if Temp = Invalid_Node_Access then
                              --  Current_Token is an empty nonterm; peek at next terminal,
                              --  do reduce until this nonterm is shiftable.
                              --  ada_mode-interactive_03.adb
                              --  test_incremental.adb Recover_1 aspect_specification_opt.
                              Temp := Tree.First_Terminal (Shared_Parser.Tree.Current_Token (Parser_State.Stream)).Node;
                              Checking_Next := True;
                           end if;
                           return Temp;
                        end Get_First_Terminal;

                        First_In_Current : constant Valid_Node_Access := Get_First_Terminal;

                     begin
                        Action_Cur := Action_For (Table, Current_State, Tree.ID (First_In_Current));
                        Action     := Action_Cur.Item;

                        case Action.Verb is
                        when Shift =>
                           if Checking_Next then
                              --  If the empty nonterm was shiftable, it would have been handled by
                              --  Goto_For above. test_incremental.adb Edit_Code_9. Edit_Tree could
                              --  delete this nonterm, but handling it here is simpler.
                              Delete_Empty;

                           else
                              declare
                                 Current_Token : Rooted_Ref := Shared_Parser.Tree.Current_Token (Parser_State.Stream);
                              begin
                                 if Shared_Parser.Tree.Current_Token (Parser_State.Stream).Stream /=
                                   Parser_State.Stream
                                 then
                                    --  To breakdown a shared_stream token, we first have to create a
                                    --  parse stream input element for it, and do the breakdown in the
                                    --  parse stream input.
                                    Tree.Move_Shared_To_Input (Parser_State.Stream);
                                    Current_Token := Shared_Parser.Tree.Current_Token (Parser_State.Stream);
                                 end if;

                                 if Trace_Parse > Detail then
                                    Shared_Parser.Trace.Put_Line
                                      (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                                         ": left_breakdown " &
                                         Tree.Image (Current_Token, First_Terminal => True));
                                 end if;
                                 Tree.Left_Breakdown (Current_Token, Shared_Parser.User_Data);

                                 if Trace_Parse > Extra then
                                    Shared_Parser.Trace.Put_Line
                                      (" ... current_token: " & Tree.Image (Current_Token, First_Terminal => True));
                                    if Trace_Parse > Detail then
                                       Shared_Parser.Trace.Put_Line
                                         (" ... input stream: " & Tree.Image
                                            (Parser_State.Stream, Stack => False, Input => True, Shared => True));
                                    end if;
                                 end if;
                              end;
                              return;
                           end if;

                        when Accept_It | Reduce =>
                           return;

                        when Error =>
                           if Checking_Next then
                              Delete_Empty;

                           elsif Handle_Error then
                              return;
                           end if;
                        end case;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Get_Action;

   procedure Do_Action
     (Action         : in     Parse_Action_Rec;
      Current_Parser : in     Parser_Lists.Cursor;
      Shared_Parser  : in out LR.Parser.Parser)
   --  Apply Action to Current_Parser; sets Current_Parser.Verb.
   is
      use all type In_Parse_Actions.Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Status       : In_Parse_Actions.Status_Label;

      procedure Delete_Errors
      is begin
         --  We have to delete errors from all nodes in the subtree under
         --  current_input; test_incremental.adb Recover_2.
         Shared_Parser.Tree.Delete_Errors_In_Input
           (Parser_State.Stream,
            Error_Pred_Lexer_Parse_Message'Access,
            Shared_Parser.User_Data);
      end Delete_Errors;

   begin
      if Trace_Parse > Detail then
         Trace.Put
           --  No prefix, leading space for compatibility with existing tests.
           (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
              (if Trace_Parse_No_State_Numbers
               then "-- : "
               else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
              Shared_Parser.Tree.Image
                (Shared_Parser.Tree.Current_Token (Parser_State.Stream),
                 First_Terminal => True, Terminal_Node_Numbers => True) & " : " &
              Trace_Image (Action, Shared_Parser.Tree.Lexer.Descriptor.all));
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Parser_State.Set_Verb (Shift);
         Parser_State.Last_Action := Action;

         if not Parser_State.Resume_Active then
            --  Errors on nodes shifted during resume are the errors that are
            --  being fixed; we want to report them so the user can actually fix
            --  them. Errors on nodes shifted not during resume have been fixed by
            --  user edits, so we delete them.
            Delete_Errors;
         end if;

         Shared_Parser.Tree.Shift (Parser_State.Stream, Action.State);

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
               Parser_State.Set_Verb (Error);
               Parser_State.Last_Action := (Error, Invalid_Production_ID);

               Shared_Parser.Tree.Add_Error_To_Input
                 (Parser_State.Stream,
                  Parse_Error'
                    (First_Terminal => 1,
                     Last_Terminal  => 0,
                     Expecting      => (1 .. 0 => False),
                     Recover_Ops    => Recover_Op_Arrays.Empty_Vector,
                     Recover_Cost   => 0),
                  Shared_Parser.User_Data);

               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... error unknown state");
               end if;

            else
               begin
                  Status := Reduce_Stack_1 (Shared_Parser, Current_Parser, Action, New_State);
               exception
               when Partial_Parse =>
                  if Parser_State.Resume_Active or Shared_Parser.Parsers.Count > 1 then
                     --  Wait until there is one parser not in resume.
                     if Trace_Parse > Outline then
                        Trace.Put_Line (" ... partial parse done, waiting for other parsers");
                     end if;
                  else
                     if Trace_Parse > Outline then
                        Trace.Put_Line (" ... partial parse done");
                     end if;

                     declare
                        Current_Token : constant Syntax_Trees.Rooted_Ref := Shared_Parser.Tree.Current_Token
                          (Parser_State.Stream);
                     begin

                        --  Insert EOI on Shared_Stream
                        if Shared_Parser.Tree.ID (Current_Token.Node) /=
                          Shared_Parser.Tree.Lexer.Descriptor.EOI_ID
                        then
                           declare
                              Last_Token_Byte_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Byte_Region
                                (Current_Token.Node).Last;
                              Last_Token_Char_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Char_Region
                                (Current_Token.Node).Last;
                              Last_Token_Line_Region_Last : constant Line_Number_Type := Shared_Parser.Tree.Line_Region
                                (Current_Token).Last;

                              EOI_Token : constant Lexer.Token :=
                                (ID          => Shared_Parser.Tree.Lexer.Descriptor.EOI_ID,
                                 Byte_Region =>
                                   (First    => Last_Token_Byte_Region_Last + 1,
                                    Last     => Last_Token_Byte_Region_Last),
                                 Char_Region =>
                                   (First    => Last_Token_Char_Region_Last + 1,
                                    Last     => Last_Token_Char_Region_Last),
                                 Line_Region => (First | Last => Last_Token_Line_Region_Last));
                           begin
                              Shared_Parser.Tree.Insert_Source_Terminal
                                (Shared_Parser.Tree.Shared_Stream,
                                 Terminal => EOI_Token,
                                 Before   => Shared_Parser.Tree.Stream_Next (Current_Token).Element,
                                 Error    => Syntax_Trees.No_Error);
                           end;
                        end if;
                     end;
                     raise;
                  end if;
               end;

               case Status is
               when Ok =>
                  Parser_State.Set_Verb (Reduce);
                  Parser_State.Last_Action := Action;

                  if Trace_Parse > Detail then
                     Trace.Put_Line
                       (" ... goto state " &
                          (if Trace_Parse_No_State_Numbers
                           then "--"
                           else Trimmed_Image (New_State)));
                  end if;

               when In_Parse_Actions.Error =>
                  Parser_State.Set_Verb (Error);
                  Parser_State.Last_Action := Action; -- not Error, since we did a reduce.
                  Parser_State.Error_Count        := @ + 1;
                  Parser_State.Zombie_Token_Count := 1;
               end case;
            end if;
         end;

      when Accept_It =>
         case Reduce_Stack_1
           (Shared_Parser, Current_Parser,
            (Reduce, Action.Production, Action.Post_Parse_Action, Action.In_Parse_Action, Action.Token_Count),
            Accept_State)
         is
         when Ok =>
            Parser_State.Set_Verb (Action.Verb);
            if not Parser_State.Resume_Active then
               --  See note in Shift about why not delete errors during resume.
               Delete_Errors;
            end if;

         when In_Parse_Actions.Error =>
            Parser_State.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Parser_State.Set_Verb (Action.Verb);
         Parser_State.Error_Count := @ + 1;

         Parser_State.Zombie_Token_Count := 1;

         declare
            use WisiToken.Syntax_Trees;
            Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Tree.State (Parser_State.Stream));

            New_Error : constant Parse_Error :=
              (First_Terminal => Tree.Lexer.Descriptor.First_Terminal,
               Last_Terminal  => Tree.Lexer.Descriptor.Last_Terminal,
               Expecting      => Expecting,
               Recover_Ops    => Recover_Op_Arrays.Empty_Vector,
               Recover_Cost   => 0);

         begin
            if Tree.Input_Has_Matching_Error (Parser_State.Stream, New_Error) then
               --  Keep the recover information so it can be used again.
               null;

            else
               Tree.Delete_Errors_In_Input
                 (Parser_State.Stream,
                  Error_Pred_Parse'Access,
                  Shared_Parser.User_Data);

               Tree.Add_Error_To_Input
                 (Stream    => Parser_State.Stream,
                  Data      => New_Error,
                  User_Data => Shared_Parser.User_Data);

            end if;

            if Trace_Parse > Detail then
               Trace.Put_Line
                 (" " & Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                    (if Trace_Parse_No_State_Numbers
                     then "--"
                     else Trimmed_Image (Tree.State (Parser_State.Stream))) &
                    ": expecting: " & Image (Expecting, Tree.Lexer.Descriptor.all));
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Nodes_Arrays;
      use Syntax_Trees;

      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

      Ins_Del     : Vector renames Parser_State.Recover_Insert_Delete;
      Ins_Del_Cur : Extended_Index renames Parser_State.Recover_Insert_Delete_Current;
   begin
      loop
         exit when Ins_Del_Cur = Recover_Op_Nodes_Arrays.No_Index;
         declare
            Op : Recover_Op_Nodes renames Variable_Ref (Ins_Del, Ins_Del_Cur);
         begin
            if Op.Op = Delete then
               declare
                  Terminal_Node : constant Valid_Node_Access := Tree.First_Sequential_Terminal
                    (Tree.Current_Token (Parser_State.Stream)).Node;
               begin
                  if Op.Del_Index = Tree.Get_Sequential_Index (Terminal_Node) then
                     Do_Delete (Tree, Parser_State.Stream, Op, Terminal_Node, Shared_Parser.User_Data);

                     if Trace_Parse > Extra  then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": delete " &
                             Op.Del_Index'Image);
                     end if;

                     Ins_Del_Cur := Ins_Del_Cur + 1;
                     if Ins_Del_Cur > Last_Index (Ins_Del)  then
                        Ins_Del_Cur := No_Index;
                     end if;

                  else
                     exit;
                  end if;
               end;
            else
               exit;
            end if;
         end;
      end loop;
   end Do_Deletes;

   procedure Parse_Verb
     (Shared_Parser : in out LR.Parser.Parser;
      Verb          :    out All_Parse_Action_Verbs;
      Zombie_Count  :    out SAL.Base_Peek_Type)
   --  Verb: the type of parser cycle to execute;
   --
   --     Accept_It : all Parsers.Verb return Accept - done parsing.
   --
   --     Shift : some Parsers.Verb return Shift.
   --
   --     Pause : Resume is active, and this parser has reached
   --  Resume_Goal, so it is waiting for the others to catch up. Or
   --  resume is not active, and this parser has shifted a nonterminal,
   --  while some other parser has broken down that nonterminal; it is
   --  waiting for the others to catch up. This ensures parsers are
   --  within Mckenzie_Param.Zombie_Limit of the same terminal when they
   --  enter error recovery.
   --
   --     Reduce : some Parsers.Verb return Reduce.
   --
   --     Error : all Parsers.Verb return Error.
   --
   --  Zombie_Count: count of parsers in Error state
   is
      use all type WisiToken.Syntax_Trees.Stream_Node_Ref;

      Shift_Count   : SAL.Base_Peek_Type := 0;
      Accept_Count  : SAL.Base_Peek_Type := 0;
      Resume_Active : Boolean            := False;
      Some_Paused   : Boolean            := False;

      Min_Sequential_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
      Max_Byte_Last        : Buffer_Pos                    := Buffer_Pos'First;
   begin
      Zombie_Count := 0;

      for Parser_State of Shared_Parser.Parsers loop
         --  Parser_State.Verb is set by Do_Action, except Pause, Accept_It are
         --  set here.
         case Parser_State.Verb is
         when Pause | Shift =>
            Shift_Count := Shift_Count + 1;
            Parser_State.Set_Verb (Shift);

            --  We call Do_Deletes here so it can break down a nonterm if needed;
            --  then the check for resume done is correct.
            --  ada_mode-recover_bad_char.adb.
            Do_Deletes (Shared_Parser, Parser_State);

            if Parser_State.Resume_Active then
               --  We check Parser_State.Current_Token, not
               --  Parser_State.Shared_Token, because Shared_Token might be a
               --  large nonterm when Current_Token is the token goal inside
               --  Shared_Token, in the parse stream input from breakdown.
               --  ada_mode-interactive_1.adb "for File_Name in File_Names loop"
               declare
                  use WisiToken.Syntax_Trees;
                  function Get_Terminal return Node_Access
                  is
                     Ref : Stream_Node_Ref := Shared_Parser.Tree.Current_Token (Parser_State.Stream);
                  begin
                     loop
                        exit when not Shared_Parser.Tree.Is_Empty_Nonterm
                          (Shared_Parser.Tree.Get_Node (Ref.Stream, Ref.Element));
                        --  ada_mode-interactive_01.adb has empty nonterms between sequential
                        --  terminals.
                        Shared_Parser.Tree.Stream_Next (Ref, Rooted => True);
                     end loop;
                     return Shared_Parser.Tree.Last_Sequential_Terminal (Ref.Node);
                  end Get_Terminal;

                  Terminal : constant Node_Access := Get_Terminal;

                  Terminal_Index : constant Base_Sequential_Index :=
                    (if Terminal = Invalid_Node_Access
                     then Invalid_Sequential_Index
                     else Shared_Parser.Tree.Get_Sequential_Index (Terminal));
               begin
                  if Terminal_Index = Invalid_Sequential_Index
                    --  Most likely we just shifted a nonterm that got past the resume
                    --  goal; ada_mode-interactive_02.adb.
                    or else
                    (Parser_State.Resume_Token_Goal < Terminal_Index and
                       Parser_State.Recover_Insert_Delete_Current = Recover_Op_Nodes_Arrays.No_Index)
                       --  Parser_State.Recover_Insert_Delete_Current can be No_Index here
                       --  when Current_Token is a nonterm that needs to be broken down
                       --  before the remaining ops can be performed.
                       --  ada_mode-interactive_01.adb
                  then
                     Parser_State.Resume_Active := False;
                     Parser_State.Resume_Token_Goal := Syntax_Trees.Invalid_Sequential_Index;
                     if Trace_Parse > Detail then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": resume_active: False");
                     end if;
                  else
                     Resume_Active := True;
                  end if;
               end;

            elsif Shared_Parser.Resume_Active then
               declare
                  use Syntax_Trees;
                  First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                    (Shared_Parser.Tree.Shared_Token (Parser_State.Stream).Node);
               begin
                  if First_Terminal /= Invalid_Node_Access then
                     Min_Sequential_Index := Syntax_Trees.Sequential_Index'Min
                       (@, Shared_Parser.Tree.Get_Sequential_Index (First_Terminal));
                  else
                     --  No terminal in Shared_Token
                     null;
                  end if;
               end;

            else
               --  Ensure parsers stay close to the same terminal; see note below at
               --  use of Max_Byte_Last.
               declare
                  use Syntax_Trees;

                  --  We don't just use Byte_Region (stack_top), because that can be
                  --  slow, and we do this every parse cycle.
                  Last_Term : constant Node_Access := Shared_Parser.Tree.Last_Terminal
                    (Shared_Parser.Tree.Get_Node (Parser_State.Stream, Shared_Parser.Tree.Peek (Parser_State.Stream)));
               begin
                  if Last_Term /= Invalid_Node_Access then
                     Max_Byte_Last := Buffer_Pos'Max (@, Shared_Parser.Tree.Byte_Region (Last_Term).Last);
                  end if;
               end;
            end if;

         when Reduce =>
            Verb := Reduce;
            --  No need to review rest of parsers, and Zombie_Count will be
            --  ignored.
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            --  This parser is waiting for others to error; they can continue
            --  parsing.
            Zombie_Count := Zombie_Count + 1;
         end case;
      end loop;

      if Accept_Count > 0 and Shared_Parser.Parsers.Count = Accept_Count + Zombie_Count then
         Verb := Accept_It;

         if Shared_Parser.Resume_Active then
            Shared_Parser.Resume_Active := False;
            McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
         end if;

      elsif Shared_Parser.Parsers.Count = Zombie_Count then
         Verb := Error;
         return;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;

      if Resume_Active then
         Shared_Parser.Resume_Active := True;

         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift and not Parser_State.Resume_Active then
               Parser_State.Set_Verb (Pause);
               if Trace_Parse > Detail then
                  Shared_Parser.Trace.Put_Line
                    (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": pause: resume exit");
               end if;
            end if;
         end loop;

      elsif Shared_Parser.Resume_Active then
         --  Ensure all parsers are on the same terminal before exiting resume.
         --  All error recover insert and delete are done, so all parsers must
         --  see the same terminals.
         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift then
               declare
                  use Syntax_Trees;
                  First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                    (Shared_Parser.Tree.Shared_Token (Parser_State.Stream).Node);
               begin
                  if First_Terminal /= Invalid_Node_Access and then
                    Min_Sequential_Index /= Syntax_Trees.Sequential_Index'Last and then
                    Min_Sequential_Index /= Shared_Parser.Tree.Get_Sequential_Index (First_Terminal)
                  then
                     Some_Paused := True;
                     Parser_State.Set_Verb (Pause);
                     if Trace_Parse > Detail then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                             ": pause: resume sync (min index" & Min_Sequential_Index'Image & ")");
                     end if;
                  end if;
               end;
            end if;
         end loop;

         if Shared_Parser.Resume_Active and not Some_Paused then
            Shared_Parser.Resume_Active := False;
            McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
         end if;

      elsif Shared_Parser.Parsers.Count > 1 then
         --  Ensure parsers stay close to the same terminal. In general,
         --  Parser_State.Current_Token.Byte_Region should not be within
         --  another parser stack_top, unless it just included that token in a
         --  reduce. But in incremental parse, one parser can shift a nonterm,
         --  while another parser has broken down that nonterm and is working
         --  thru it one terminal at a time.
         declare
            Not_Paused         : array (1 .. Shared_Parser.Parsers.Count) of Boolean := (others => False);
            Parser_Index       : SAL.Base_Peek_Type                                  := Not_Paused'First;
            Max_Terminal_Count : Integer                                             := 0;
         begin
            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Shift then
                  declare
                     use Syntax_Trees;
                     Current_Token_Node : constant Node_Access := Shared_Parser.Tree.Current_Token
                       (Parser_State.Stream).Node;
                     First_Terminal : constant Node_Access :=
                       (if Current_Token_Node /= Invalid_Node_Access
                        then Shared_Parser.Tree.First_Terminal (Current_Token_Node)
                        else Invalid_Node_Access);
                  begin
                     if First_Terminal /= Invalid_Node_Access then
                        if Shift_Count < Shared_Parser.Parsers.Count then
                           --  Some parsers are zombies; otherwise this count is a waste of time.
                           --  ada_mode-recover_40.adb used to require Max_Terminal_Count (before
                           --  Matching_Begin added 'null;') .
                           Max_Terminal_Count := Integer'Max
                             (@, Shared_Parser.Tree.Count_Terminals (Current_Token_Node));
                        end if;

                        if Shared_Parser.Tree.Label (First_Terminal) = Source_Terminal then
                           declare
                              Region : constant Buffer_Region := Shared_Parser.Tree.Byte_Region (First_Terminal);
                           begin
                              --  Max_Byte_Last is last byte of farthest token on stack top; parsers
                              --  whose Current_Token are within that token are not paused, so they
                              --  can catch up.
                              if Region.First < Max_Byte_Last then
                                 Not_Paused (Parser_Index) := False;
                              end if;
                           end;
                        end if;
                     end if;
                  end;
                  Parser_Index := @ + 1;
               end if;
            end loop;

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Verb = Error then
                  Parser_State.Zombie_Token_Count := @ + Max_Terminal_Count;

                  if Trace_Parse > Extra then
                     Shared_Parser.Trace.Put_Line
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": zombie (" &
                          Integer'Image
                            (Shared_Parser.Table.McKenzie_Param.Zombie_Limit - Parser_State.Zombie_Token_Count) &
                          " tokens remaining)");
                  end if;
               end if;
            end loop;

            if (for all P of Not_Paused => P = False) then
               --  All parsers Current_Token are after farthest stack top; none need
               --  to be paused.
               null;
            else
               Parser_Index := Not_Paused'First;
               for Parser_State of Shared_Parser.Parsers loop
                  if Parser_State.Verb = Shift and not Not_Paused (Parser_Index) then
                     Parser_State.Set_Verb (Pause);
                     if Trace_Parse > Detail then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": pause: main sync");
                     end if;
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Parse_Verb;

   procedure Recover_To_Log
     (Shared_Parser            : in out LR.Parser.Parser;
      Recover_Log_File         : in     Ada.Text_IO.File_Type;
      Recover_Result           : in     McKenzie_Recover.Recover_Status;
      Pre_Recover_Parser_Count : in     SAL.Base_Peek_Type)
   is
      use Ada.Text_IO;
   begin
      Put
        (Recover_Log_File,
         Ada.Calendar.Formatting.Image (Ada.Calendar.Clock) & " " &
           Shared_Parser.Partial_Parse_Active'Image & " " &
           Recover_Result'Image & " " &
           Pre_Recover_Parser_Count'Image & " '" &
           Shared_Parser.Tree.Lexer.File_Name & "'");

      for Parser of Shared_Parser.Parsers loop
         Put (Recover_Log_File, '(');
         if Parser.Recover.Results.Count > 0 then
            --  Count can be 0 when error recovery fails
            Put (Recover_Log_File, Image (Parser.Recover.Results.Peek.Strategy_Counts));
         end if;
         Put
           (Recover_Log_File,
            Integer'Image (Parser.Recover.Enqueue_Count) &
              Integer'Image (Parser.Recover.Check_Count) & " " &
              Boolean'Image (Parser.Recover.Success));
         Put (Recover_Log_File, ')');
      end loop;

      New_Line (Recover_Log_File);
      Flush (Recover_Log_File);
   exception
   when others =>
      New_Line (Recover_Log_File);
      Flush (Recover_Log_File);
   end Recover_To_Log;

   procedure Check_Error
     (Shared_Parser : in out LR.Parser.Parser;
      Check_Parser  : in out Parser_Lists.Cursor)
   is
      procedure Report_Error
      is
         --  This is actually a bug in error recovery, not a source syntax error.
         Msg : constant String := Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) &
              ": error during resume";
      begin
         if Debug_Mode then
            raise SAL.Programmer_Error with Msg;
         else
            raise WisiToken.Parse_Error with Msg;
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
               Shared_Parser.Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (Check_Parser.Stream) & ": zombie");
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

   procedure Finish_Parse (Parser : in out LR.Parser.Parser)
   --  Final actions after LR accept state reached; call
   --  User_Data.Insert_Token, Delete_Token.
   is
      use WisiToken.Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;
   begin
      --  We need parents set in the following code.
      Parser.Tree.Clear_Parse_Streams;
      Parser_State.Clear_Stream;

      if Trace_Parse > Extra and then Parser_State.Recover_Insert_Delete.Length > 0 then
         Parser.Trace.New_Line;
         Parser.Trace.Put_Line ("before insert/delete tree:");
         Parser.Trace.Put_Line
           (Parser.Tree.Image
              (Children     => True,
               Non_Grammar  => True,
               Augmented    => True,
               Line_Numbers => True));
         Parser.Trace.Put_Line
           ("recover_insert_delete: " & Image (Parser_State.Recover_Insert_Delete, Parser.Tree));
         Parser.Trace.New_Line;
      end if;

      if Parser.User_Data /= null then
         --  ada-mode-recover_33.adb requires calling Insert_Token,
         --  Delete_Token in lexical order, which is Recover_Insert_Delete
         --  order. Other use cases would benefit from calling all Delete
         --  first, or all Insert first, but we use this order as the least
         --  surprising.
         for Op of Parser_State.Recover_Insert_Delete loop
            case Op.Op is
            when Insert =>
               Parser.User_Data.Insert_Token (Parser.Tree, Parser.Trace.all, Op.Ins_Node);

            when Delete =>
               Parser.User_Data.Delete_Token
                 (Parser.Tree, Parser.Trace.all,
                  Deleted_Token => Op.Del_Node);
            end case;
         end loop;
      end if;

      if Trace_Parse > Extra or Trace_Action > Extra then
         Parser.Trace.Put_Line ("post-parse tree:");
         Parser.Trace.Put_Line
           (Parser.Tree.Image
              (Children     => True,
               Non_Grammar  => True,
               Augmented    => True,
               Line_Numbers => True));
         Parser.Trace.New_Line;
      end if;

      if Debug_Mode then
         declare
            Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
         begin
            if Parser.User_Data = null then
               declare
                  Dummy : User_Data_Type;
               begin
                  Parser.Tree.Validate_Tree (Dummy, Error_Reported, Node_Index_Order => False);
               end;
            else
               Parser.Tree.Validate_Tree (Parser.User_Data.all, Error_Reported, Node_Index_Order => False);
            end if;

            if Error_Reported.Count /= 0 then
               raise WisiToken.Parse_Error with "parser: validate_tree failed";
            end if;
         end;
      end if;
   end Finish_Parse;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out LR.Parser.Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser                         :    out LR.Parser.Parser;
      Trace                          : in     WisiToken.Trace_Access;
      Lexer                          : in     WisiToken.Lexer.Handle;
      Table                          : in     Parse_Table_Ptr;
      Language_Fixes                 : in     Language_Fixes_Access;
      Language_Matching_Begin_Tokens : in     Language_Matching_Begin_Tokens_Access;
      Language_String_ID_Set         : in     Language_String_ID_Set_Access;
      User_Data                      : in     Syntax_Trees.User_Data_Access)
   is begin
      Parser.Trace      := Trace;
      Parser.Tree.Lexer := Lexer;
      Parser.User_Data  := User_Data;

      --  In Base_Parser; Tree, Line_Begin_Token, Last_Grammar_Node are default initialized.

      Parser.Table                          := Table;
      Parser.Language_Fixes                 := Language_Fixes;
      Parser.Language_Matching_Begin_Tokens := Language_Matching_Begin_Tokens;
      Parser.Language_String_ID_Set         := Language_String_ID_Set;

      --  In Parser; String_Quote_Checked, Post_Recover, Parsers are default
      --  initialized. Partial_Parse_Active is set by user after this.
   end New_Parser;

   overriding procedure Parse
     (Shared_Parser    : in out LR.Parser.Parser;
      Recover_Log_File : in     Ada.Text_IO.File_Type;
      Edits            : in     KMN_Lists.List := KMN_Lists.Empty_List)
   is separate;

   overriding procedure Execute_Actions
     (Parser              : in out LR.Parser.Parser;
      Action_Region_Bytes : in     WisiToken.Buffer_Region)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.Post_Parse_Action;
      use all type Syntax_Trees.User_Data_Access;

      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use all type Syntax_Trees.Node_Label;
         Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
           (Node, Trailing_Non_Grammar => True);
      begin
         if Tree.Label (Node) /= Nonterm or else
           not (Node_Byte_Region = Null_Buffer_Region or
                  Overlaps (Node_Byte_Region, Action_Region_Bytes))
         then
            return;
         end if;

         for Child of Tree.Children (Node) loop
            if Child /= Syntax_Trees.Invalid_Node_Access then
               --  Child can be null in an edited tree
               Process_Node (Tree, Child);
            end if;
         end loop;

         Parser.User_Data.Reduce (Tree, Node);
         if Tree.Action (Node) /= null then
            begin
               Tree.Action (Node) (Parser.User_Data.all, Tree, Node);
            exception
            when E : others =>
               if WisiToken.Debug_Mode then
                  Parser.Trace.Put_Line
                    (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
                  Parser.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
                  Parser.Trace.New_Line;
               end if;

               raise WisiToken.Parse_Error with Tree.Error_Message
                 (Node,
                  "action raised exception " & Ada.Exceptions.Exception_Name (E) & ": " &
                    Ada.Exceptions.Exception_Message (E));
            end;
         end if;
      end Process_Node;

   begin
      if Parser.User_Data = null then
         return;
      end if;

      if Parser.Tree.Root = Syntax_Trees.Invalid_Node_Access then
         --  No code in file, and error recovery failed to insert valid code.
         --  Or ambiguous parse; Finish_Parse not called.
         return;
      end if;

      Parser.User_Data.Initialize_Actions (Parser.Tree);

      Process_Node (Parser.Tree, Parser.Tree.Root);
   exception
   when WisiToken.Parse_Error =>
      raise;

   when E : others =>
      if Debug_Mode then
         Parser.Trace.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Parser.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         Parser.Trace.New_Line;
      end if;
      raise;
   end Execute_Actions;

end WisiToken.Parse.LR.Parser;
