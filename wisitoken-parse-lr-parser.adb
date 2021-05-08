--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 - 2021 Free Software Foundation, Inc.
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

      Nonterm : constant Syntax_Trees.Stream_Node_Ref := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, Action.Post_Parse_Action, New_State,
         Default_Virtual => Shared_Parser.Tree.Is_Virtual (Parser_State.Current_Token.Node));
      --  Default_Virtual is used if Nonterm is empty; we set it True
      --  because if Current_Token (that triggered the reduce) is virtual,
      --  then Nonterm would be Virtual if it was not empty.
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
         --  because it might do other things than return a status.
         declare
            Nonterm_Token : Syntax_Trees.Recover_Token := Shared_Parser.Tree.Get_Recover_Token (Nonterm);

            Children_Token : constant Syntax_Trees.Recover_Token_Array :=
              Shared_Parser.Tree.Children_Recover_Tokens (Parser_State.Stream, Nonterm.Element);
            Status         : In_Parse_Actions.Status;
         begin
            Status := Action.In_Parse_Action
              (Shared_Parser.Tree, Nonterm_Token, Children_Token, Recover_Active => False);

            if Trace_Parse > Detail then
               Shared_Parser.Trace.Put_Line ("semantic check " & In_Parse_Actions.Image (Status, Shared_Parser.Tree));
            end if;

            case Status.Label is
            when Ok =>
               return Ok;

            when In_Parse_Actions.Error =>
               if Parser_State.Resume_Active then
                  --  Ignore this error; that's how McKenzie_Recover decided to fix it
                  return Ok;

               else
                  Parser_State.Errors.Append
                    ((Label          => User_Parse_Action,
                      First_Terminal => Shared_Parser.Tree.Lexer.Descriptor.First_Terminal,
                      Last_Terminal  => Shared_Parser.Tree.Lexer.Descriptor.Last_Terminal,
                      Status         => Status,
                      Recover        => (others => <>)));
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
      --  Same logic as in McKensie_Recover.Parse.Get_Action, but this
      --  operates on Parser_State.
      use Syntax_Trees;

      Table : Parse_Table renames Shared_Parser.Table.all;
      Tree  : Syntax_Trees.Tree renames Shared_Parser.Tree;

      Current_State : constant State_Index := Tree.State (Parser_State.Stream);

      First_In_Current : Node_Access;
   begin
      loop --  Skip empty nonterms
         if Tree.Label (Parser_State.Current_Token.Node) in Terminal_Label then
            Action_Cur := Action_For (Table, Current_State, Tree.ID (Parser_State.Current_Token.Node));
            Action     := Action_Cur.Item;
            return;
         else
            --  nonterminal; we don't Insert nonterms, so
            --  Parser_State.Current_Token cannot be from an Insert.

            declare
               New_State : constant Unknown_State_Index := Goto_For
                 (Table, Current_State, Tree.ID (Parser_State.Current_Token.Node));
            begin
               if New_State /= Unknown_State then
                  Action_Cur := null;
                  Action     :=
                    (Verb       => Shift,
                     Production => Invalid_Production_ID,
                     State      => New_State);
                  return;
               else
                  First_In_Current := Tree.First_Terminal (Parser_State.Current_Token.Node);

                  if First_In_Current = Invalid_Node_Access then
                     --  Current_Token is an empty nonterm; skip it. This will not affect
                     --  Insert_Delete; that already skipped it via First_Shared_Terminal
                     --  (Stream), and we don't Delete nonterms.

                     if Parser_State.Current_Token.Stream = Parser_State.Stream then
                        Tree.Stream_Delete (Parser_State.Current_Token.Stream, Parser_State.Current_Token.Element);
                        Parser_State.Current_Token := Tree.First_Input (Parser_State.Stream);
                     else
                        Tree.Stream_Next (Parser_State.Shared_Token, Rooted => True);
                        Parser_State.Current_Token := Parser_State.Shared_Token;
                     end if;

                  else
                     Action_Cur := Action_For (Table, Current_State, Tree.ID (First_In_Current));
                     Action     := Action_Cur.Item;

                     case Action.Verb is
                     when Shift =>
                        if Parser_State.Current_Token.Stream /= Parser_State.Stream then
                           --  To breakdown a shared_stream token, we first have to create a
                           --  parse stream input element for it, and do the breakdown in the
                           --  parse stream input.
                           Tree.Move_Shared_To_Input
                             (Parser_State.Shared_Token, Parser_State.Stream, Parser_State.Current_Token);
                           Parser_State.Inc_Shared_Stream_Token := False;
                        end if;

                        if Trace_Parse > Detail then
                           Shared_Parser.Trace.Put_Line
                             (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": left_breakdown " &
                                Tree.Image (Parser_State.Current_Token, First_Terminal => True));
                        end if;
                        Tree.Left_Breakdown (Parser_State.Current_Token);

                        if Trace_Parse > Detail then
                           Shared_Parser.Trace.Put_Line
                             (" ... current_token: " & Tree.Image
                                (Parser_State.Current_Token, First_Terminal => True));
                           if Trace_Parse > Detail then
                              Shared_Parser.Trace.Put_Line
                                (" ... input stream: " & Tree.Image
                                   (Parser_State.Stream, Stack => False, Input => True, Shared => True));
                           end if;
                        end if;
                        return;

                     when Accept_It | Reduce =>
                        return;

                     when Error =>
                        if Tree.Label (Tree.Peek (Parser_State.Stream)) in Terminal_Label then
                           return;

                        else
                           --  [Wagner Graham 1998] has Right_Breakdown here, but that is often
                           --  overkill; we only need Undo_Reduce until Current_Token is
                           --  shiftable.
                           --
                           --  IMPROVEME: if error recovery is correct here, we do more
                           --  Undo_Reduce than necessary (and it will never happen in
                           --  Error_Recovery).
                           --
                           --  Note that we don't need to check if we've just done a reduce here;
                           --  we would have gotten Error for that action instead.

                           Undo_Reduce (Tree, Table, Parser_State.Stream);

                           if Trace_Parse > Detail then
                              Shared_Parser.Trace.Put_Line (" ... undo_reduce");
                           end if;
                        end if;

                     end case;
                  end if;
               end if;
            end;
         end if;
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
   begin
      if Trace_Parse > Detail then
         Trace.Put
           --  Leading space for compatibility with existing tests.
           (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
              (if Trace_Parse_No_State_Numbers
               then "-- : "
               else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
              Shared_Parser.Tree.Image (Parser_State.Current_Token, First_Terminal => True) & " : " &
              Trace_Image (Action, Shared_Parser.Tree.Lexer.Descriptor.all));
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Parser_State.Set_Verb (Shift);
         Shared_Parser.Tree.Shift (Parser_State.Stream, Action.State, Parser_State.Current_Token.Element);

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

               Parser_State.Errors.Append
                 ((Label          => LR_Parse_Action,
                   First_Terminal => 1,
                   Last_Terminal  => 0,
                   Error_Token    => Parser_State.Current_Token,
                   Expecting      => (1 .. 0 => False),
                   Recover        => (others => <>)));

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

                     --  Insert EOI on Shared_Stream
                     if Shared_Parser.Tree.ID (Parser_State.Current_Token.Node) /=
                       Shared_Parser.Tree.Lexer.Descriptor.EOI_ID
                     then
                        declare
                           Last_Token_Byte_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Byte_Region
                             (Parser_State.Current_Token.Node).Last;
                           Last_Token_Char_Region_Last : constant Buffer_Pos := Shared_Parser.Tree.Char_Region
                             (Parser_State.Current_Token.Node).Last;
                           Last_Token_Line_Region_Last : constant Line_Number_Type := Shared_Parser.Tree.Line_Region
                             (Parser_State.Current_Token).Last;

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
                           Parser_State.Current_Token := Shared_Parser.Tree.Insert_Source_Terminal
                             (Shared_Parser.Tree.Shared_Stream,
                              Terminal => EOI_Token,
                              Before   => Shared_Parser.Tree.Stream_Next (Parser_State.Current_Token).Element);
                        end;
                     end if;

                     raise;
                  end if;
               end;

               case Status is
               when Ok =>
                  Parser_State.Set_Verb (Reduce);

                  if Trace_Parse > Detail then
                     Trace.Put_Line
                       (" ... goto state " &
                          (if Trace_Parse_No_State_Numbers
                           then "--"
                           else Trimmed_Image (New_State)));
                  end if;

               when In_Parse_Actions.Error =>
                  Parser_State.Set_Verb (Error);
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

         when In_Parse_Actions.Error =>
            Parser_State.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

      when Error =>
         Parser_State.Set_Verb (Action.Verb);

         Parser_State.Zombie_Token_Count := 1;

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Shared_Parser.Tree.State (Parser_State.Stream));
         begin
            Parser_State.Errors.Append
              ((Label          => LR_Parse_Action,
                First_Terminal => Expecting'First,
                Last_Terminal  => Expecting'Last,

                Error_Token => Shared_Parser.Tree.First_Terminal_In_Node (Parser_State.Current_Token),
                --  Current_Token is never an empty nonterm; it would be broken down
                --  to nothing if it was not shiftable.

                Expecting => Expecting,
                Recover   => (others => <>)));

            if Trace_Parse > Outline then
               if Trace_Parse <= Detail then
                  Trace.Put_Line
                    (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                       (if Trace_Parse_No_State_Numbers
                        then "--"
                        else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream))) &
                       ": error: " & Shared_Parser.Tree.Image (Parser_State.Current_Token));
               end if;
               Trace.Put_Line
                 (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                    (if Trace_Parse_No_State_Numbers
                     then "--"
                     else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream))) & ": expecting: " &
                    Image (Expecting, Shared_Parser.Tree.Lexer.Descriptor.all));
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Arrays;
      use Syntax_Trees;

      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

      Ins_Del     : Vector renames Parser_State.Recover_Insert_Delete;
      Ins_Del_Cur : Extended_Index renames Parser_State.Recover_Insert_Delete_Current;
   begin
      loop
         exit when Ins_Del_Cur = Recover_Op_Arrays.No_Index;
         declare
            Op : Recover_Op renames Variable_Ref (Ins_Del, Ins_Del_Cur);
         begin
            if Op.Op = Delete then
               declare
                  Next_Sequential_Terminal : constant Stream_Node_Parents := Parser_Lists.Peek_Next_Sequential_Terminal
                    (Parser_State, Shared_Parser.Tree);
                  After_Node : Stream_Node_Parents := Tree.To_Stream_Node_Parents (Parser_State.Current_Token);
               begin
                  if Op.Del_Index = Tree.Get_Sequential_Index (Next_Sequential_Terminal.Ref.Node) then

                     Op.Del_Node := Next_Sequential_Terminal.Ref.Node;
                     Tree.Last_Terminal (After_Node);
                     Op.Del_After_Node := After_Node.Ref.Node;

                     Ins_Del_Cur := Ins_Del_Cur + 1;
                     if Ins_Del_Cur > Last_Index (Ins_Del)  then
                        Ins_Del_Cur := No_Index;
                     end if;

                     Parser_Lists.Next_Token (Parser_State, Shared_Parser.Tree, Set_Current => False, Delete => True);

                     if Trace_Parse > Extra  then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": delete " &
                             Op.Del_Index'Image);
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
   is
      use all type WisiToken.Syntax_Trees.Stream_Node_Parents;
      use all type WisiToken.Syntax_Trees.Sequential_Index;
      use all type WisiToken.Syntax_Trees.Stream_Node_Ref;

      Shift_Count   : SAL.Base_Peek_Type := 0;
      Accept_Count  : SAL.Base_Peek_Type := 0;
      Resume_Active : Boolean            := False;
      Some_Paused   : Boolean            := False;

      Min_Sequential_Index : Syntax_Trees.Sequential_Index := Syntax_Trees.Sequential_Index'Last;
   begin
      Zombie_Count := 0;

      for Parser_State of Shared_Parser.Parsers loop
         case Parser_State.Verb is
         when Pause | Shift =>
            Shift_Count := Shift_Count + 1;
            Parser_State.Set_Verb (Shift);

            --  We call Do_Deletes here so it can break down a nonterm if needed;
            --  then the check for resume done is correct. See
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
                  --  Ensure_Sequential_Node_Index only sets Node_Index thru
                  --  Resume_Token_Goal; Last_Terminal may be after that, in which case
                  --  it will have Invalid_Sequential_Index.
                  Terminal : constant Node_Access := Shared_Parser.Tree.Last_Terminal
                    (Parser_State.Current_Token.Node);
                  Terminal_Index : constant Base_Sequential_Index :=
                    (if Terminal = Invalid_Node_Access
                     then Invalid_Sequential_Index
                     else Shared_Parser.Tree.Get_Sequential_Index (Terminal));
               begin
                  if Terminal /= Invalid_Node_Access and then
                    Shared_Parser.Tree.Is_Source_Terminal (Terminal) and then
                    (Terminal_Index = Invalid_Sequential_Index or
                       Parser_State.Resume_Token_Goal <= Terminal_Index)
                  then
                     if Parser_State.Recover_Insert_Delete_Current /= Recover_Op_Arrays.No_Index then
                        --  There may still be ops left in Recover_Insert_Delete after we get to
                        --  Resume_Token_Goal. If they are from a Language_Fix that's a bug in the Language_Fix.
                        --
                        --  We don't check the parse stream input, because it can contain the
                        --  breakdown of a nonterm in the parse stream
                        --  (ada_mode-recover_10.adb), as well as push_back tokens from error
                        --  recover (ada_mode-incremental_recover_01.adb).
                        if Debug_Mode then
                           raise SAL.Programmer_Error with
                             Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) &
                             ": resume_token_goal reached with remaining insert/delete";
                        end if;
                        Resume_Active := True;
                     else
                        Parser_State.Resume_Active := False;
                        Parser_State.Resume_Token_Goal := Syntax_Trees.Invalid_Sequential_Index;
                        if Trace_Parse > Detail then
                           Shared_Parser.Trace.Put_Line
                             (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": resume_active: False");
                        end if;
                     end if;
                  else
                     Resume_Active := True;
                  end if;
               end;
            else
               if Parser_State.Shared_Token /= Syntax_Trees.Invalid_Stream_Node_Ref then
                  declare
                     use Syntax_Trees;
                     First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                       (Parser_State.Shared_Token.Node);
                  begin
                     if First_Terminal /= Invalid_Node_Access then
                        Min_Sequential_Index := Syntax_Trees.Sequential_Index'Min
                          (@, Shared_Parser.Tree.Get_Sequential_Index (First_Terminal));
                     else
                        --  No terminal in Shared_Token, so node_index does not advance
                        null;
                     end if;
                  end;
               end if;
            end if;

         when Reduce =>
            Verb := Reduce;
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

      elsif Shared_Parser.Parsers.Count = Zombie_Count then
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
      else
         for Parser_State of Shared_Parser.Parsers loop
            if Parser_State.Verb = Shift and Parser_State.Shared_Token /= Syntax_Trees.Invalid_Stream_Node_Ref then
               declare
                  use Syntax_Trees;
                  First_Terminal : constant Node_Access := Shared_Parser.Tree.First_Sequential_Terminal
                    (Parser_State.Shared_Token.Node);
               begin
                  if First_Terminal /= Invalid_Node_Access and then
                    Min_Sequential_Index /= Shared_Parser.Tree.Get_Sequential_Index (First_Terminal)
                  then
                     Some_Paused := True;
                     Parser_State.Set_Verb (Pause);
                  end if;
               end;
            end if;
         end loop;
         if not Some_Paused and Shared_Parser.Max_Sequential_Index /= Syntax_Trees.Invalid_Stream_Node_Parents then
            McKenzie_Recover.Clear_Sequential_Index (Shared_Parser);
         end if;
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
      is begin
         Shared_Parser.Parsers.First_State_Ref.Errors.Append
           ((Label          => LR.Message,
             First_Terminal => Shared_Parser.Tree.Lexer.Descriptor.First_Terminal,
             Last_Terminal  => Shared_Parser.Tree.Lexer.Descriptor.Last_Terminal,
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
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;
   begin
      Parser.Deleted_Nodes.Clear;

      --  We don't add Parser.Wrapped_Lexer_Errors nodes to Keep_Nodes; they
      --  are not always deleted, and if they were, they are in
      --  Deleted_Nodes.
      for Error of Parser.Wrapped_Lexer_Errors loop
         if Error.Recover_Token_Ref /= Invalid_Stream_Node_Ref then
            Error.Recover_Token_Ref.Stream := Invalid_Stream_ID;
            Error.Recover_Token_Ref.Element := Invalid_Stream_Index;
         end if;
      end loop;

      --  We need parents set in the following code, and we also need
      --  Recover_Op.Del_Node not yet free'd; later we need error_Token
      --  nodes not free'd.
      declare
         Keep_Nodes : Valid_Node_Access_Lists.List;
      begin
         for Error of Parser_State.Errors loop
            case Error.Label is
            when LR_Parse_Action =>
               Error.Error_Token.Stream  := Invalid_Stream_ID;
               Error.Error_Token.Element := Invalid_Stream_Index;

               Keep_Nodes.Append (Error.Error_Token.Node);
            when User_Parse_Action =>
               if not Error.Status.Begin_Name.Virtual then
                  Keep_Nodes.Append (Error.Status.Begin_Name.Node);
               end if;
               if not Error.Status.End_Name.Virtual then
                  Keep_Nodes.Append (Error.Status.End_Name.Node);
               end if;
            when Message =>
               null;
            end case;
         end loop;

         for Op of Parser_State.Recover_Insert_Delete loop
            if Op.Op = Delete then
               Parser.Deleted_Nodes.Append (Op.Del_Node);

               Keep_Nodes.Append (Op.Del_Node);
            end if;
         end loop;

         Parser.Tree.Clear_Parse_Streams (Keep_Nodes);
         Parser_State.Clear_Stream;
      end;

      if Trace_Action > Extra then
         Parser.Trace.New_Line;
         Parser.Trace.Put_Line ("post-parse tree:");
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
         --  order.
         for Op of Parser_State.Recover_Insert_Delete loop
            case Op.Op is
            when Insert =>
               Parser.User_Data.Insert_Token (Parser.Tree, Parser.Trace.all, Op.Ins_Node);

            when Delete =>
               Parser.User_Data.Delete_Token
                 (Parser.Tree, Parser.Trace.all,
                  Deleted_Token => Op.Del_Node,
                  Prev_Token    => Op.Del_After_Node);
            end case;
         end loop;

         if Trace_Action > Extra then
            Parser.Trace.Put_Line ("after insert/delete tree:");
            Parser.Trace.Put_Line
              (Parser.Tree.Image
                 (Children     => True,
                  Non_Grammar  => True,
                  Augmented    => True,
                  Line_Numbers => True));
            Parser.Trace.New_Line;
         end if;
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

      if Parser.Parsers.Count > 1 then
         raise WisiToken.Parse_Error with "ambiguous parse; can't execute actions";
      end if;

      declare
         Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;
      begin
         if Trace_Action > Outline and Parser_State.Stream /= WisiToken.Syntax_Trees.Invalid_Stream_ID then
            if Trace_Action > Extra then
               Parser.Trace.Put_Line
                 (Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": parse stream: " & Parser.Tree.Image
                    (Parser_State.Stream, Children => True, Non_Grammar => True, Augmented => True,
                     Line_Numbers => True));
            else
               Parser.Trace.Put_Line
                 (Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": root node: " & Parser.Tree.Image
                    (Parser.Tree.Root));
            end if;
         end if;

         if Parser.Tree.Root = Syntax_Trees.Invalid_Node_Access then
            --  No code in file, and error recovery failed to insert valid code.
            return;
         end if;

         Parser.User_Data.Initialize_Actions (Parser.Tree);

         Process_Node (Parser.Tree, Parser.Tree.Root);
      end;
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

   overriding function Any_Errors (Parser : in LR.Parser.Parser) return Boolean
   is
      use all type Ada.Containers.Count_Type;
      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
   begin
      return Parser.Parsers.Count > 1 or Parser_State.Errors.Length > 0 or Parser.Tree.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in LR.Parser.Parser)
   is
      use Ada.Text_IO;

      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
      Descriptor   : WisiToken.Descriptor renames Parser.Tree.Lexer.Descriptor.all;
   begin
      for Item of Parser.Tree.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Tree.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      for Item of Parser_State.Errors loop
         case Item.Label is
         when LR_Parse_Action =>
            declare
               Msg : constant String := "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                 ", found '" & Parser.Tree.Lexer.Buffer_Text (Parser.Tree.Byte_Region (Item.Error_Token.Node)) &
                 "'";
            begin
               --  If we get here because Parse raised Syntax_Error or other
               --  exception, Finish_Parse has not been called.
               Put_Line
                 (Current_Error,
                  (if Parser.Tree.Parents_Set
                   then Parser.Tree.Error_Message (Item.Error_Token.Node, Msg)
                   else Parser.Tree.Error_Message (Item.Error_Token, Msg)));
            end;

         when User_Parse_Action =>
            Put_Line
              (Current_Error,
               Parser.Tree.Lexer.File_Name & ":1:0: semantic check error: " &
                 In_Parse_Actions.Image (Item.Status, Parser.Tree));

         when Message =>
            Put_Line (Current_Error, -Item.Msg);
         end case;

         if Item.Recover.Stack.Depth /= 0 then
            Put_Line (Current_Error, "   recovered: " & Image (Item.Recover.Ops, Descriptor));
         end if;
      end loop;
   end Put_Errors;

end WisiToken.Parse.LR.Parser;
