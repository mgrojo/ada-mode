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
      New_State      : in     Unknown_State_Index;
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

      Nonterm : constant Syntax_Trees.Stream_Node_Ref := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, Action.Action, New_State,
         Default_Virtual => Shared_Parser.Tree.Is_Virtual (Parser_State.Current_Token.Node));
      --  Default_Virtual is used if Nonterm is empty; we set it True
      --  because if Current_Token (that triggered the reduce) is virtual,
      --  then Nonterm would be Virtual if it was not empty.
   begin
      if Trace_Parse > Detail then
         Trace.Put_Line
           (Shared_Parser.Tree.Image
              (Nonterm.Node,
               Children              => True,
               Terminal_Node_Numbers => True,
               RHS_Index             => True));
      end if;

      if Action.Check = null then
         return Ok;

      else
         --  We have to call the semantic action even when Resume_Active,
         --  because it might do other things than return a status.
         declare
            Nonterm_Token : Syntax_Trees.Recover_Token := Shared_Parser.Tree.Get_Recover_Token (Nonterm);

            Children_Token : constant Syntax_Trees.Recover_Token_Array :=
              Shared_Parser.Tree.Children_Recover_Tokens (Parser_State.Stream, Nonterm.Element);
            Status         : Semantic_Checks.Check_Status;
         begin
            Status := Action.Check (Lexer, Nonterm_Token, Children_Token, Recover_Active => False);

            if Trace_Parse > Detail then
               Trace.Put_Line ("semantic check " & Semantic_Checks.Image (Status, Shared_Parser.Tree));
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
                      First_Terminal => Shared_Parser.Descriptor.First_Terminal,
                      Last_Terminal  => Shared_Parser.Descriptor.Last_Terminal,
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
   --  Apply Action to Current_Parser; sets Current_Parser.Verb.
   is
      use all type Semantic_Checks.Check_Status_Label;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Status       : Semantic_Checks.Check_Status_Label;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           --  Leading space for compatibility with existing tests.
           (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
              (if Trace_Parse_No_State_Numbers
               then "-- : "
               else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
              Shared_Parser.Tree.Image (Parser_State.Current_Token) & " : ");
         Put (Trace, Trace_Image (Action, Shared_Parser.Tree.Descriptor.all));
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Parser_State.Set_Verb (Shift);
         Shared_Parser.Tree.Shift
           (Parser_State.Stream, Action.State, Parser_State.Current_Token.Element, Shared_Parser.User_Data);

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
               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... error");
               end if;

            else
               Status := Reduce_Stack_1 (Shared_Parser, Current_Parser, Action, New_State, Shared_Parser.Lexer, Trace);

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

               when Semantic_Checks.Error =>
                  Parser_State.Set_Verb (Error);
                  Parser_State.Zombie_Token_Count := 1;
               end case;
            end if;
         exception
         when Partial_Parse =>
            --  Insert EOI on parse stream and Shared_Stream.
            declare
               Last_Token : constant Base_Token := Shared_Parser.Tree.Base_Token (Parser_State.Current_Token.Node);
               EOI_Token  : constant Base_Token :=
                 (ID          => Shared_Parser.Descriptor.EOI_ID,
                  Byte_Region =>
                    (First    => Last_Token.Byte_Region.Last + 1,
                     Last     => Last_Token.Byte_Region.Last),
                  Line        => Last_Token.Line,
                  Char_Region =>
                    (First    => Last_Token.Char_Region.Last + 1,
                     Last     => Last_Token.Char_Region.Last));
            begin
               if Shared_Parser.Tree.ID (Parser_State.Current_Token.Node) /=
                 Shared_Parser.Descriptor.EOI_ID
               then
                  Parser_State.Current_Token := Shared_Parser.Tree.Insert_Shared_Terminal
                    (Shared_Parser.Tree.Shared_Stream,
                     Terminal => EOI_Token,
                     Index    => Shared_Parser.Tree.Get_Node_Index (Parser_State.Current_Token.Node) + 1,
                     Before   => Shared_Parser.Tree.Stream_Next
                       (Shared_Parser.Tree.Shared_Stream, Parser_State.Current_Token.Element));
                  --  FIXME: delete rest of Shared_Stream?
               end if;

               Shared_Parser.Tree.Finish_Parse
                 (Parser_State.Stream, Parser_State.Current_Token.Element, Shared_Parser.User_Data);
            end;
            raise;
         end;

      when Accept_It =>
         case Reduce_Stack_1
           (Shared_Parser, Current_Parser,
            (Reduce, Action.Production, Action.Action, Action.Check, Action.Token_Count),
            Unknown_State, Shared_Parser.Lexer, Trace)
         is
         when Ok =>
            Parser_State.Set_Verb (Action.Verb);

         when Semantic_Checks.Error =>
            Parser_State.Set_Verb (Error);
            Parser_State.Zombie_Token_Count := 1;
         end case;

         --  Insert EOI so parse stream matches Shared_Stream; Stack_Top is wisitoken_accept.
         Shared_Parser.Tree.Finish_Parse
           (Parser_State.Stream, Parser_State.Current_Token.Element, Shared_Parser.User_Data);

      when Error =>
         Parser_State.Set_Verb (Action.Verb);

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
                  " " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                    (if Trace_Parse_No_State_Numbers
                     then "--"
                     else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream))) & ": expecting: " &
                    Image (Expecting, Shared_Parser.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;
   end Do_Action;

   procedure Do_Deletes
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use Recover_Op_Arrays;
      use all type WisiToken.Syntax_Trees.Stream_Index;
      use all type WisiToken.Syntax_Trees.Stream_Node_Ref;
      use all type WisiToken.Syntax_Trees.Node_Label;

      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

      Ins_Del     : Vector renames Parser_State.Recover_Insert_Delete;
      Ins_Del_Cur : Extended_Index renames Parser_State.Recover_Insert_Delete_Current;
   begin
      loop
         exit when Ins_Del_Cur = Recover_Op_Arrays.No_Index;
         declare
            Op : Recover_Op renames Variable_Ref (Ins_Del, Ins_Del_Cur);
         begin
            if Op.Op = Delete and then
              Op.Del_Index = Tree.Get_Node_Index
                (if Parser_State.Inc_Shared_Token
                 then Tree.Next_Shared_Terminal (Tree.Shared_Stream, Parser_State.Shared_Token).Node
                 else Parser_State.Shared_Token.Node)
            then
               pragma Assert
                 (Tree.Label (Parser_State.Shared_Token.Node) = Syntax_Trees.Shared_Terminal,
                  "FIXME: Do_Deletes needs Left_Breakdown.");

               Op.Del_Node := Parser_State.Shared_Token.Node;

               Parser_State.Shared_Token := Tree.Next_Shared_Terminal (Tree.Shared_Stream, Parser_State.Shared_Token);
               --  We don't reset Inc_Shared_Token here; only after
               --  Parser_State.Shared_Token is shifted.

               Ins_Del_Cur := Ins_Del_Cur + 1;
               if Ins_Del_Cur > Last_Index (Ins_Del)  then
                  Ins_Del_Cur := No_Index;
               end if;
            else
               exit;
            end if;
         end;
      end loop;

      if Trace_Parse > Extra and Parser_State.Shared_Token /= Syntax_Trees.Invalid_Stream_Node_Ref then
         Shared_Parser.Trace.Put_Line
           (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": (do_deletes) shared_token:" &
              Tree.Get_Node_Index (Parser_State.Shared_Token.Node)'Image &
              " inc_shared_token: " & Parser_State.Inc_Shared_Token'Image &
              " recover_insert_delete:" &
              (if Parser_State.Recover_Insert_Delete_Current = No_Index
               then ""
               else Parser_State.Recover_Insert_Delete_Current'Image & " " &
                  Image
                    (Constant_Ref (Parser_State.Recover_Insert_Delete, Parser_State.Recover_Insert_Delete_Current),
                     Shared_Parser.Tree)));
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
               --  There may still be ops left in Recover_Insert_Delete, or
               --  pushed_back tokens in the parse stream input, after we get to
               --  Resume_Token_Goal, probably from a Language_Fix or string quote
               --  fix that deletes a lot of tokens. FIXME: that's a bug!
               if Parser_State.Resume_Token_Goal <= Shared_Parser.Tree.Get_Node_Index
                 (Parser_State.Shared_Token.Node)
               then
                  if Parser_State.Recover_Insert_Delete_Current /= Recover_Op_Arrays.No_Index or
                    Shared_Parser.Tree.Has_Input (Parser_State.Stream)
                  then
                     if Debug_Mode then
                        raise SAL.Programmer_Error with
                          "resume_token_goal reached with remaining insert/delete/push_back";
                     end if;
                     Resume_Active := True;
                  else
                     Parser_State.Resume_Active := False;
                     if Trace_Parse > Detail then
                        Shared_Parser.Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": resume_active: False");
                     end if;
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
      User_Data                      : in              WisiToken.Syntax_Trees.User_Data_Access)
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

      Parser.Enable_McKenzie_Recover := not McKenzie_Defaulted (Table.all);

      --  In Parser; String_Quote_Checked, Post_Recover, Parsers are default
      --  initialized. Recover_Log_File, Partial_Parse_Active are set by
      --  user after this.

      if User_Data /= null then
         User_Data.Set_Lexer (Lexer, Parser.Line_Begin_Char_Pos'Unchecked_Access);
         --  Parser will last as long as Lexer.
      end if;
   end New_Parser;

   procedure Check_Error
     (Shared_Parser : in out LR.Parser.Parser;
      Check_Parser  : in out Parser_Lists.Cursor)
   is
      procedure Report_Error
      is begin
         Shared_Parser.Parsers.First_State_Ref.Errors.Append
           ((Label          => LR.Message,
             First_Terminal => Shared_Parser.Descriptor.First_Terminal,
             Last_Terminal  => Shared_Parser.Descriptor.Last_Terminal,
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

   overriding procedure Parse (Shared_Parser : in out LR.Parser.Parser) is separate;

   procedure Parse_Incremental
     (Shared_Parser : in out LR.Parser.Parser;
      Edits         : in     KMN_Lists.List)
   is separate;

   overriding
   procedure Execute_Actions
     (Parser          : in out LR.Parser.Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null)
   is
      use all type Syntax_Trees.User_Data_Access;
      use all type WisiToken.Syntax_Trees.Semantic_Action;

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
                     if WisiToken.Debug_Mode then
                        Parser.Trace.Put_Line
                          (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
                        Parser.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
                        Parser.Trace.New_Line;
                     end if;

                     raise WisiToken.Parse_Error with Error_Message
                       (Parser.Lexer.File_Name, Token.Line, Column (Token, Parser.Line_Begin_Char_Pos),
                        "action raised exception " & Ada.Exceptions.Exception_Name (E) & ": " &
                          Ada.Exceptions.Exception_Message (E));
                  end;
               end;
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data = null then
         return;
      end if;

      if Parser.Parsers.Count > 1 then
         raise Syntax_Error with "ambiguous parse; can't execute actions";
      end if;

      declare
         use Recover_Op_Arrays;

         Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_State_Ref;

         function Validate_Recover_Ops return Boolean
         is
            use Syntax_Trees;
         begin
            for I in First_Index (Parser_State.Recover_Insert_Delete) ..
              Last_Index (Parser_State.Recover_Insert_Delete)
            loop
               declare
                  Op : constant Recover_Op := Element (Parser_State.Recover_Insert_Delete, I);
               begin
                  case Op.Op is
                  when Insert =>
                     if Op.Ins_Before /= Invalid_Node_Index or
                       Op.Ins_Node = Invalid_Node_Access
                     then
                        return False;
                     end if;
                  when Delete =>
                     if Op.Del_Index /= Invalid_Node_Index or
                       Op.Del_Node = Invalid_Node_Access or
                       (I > First_Index (Parser_State.Recover_Insert_Delete) and
                          Op.Del_After_Node = Invalid_Node_Access)
                     then
                        return False;
                     end if;
                  end case;
               end;
            end loop;
            return True;
         end Validate_Recover_Ops;

      begin
         if Trace_Action > Outline then
            if Trace_Action > Extra then
               Parser.Tree.Print_Tree (Parser.Tree.Root, Image_Augmented);
               Parser.Trace.New_Line;
            end if;
            Parser.Trace.Put_Line
              (Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": root node: " & Parser.Tree.Image
                 (Parser.Tree.Root));
         end if;

         declare
            --  Recompute Parser.Line_Begin_Token using final parse tree terminal
            --  sequence, and compute recover_op.Del_After_Node.
            --
            --  We do not move non_grammar from deleted tokens, because the user
            --  code may want to do something different.

            use Syntax_Trees;
            Tree : Syntax_Trees.Tree renames Parser.Tree;

            I                    : Node_Access        := Tree.First_Terminal (Tree.Root);
            Last_Line            : Line_Number_Type   := Invalid_Line_Number;
            Last_Terminal        : Node_Access        := Invalid_Node_Access;
            Last_Shared_Terminal : Node_Access        := Invalid_Node_Access;
            Next_Shared_Terminal : Node_Access        := Tree.First_Shared_Terminal (Tree.Root);
            J                    : SAL.Base_Peek_Type := Parser_State.Recover_Insert_Delete.First_Index;

            Next_Delete_Index : Node_Index;

            procedure Get_Next_Recover_Op
            is begin
               loop
                  if J > Parser_State.Recover_Insert_Delete.Last_Index then
                     Next_Delete_Index := Invalid_Node_Index;
                     exit;
                  end if;
                  declare
                     Op : Recover_Op renames Constant_Ref (Parser_State.Recover_Insert_Delete, J);
                  begin
                     case Op.Op is
                     when Insert =>
                        null;

                     when Delete =>
                        Next_Delete_Index := Op.Del_Index;
                        exit;
                     end case;
                  end;
                  J := J + 1;
               end loop;
            end Get_Next_Recover_Op;

         begin
            Get_Next_Recover_Op;

            loop
               loop
                  exit when Next_Delete_Index = Invalid_Node_Index;

                  if (Last_Shared_Terminal = Invalid_Node_Access or else
                        Next_Delete_Index > Tree.Get_Node_Index (Last_Shared_Terminal)) and
                    (Next_Shared_Terminal = Invalid_Node_Access or else
                       Next_Delete_Index < Tree.Get_Node_Index (Next_Shared_Terminal))
                  then
                     declare
                        Op : Recover_Op renames Parser_State.Recover_Insert_Delete.Variable_Ref (J);
                     begin
                        pragma Assert (Op.Op = Delete);
                        Op.Del_After_Node := Last_Terminal;
                     end;
                  else
                     exit;
                  end if;
                  J := J + 1;
                  Get_Next_Recover_Op;
               end loop;

               case Tree.Label (I) is
               when Shared_Terminal =>
                  if Tree.Base_Token (I).Line /= Last_Line then
                     Parser.Line_Begin_Token (Tree.Base_Token (I).Line) :=
                       (Node    => I,
                        Element => Invalid_Stream_Index);

                     Last_Line := Tree.Base_Token (I).Line;
                  end if;

                  Last_Terminal        := I;
                  Last_Shared_Terminal := I;
                  Next_Shared_Terminal := Tree.Next_Shared_Terminal (I);

               when Virtual_Terminal =>
                  if Tree.Base_Token (I).Line /= Last_Line then
                     Parser.Line_Begin_Token (Tree.Base_Token (I).Line) :=
                       (Node    => I,
                        Element => Invalid_Stream_Index);

                     Last_Line := Tree.Base_Token (I).Line;
                  end if;
                  Last_Terminal := I;

               when Virtual_Identifier =>
                  raise SAL.Programmer_Error with "Virtual_Identifier in parse tree";

               when Nonterm =>
                  raise SAL.Programmer_Error with "Nonterm as Next_Terminal";
               end case;

               exit when Tree.ID (I) = Parser.Descriptor.EOI_ID;
               --  From a wisitoken_accept production containing EOI

               I := Tree.Next_Terminal (I);

               exit when I = Invalid_Node_Access;
               --  From a partial parse.
            end loop;
         end;

         if Trace_Action > Detail then
            Parser.Trace.Put_Line
              ("recover_insert_delete: " & Image (Parser_State.Recover_Insert_Delete, Parser.Tree));
         end if;

         pragma Assert (Validate_Recover_Ops);

         --  FIXME: incremental reparse requires parse_streams. They are
         --  cleared here because we are editing the tree; does that actually
         --  invalidate the parse streams?
         Parser.Tree.Clear_Parse_Streams;

         --  In ada-mode, Delete_Token modifies Next_Terminal
         --  (Deleted_Token).Augmented, which may be an inserted token; call
         --  all Insert_Token before any Delete_Token.
         for I in First_Index (Parser_State.Recover_Insert_Delete) ..
           Last_Index (Parser_State.Recover_Insert_Delete)
         loop
            declare
               Op : Recover_Op renames Constant_Ref (Parser_State.Recover_Insert_Delete, I);
            begin
               case Op.Op is
               when Insert =>
                  Parser.User_Data.Insert_Token (Parser.Tree, Op.Ins_Node);

               when Delete =>
                  null;
               end case;
            end;
         end loop;

         for I in First_Index (Parser_State.Recover_Insert_Delete) ..
           Last_Index (Parser_State.Recover_Insert_Delete)
         loop
            declare
               Op : Recover_Op renames Constant_Ref (Parser_State.Recover_Insert_Delete, I);
            begin
               case Op.Op is
               when Insert =>
                  null;
               when Delete         =>
                  Parser.User_Data.Delete_Token
                    (Parser.Tree,
                     Deleted_Token => Op.Del_Node,
                     Prev_Token    => Op.Del_After_Node);
               end case;
            end;
         end loop;

         Parser.User_Data.Initialize_Actions (Parser.Tree);

         Parser.Tree.Process_Tree (Process_Node'Access);
      end;
   exception
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
      return Parser.Parsers.Count > 1 or Parser_State.Errors.Length > 0 or Parser.Lexer.Errors.Length > 0;
   end Any_Errors;

   overriding procedure Put_Errors (Parser : in LR.Parser.Parser)
   is
      use Ada.Text_IO;

      Parser_State : Parser_Lists.Parser_State renames Parser.Parsers.First_Constant_State_Ref;
      Descriptor   : WisiToken.Descriptor renames Parser.Descriptor.all;
   begin
      for Item of Parser.Lexer.Errors loop
         Put_Line
           (Current_Error,
            Parser.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image (Item.Char_Pos));
      end loop;

      for Item of Parser_State.Errors loop
         case Item.Label is
         when Action =>
            if Parser.Tree.Is_Virtual (Item.Error_Token.Node) then
               Put_Line
                 (Current_Error,
                  Error_Message
                    (Parser.Lexer.File_Name, 1, 0,
                     "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                       ", found " & Image (Parser.Tree.ID (Item.Error_Token.Node), Descriptor)));
            else
               declare
                  Token : constant Base_Token := Parser.Tree.Base_Token (Item.Error_Token.Node);
               begin
                  Put_Line
                    (Current_Error,
                     Error_Message
                       (Parser.Lexer.File_Name, Token.Line, Column (Token, Parser.Line_Begin_Char_Pos),
                        "syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                          ", found '" & Parser.Lexer.Buffer_Text (Token.Byte_Region) & "'"));
               end;
            end if;

         when Check =>
            Put_Line
              (Current_Error,
               Parser.Lexer.File_Name & ":1:0: semantic check error: " &
                 Semantic_Checks.Image (Item.Check_Status, Parser.Tree));

         when Message =>
            Put_Line (Current_Error, -Item.Msg);
         end case;

         if Item.Recover.Stack.Depth /= 0 then
            Put_Line (Current_Error, "   recovered: " & Image (Item.Recover.Ops, Descriptor));
         end if;
      end loop;
   end Put_Errors;

end WisiToken.Parse.LR.Parser;
