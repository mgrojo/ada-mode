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

with Ada.Exceptions;
with GNAT.Traceback.Symbolic;
package body WisiToken.Parse.LR.Parser_No_Recover is

   procedure Reduce_Stack_1
     (Shared_Parser  : in out Parser;
      Current_Parser : in     Parser_Lists.Cursor;
      Action         : in     Reduce_Action_Rec;
      New_State      : in     State_Index;
      Trace          : in out WisiToken.Trace'Class)
   is
      Parser_State  : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;

      Nonterm : constant Syntax_Trees.Stream_Node_Ref := Shared_Parser.Tree.Reduce
        (Parser_State.Stream, Action.Production, Action.Token_Count, Action.Post_Parse_Action, New_State);
   begin
      if Trace_Parse > Detail then
         Trace.Put_Line
           (Shared_Parser.Tree.Image (Nonterm.Node, Children => True, Terminal_Node_Numbers => True));
      end if;
   end Reduce_Stack_1;

   procedure Do_Action
     (Action         : in     Parse_Action_Rec;
      Current_Parser : in     Parser_Lists.Cursor;
      Shared_Parser  : in out Parser)
   is
      use Syntax_Trees;
      use all type Ada.Containers.Count_Type;

      Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      Trace        : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
   begin
      if Trace_Parse > Detail then
         Trace.Put
           (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": " &
              Trimmed_Image (Shared_Parser.Tree.State (Current_Parser.Stream)) & ": " &
              Shared_Parser.Tree.Image (Parser_State.Current_Token.Node, Terminal_Node_Numbers => True) & " : ");
         Put (Trace, Trace_Image (Action, Shared_Parser.Tree.Lexer.Descriptor.all));
         Trace.New_Line;
      end if;

      case Action.Verb is
      when Shift =>
         Current_Parser.Set_Verb (Shift);
         Shared_Parser.Tree.Shift (Parser_State.Stream, Action.State, Parser_State.Current_Token.Element);

      when Reduce =>
         Current_Parser.Set_Verb (Reduce);

         declare
            New_State : constant Unknown_State_Index := Goto_For
              (Table => Shared_Parser.Table.all,
               State => Shared_Parser.Tree.State
                 (Parser_State.Stream,
                  Shared_Parser.Tree.Peek (Parser_State.Stream, SAL.Peek_Type (Action.Token_Count + 1))),
               ID    => Action.Production.LHS);
         begin
            if New_State = Unknown_State then
               --  This is due to a bug in the LALR parser generator (see
               --  lalr_generator_bug_01.wy); we treat it as a syntax error.
               Current_Parser.Set_Verb (Error);
               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... error");
               end if;
            else
               Reduce_Stack_1 (Shared_Parser, Current_Parser, Action, New_State, Trace);

               if Trace_Parse > Detail then
                  Trace.Put_Line (" ... goto state " & Trimmed_Image (New_State));
               end if;
            end if;
         end;

      when Accept_It =>
         Current_Parser.Set_Verb (Accept_It);

         Reduce_Stack_1
           (Shared_Parser, Current_Parser,
            (Reduce, Action.Production, Action.Post_Parse_Action, Action.In_Parse_Action, Action.Token_Count),
            Accept_State, Trace);

         pragma Assert
           (Shared_Parser.Tree.ID
              (Shared_Parser.Tree.Shared_Stream, Parser_State.Current_Token.Element) =
              Shared_Parser.Tree.Lexer.Descriptor.EOI_ID);

      when Error =>
         Current_Parser.Set_Verb (Action.Verb);

         --  We don't raise Syntax_Error here; another parser may be able to
         --  continue.

         declare
            Expecting : constant Token_ID_Set := LR.Expecting
              (Shared_Parser.Table.all, Shared_Parser.Tree.State (Current_Parser.Stream));
         begin
            Shared_Parser.Tree.Set_Error
              (Stream            => Parser_State.Stream,
               Error_Ref         => Parser_State.Current_Token,
               Data              => new WisiToken.Parse.Parse_Error'
                 (First_Terminal => Shared_Parser.Tree.Lexer.Descriptor.First_Terminal,
                  Last_Terminal  => Shared_Parser.Tree.Lexer.Descriptor.Last_Terminal,
                  Expecting      => Expecting,
                  Recover_Ops    => Recover_Op_Arrays.Empty_Vector,
                  Recover_Cost   => 0),
               User_Data         => Shared_Parser.User_Data);

            if Trace_Parse > Outline then
               Put
                 (Trace,
                  " " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ": expecting: " &
                    Image (Expecting, Shared_Parser.Tree.Lexer.Descriptor.all));
               Trace.New_Line;
            end if;
         end;
      end case;
   end Do_Action;

   procedure Parse_Verb
     (Shared_Parser : in out Parser;
      Verb          :    out All_Parse_Action_Verbs)
   --  Return the type of parser cycle to execute.
   --
   --  Accept : all Parsers.Verb return Accept - done parsing.
   --
   --  Shift : some Parsers.Verb return Shift.
   --
   --  Reduce : some Parsers.Verb return Reduce.
   --
   --  Error : all Parsers.Verb return Error.
   is
      Shift_Count  : SAL.Base_Peek_Type := 0;
      Accept_Count : SAL.Base_Peek_Type := 0;
      Error_Count  : SAL.Base_Peek_Type := 0;
   begin
      for Parser_State of Shared_Parser.Parsers loop
         case Parser_State.Verb is
         when Shift =>
            Shift_Count := Shift_Count + 1;

         when Reduce =>
            Verb := Reduce;
            return;

         when Accept_It =>
            Accept_Count := Accept_Count + 1;

         when Error =>
            Error_Count := Error_Count + 1;

         when Pause =>
            --  This is parser_no_recover
            raise SAL.Programmer_Error;
         end case;
      end loop;

      if Shared_Parser.Parsers.Count = Accept_Count then
         Verb := Accept_It;

      elsif Shared_Parser.Parsers.Count = Error_Count then
         Verb := Error;

      elsif Shift_Count > 0 then
         Verb := Shift;

      else
         raise SAL.Programmer_Error;
      end if;
   end Parse_Verb;

   ----------
   --  Public subprograms, declaration order

   overriding procedure Finalize (Object : in out Parser)
   is begin
      Free_Table (Object.Table);
   end Finalize;

   procedure New_Parser
     (Parser             :    out LR.Parser_No_Recover.Parser;
      Trace              : in     WisiToken.Trace_Access;
      Lexer              : in     WisiToken.Lexer.Handle;
      Table              : in     Parse_Table_Ptr;
      User_Data          : in     WisiToken.Syntax_Trees.User_Data_Access)
   is begin
      Parser.Tree.Lexer := Lexer;
      Parser.Trace      := Trace;
      Parser.Table      := Table;
      Parser.User_Data  := User_Data;
   end New_Parser;

   overriding procedure Parse
     (Shared_Parser : in out Parser;
      Log_File      : in     Ada.Text_IO.File_Type;
      Edits         : in     KMN_Lists.List := KMN_Lists.Empty_List)
   is
      pragma Unreferenced (Log_File);

      use all type KMN_Lists.List;
      use all type WisiToken.Syntax_Trees.Terminal_Ref;
      use all type Syntax_Trees.User_Data_Access;

      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Current_Verb : All_Parse_Action_Verbs;
      Action       : Parse_Action_Node_Ptr;

      procedure Check_Error (Check_Parser : in out Parser_Lists.Cursor)
      is begin
         if Check_Parser.Verb = Error then
            --  This parser errored on last input. This is how grammar conflicts
            --  are resolved when the input text is valid, so we terminate this
            --  parser.

            if Shared_Parser.Parsers.Count = 1 then
               raise Syntax_Error;
            else
               Shared_Parser.Parsers.Terminate_Parser (Check_Parser, Shared_Parser.Tree, "", Shared_Parser.Trace.all);
            end if;
         else
            Check_Parser.Next;
         end if;
      end Check_Error;

   begin
      if Edits /= KMN_Lists.Empty_List then
         raise SAL.Programmer_Error;
      end if;

      if Shared_Parser.User_Data /= null then
         Shared_Parser.User_Data.Reset;
      end if;

      Shared_Parser.Tree.Clear;

      Shared_Parser.Lex_All;

      Shared_Parser.Parsers := Parser_Lists.New_List (Shared_Parser.Tree);

      Shared_Parser.Tree.Start_Parse
        (Shared_Parser.Parsers.First.State_Ref.Stream,
         Shared_Parser.Table.State_First);

      Main_Loop :
      loop
         --  exit on Accept_It action or syntax error.

         Parse_Verb (Shared_Parser, Current_Verb);

         case Current_Verb is
         when Shift =>
            --  All parsers just shifted a token, or we are just starting a parse;
            --  get the next token

            for Parser_State of Shared_Parser.Parsers loop
               if Parser_State.Shared_Token = Syntax_Trees.Invalid_Stream_Node_Ref then
                  Parser_State.Shared_Token := Shared_Parser.Tree.Stream_First (Shared_Parser.Tree.Shared_Stream);
               else
                  --  We don't support incremental parse, so Shared_Token is a terminal;
                  --  Stream_Next is the same as Next_Shared_Terminal.
                  Shared_Parser.Tree.Stream_Next (Parser_State.Shared_Token, Rooted => True);
               end if;
               Parser_State.Current_Token := Parser_State.Shared_Token;
            end loop;

         when Accept_It =>
            --  All parsers accepted.
            declare
               Count : constant SAL.Base_Peek_Type := Shared_Parser.Parsers.Count;
               State : Parser_Lists.Parser_State renames Shared_Parser.Parsers.First.State_Ref.Element.all;
            begin
               if Count = 1 then
                  --  Nothing more to do
                  if Trace_Parse > Outline then
                     Trace.Put_Line (" " & Shared_Parser.Tree.Trimmed_Image (State.Stream) & ": succeed");
                  end if;
                  exit Main_Loop;

               else
                  --  More than one parser is active; ambiguous parse.
                  raise WisiToken.Parse_Error with Shared_Parser.Tree.Error_Message
                    (State.Current_Token,
                     "Ambiguous parse:" & SAL.Base_Peek_Type'Image (Count) & " parsers active.");
               end if;
            end;

         when Reduce =>
            null;

         when Error =>
            --  All parsers errored; terminate with error. Do_Action reported the
            --  error, so we just raise the exception.
            raise Syntax_Error;

         when Pause =>
            --  This is parser_no_recover
            raise SAL.Programmer_Error;
         end case;

         --  We don't use 'for Parser_State of Parsers loop' here,
         --  because terminate on error and spawn on conflict require
         --  changing the parser list.
         declare
            Current_Parser : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
         begin
            loop
               exit when Current_Parser.Is_Done;

               if Current_Verb = Shift then
                  Shared_Parser.Parsers.Duplicate_State (Current_Parser, Shared_Parser.Tree, Shared_Parser.Trace.all);
                  --  If Duplicate_State terminated Current_Parser, Current_Parser now
                  --  points to the next parser. Otherwise it is unchanged.
               end if;

               exit when Current_Parser.Is_Done;

               if Trace_Parse > Extra then
                  Trace.Put_Line
                    ("current_verb: " & Image (Current_Verb) &
                       ", " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) &
                       ".verb: " & Parse_Action_Verbs'Image (Current_Parser.Verb));
               end if;

               --  Each branch of the following 'if' calls either Current_Parser.Free
               --  (which advances to the next parser) or Current_Parser.Next.

               if Current_Parser.Verb = Current_Verb then
                  declare
                     Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
                  begin
                     if Trace_Parse > Extra then
                        Trace.Put (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": stack: ");
                        Trace.Put_Line (Parser_Lists.Image (Parser_State.Stream, Shared_Parser.Tree));
                     end if;

                     Action := Action_For
                       (Table => Shared_Parser.Table.all,
                        State => Shared_Parser.Tree.State (Parser_State.Stream),
                        ID    => Shared_Parser.Tree.ID (Parser_State.Current_Token.Node));
                  end;

                  declare
                     Conflict : Parse_Action_Node_Ptr := Action.Next;
                  begin
                     loop
                        exit when Conflict = null;
                        --  Spawn a new parser (before modifying Current_Parser stack).

                        if Shared_Parser.Parsers.Count = Shared_Parser.Table.Max_Parallel then
                           declare
                              Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
                           begin
                              raise WisiToken.Parse_Error with Shared_Parser.Tree.Error_Message
                                (Parser_State.Shared_Token,
                                 ": too many parallel parsers required in grammar state" &
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
                                   (" " & Shared_Parser.Tree.Trimmed_Image (Current_Parser.Stream) & ":" &
                                      Shared_Parser.Tree.State (Parser_State.Stream)'Image & ": " &
                                      Shared_Parser.Tree.Image
                                        (Parser_State.Current_Token.Node, Terminal_Node_Numbers => True) & " : " &
                                      "spawn " & Shared_Parser.Tree.Next_Stream_ID_Trimmed_Image &
                                      ", (" & Trimmed_Image (1 + Integer (Shared_Parser.Parsers.Count)) & " active)");
                              end;
                           end if;

                           Shared_Parser.Parsers.Prepend_Copy
                             (Current_Parser, Shared_Parser.Tree, Shared_Parser.User_Data, Trace);
                           Do_Action (Conflict.Item, Shared_Parser.Parsers.First, Shared_Parser);

                           declare
                              Temp : Parser_Lists.Cursor := Shared_Parser.Parsers.First;
                           begin
                              Check_Error (Temp);
                           end;
                        end if;

                        Conflict := Conflict.Next;
                     end loop;
                  end;

                  Do_Action (Action.Item, Current_Parser, Shared_Parser);
                  Check_Error (Current_Parser);

               else
                  --  Current parser is waiting for others to catch up
                  Current_Parser.Next;
               end if;
            end loop;
         end;
      end loop Main_Loop;

      Shared_Parser.Tree.Clear_Parse_Streams;

      if Trace_Action > Extra then
         Shared_Parser.Trace.Put_Line
           (Shared_Parser.Tree.Image
              (Children     => True,
               Non_Grammar  => True,
               Augmented    => True,
               Line_Numbers => True));
         Shared_Parser.Trace.New_Line;
      end if;

      --  We don't raise Syntax_Error for lexer errors, since they are all
      --  recovered, either by inserting a quote, or by ignoring the
      --  character.
   end Parse;

   overriding procedure Execute_Actions
     (Parser              : in out LR.Parser_No_Recover.Parser;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region)
   is
      pragma Unreferenced (Action_Region_Bytes);

      use all type Syntax_Trees.User_Data_Access;
      procedure Process_Node
        (Tree : in out Syntax_Trees.Tree;
         Node : in     Syntax_Trees.Valid_Node_Access)
      is
         use Syntax_Trees;
      begin
         if Tree.Label (Node) /= Nonterm then
            return;
         end if;

         Parser.User_Data.Reduce (Tree, Node);

         declare
            use all type Syntax_Trees.Post_Parse_Action;
         begin
            if Tree.Action (Node) /= null then
               begin
                  Tree.Action (Node) (Parser.User_Data.all, Tree, Node);
               exception
               when E : others =>
                  if Trace_Tests > Outline then
                     --  running a unit test; exception may be AUnit assert fail
                     raise;

                  elsif WisiToken.Debug_Mode then
                     Parser.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
                     Parser.Trace.New_Line;
                  end if;

                  raise WisiToken.Parse_Error with Tree.Error_Message
                    (Node, "action raised exception " & Ada.Exceptions.Exception_Name (E) & ": " &
                       Ada.Exceptions.Exception_Message (E));
               end;
            end if;
         end;
      end Process_Node;

   begin
      if Parser.User_Data /= null then
         if Parser.Parsers.Count > 1 then
            raise Syntax_Error with "ambiguous parse; can't execute actions";
         end if;

         Parser.User_Data.Initialize_Actions (Parser.Tree);
         Parser.Tree.Process_Tree (Process_Node'Access);
      end if;
   end Execute_Actions;

end WisiToken.Parse.LR.Parser_No_Recover;
