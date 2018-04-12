--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2018 Stephen Leake All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Task_Identification;
with System.Multiprocessors;
with WisiToken.LR.McKenzie_Recover.Base;
with WisiToken.LR.McKenzie_Recover.Explore;
with WisiToken.LR.Parser_Lists;
package body WisiToken.LR.McKenzie_Recover is

   procedure Put
     (Message      : in     String;
      Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Terminals    : in     Base_Token_Arrays.Vector;
      Config       : in     Configuration;
      Task_ID      : in     Boolean := True)
   is
      --  For debugging output

      --  Build a string, call trace.put_line once, so output from multiple
      --  tasks is not interleaved (mostly).
      use all type Ada.Containers.Count_Type;
      use all type Ada.Strings.Unbounded.Unbounded_String;
      use all type SAL.Base_Peek_Type;
      use all type WisiToken.Semantic_Checks.Check_Status_Label;

      Descriptor : WisiToken.Descriptor renames Trace.Descriptor.all;

      Result : Ada.Strings.Unbounded.Unbounded_String :=
        (if Task_ID then +Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else +"") &
        Integer'Image (Parser_Label) & ": " &
        (if Message'Length > 0 then Message & ":" else "");
   begin
      Result := Result & Natural'Image (Config.Cost) & ", ";
      if Config.Check_Status.Label /= Ok then
         Result := Result & Semantic_Checks.Check_Status_Label'Image (Config.Check_Status.Label) & " ";
      elsif Config.Error_Token.ID /= Invalid_Token_ID then
         Result := Result & "Error " & Image (Config.Error_Token, Descriptor) & " ";
      end if;
      Result := Result & Image (Config.Stack, Descriptor, Depth => 1);

      if Config.Current_Inserted = No_Inserted then
         Result := Result & "|" & Image (Config.Current_Shared_Token, Terminals, Descriptor) & "|";
      else
         Result := Result & "/" & Image (Config.Current_Inserted, Config.Inserted, Descriptor) & "/";
      end if;

      Result := Result & Image (Config.Ops, Descriptor);
      Trace.Put_Line (-Result);
   end Put;

   procedure Put_Line
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Message      : in     String;
      Task_ID      : in     Boolean := True)
   is
      use all type Ada.Strings.Unbounded.Unbounded_String;
   begin
      Trace.Put_Line
        ((if Task_ID then Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) else "") &
           Integer'Image (Parser_Label) & ": " & Message);
   end Put_Line;

   function Undo_Reduce
     (Stack : in out Recover_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
     return Ada.Containers.Count_Type
   is
      Nonterm_Item : constant Recover_Stack_Item                  := Stack.Pop;
      Children     : constant Syntax_Trees.Valid_Node_Index_Array := Tree.Children (Nonterm_Item.Tree_Index);
   begin
      for C of Children loop
         Stack.Push ((Tree.State (C), C, Tree.Recover_Token (C)));
      end loop;
      return Children'Length;
   end Undo_Reduce;

   procedure Undo_Reduce
     (Stack : in out Parser_Lists.Parser_Stacks.Stack;
      Tree  : in     Syntax_Trees.Tree)
   is
      Item : constant Parser_Lists.Parser_Stack_Item := Stack.Pop;
   begin
      for C of Tree.Children (Item.Token) loop
         Stack.Push ((Tree.State (C), C));
      end loop;
   end Undo_Reduce;

   task type Worker_Task
     (Super  : not null access Base.Supervisor;
      Shared : not null access Base.Shared_Lookahead)
   is
      entry Start;
      --  Start getting parser/configs to check from Config_Store.

      entry Done;
      --  Available when task is ready to terminate; after this rendezvous,
      --  task discriminants may be freed.

   end Worker_Task;

   task body Worker_Task
   is
      use all type Base.Config_Status;
      Status : Base.Config_Status;
   begin
      accept Start;

      loop
         Explore.Process_One (Super, Shared, Status);

         exit when Status = All_Done;
      end loop;

      accept Done;
   exception
   when E : others =>
      Super.Fatal (E);
   end Worker_Task;

   function To_Recover
     (Parser_Stack : in Parser_Lists.Parser_Stacks.Stack;
      Tree         : in Syntax_Trees.Tree)
     return Recover_Stacks.Stack
   is
      use all type SAL.Base_Peek_Type;
      Result : Recover_Stacks.Stack;
      Depth  : constant SAL.Peek_Type := Parser_Stack.Depth;
   begin
      Result.Set_Depth (Depth);
      for I in 1 .. Depth loop
         declare
            Item  : Parser_Lists.Parser_Stack_Item renames Parser_Stack (I);
            Token : constant Recover_Token := (if I = Depth then (others => <>) else Tree.Recover_Token (Item.Token));
         begin
            Result.Set (I, Depth, (Item.State, Item.Token, Token));
         end;
      end loop;
      return Result;
   end To_Recover;

   procedure Recover_Init
     (Shared_Parser : in out LR.Parser.Parser;
      Parser_State  : in out Parser_Lists.Parser_State)
   is
      use all type SAL.Base_Peek_Type;

      Trace  : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
      Config : constant Configuration_Access := Parser_State.Recover.Config_Heap.Add (Configuration'(others => <>));
      Error  : Parse_Error renames Parser_State.Errors (Parser_State.Errors.Last);
   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line
           ("parser" & Integer'Image (Parser_State.Label) &
              ": State" & State_Index'Image (Parser_State.Stack (1).State) &
              " Current_Token" & Parser_State.Tree.Image (Parser_State.Current_Token, Trace.Descriptor.all));
         Trace.Put_Line (Image (Error, Parser_State.Tree, Trace.Descriptor.all));
         if Trace_McKenzie > Extra then
            Put_Line
              (Trace, Parser_State.Label, Parser_Lists.Image
                 (Parser_State.Stack, Trace.Descriptor.all, Parser_State.Tree));
         end if;
      end if;

      --  Additional initialization of Parser_State.Recover is done in
      --  Supervisor.Initialize.

      Config.Stack := To_Recover (Parser_State.Stack, Parser_State.Tree);

      --  Parser_State.Recover_Insert_Delete must be empty (else we would not get
      --  here). Therefore Parser_State current token is in
      --  Shared_Parser.Shared_Token.

      Config.Current_Shared_Token := Parser_State.Shared_Token;

      case Error.Label is
      when Action =>
         Config.Error_Token := Parser_State.Tree.Recover_Token (Error.Error_Token);
         if Trace_McKenzie > Detail then
            Put ("enqueue", Trace, Parser_State.Label, Shared_Parser.Terminals, Config.all,
                 Task_ID => False);
         end if;

      when Check =>
         --  Undo the reduction that encountered the error, let Process_One
         --  enqueue possible solutions. We leave the cost at 0, because this
         --  is the root config.

         Config.Check_Status      := Error.Check_Status;
         Config.Error_Token       := Config.Stack (1).Token;
         Config.Check_Token_Count := Undo_Reduce (Config.Stack, Parser_State.Tree);

         Config.Ops.Append ((Undo_Reduce, Config.Error_Token.ID, Config.Check_Token_Count));

         if Trace_McKenzie > Detail then
            Put ("undo_reduce " & Image
                   (Config.Error_Token.ID, Trace.Descriptor.all), Trace, Parser_State.Label,
                 Shared_Parser.Terminals, Config.all, Task_ID => False);
         end if;
      end case;

      Parser_State.Recover.Enqueue_Count := Parser_State.Recover.Enqueue_Count + 1;
   end Recover_Init;

   function Recover (Shared_Parser : in out LR.Parser.Parser) return Recover_Status
   is
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;
      Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;

      Parsers : Parser_Lists.List renames Shared_Parser.Parsers;

      Super : aliased Base.Supervisor
        (Trace'Access,
         Parsers'Access,
         Shared_Parser.Terminals'Access,
         Cost_Limit   => Shared_Parser.Table.McKenzie_Param.Cost_Limit,
         Parser_Count => Parsers.Count);

      Shared : aliased Base.Shared_Lookahead (Shared_Parser'Access);

      Worker_Tasks   : array (1 .. System.Multiprocessors.Number_Of_CPUs - 1) of Worker_Task
        (Super'Access, Shared'Access);
      --  Keep one CPU free for this main task, and the user.
      --  FIXME: see if more tasks go faster or slower.

      Shared_Verb : All_Parse_Action_Verbs := Shift;
      --  When there are multiple parsers, this is the 'worst case' verb.
      --  Verbs are worse in the order Shift, Shift_Reduce, Reduce.

      procedure Update_Shared_Verb (Parser_Verb : in All_Parse_Action_Verbs)
      is begin
         case Parser_Verb is
         when Shift =>
            return;
         when Shift_Recover =>
            if Shared_Verb = Shift then
               Shared_Verb := Shift_Recover;
            end if;
         when Reduce =>
            Shared_Verb := Reduce;
         when others =>
            raise Programmer_Error;
         end case;
      end Update_Shared_Verb;


      procedure Cleanup
      is begin
         for I in Worker_Tasks'Range loop
            if Worker_Tasks (I)'Callable then
               abort Worker_Tasks (I);
            end if;
         end loop;
      end Cleanup;

   begin
      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (" McKenzie error recovery");
      end if;

      Super.Initialize;

      for Parser_State of Parsers loop
         Recover_Init (Shared_Parser, Parser_State);
      end loop;

      if Trace_McKenzie > Outline then
         Trace.New_Line;
         Trace.Put_Line (System.Multiprocessors.CPU_Range'Image (Worker_Tasks'Last) & " parallel tasks");
      end if;

      for I in Worker_Tasks'Range loop
         Worker_Tasks (I).Start;
      end loop;

      declare
         use Ada.Exceptions;
         ID : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Super.Done (ID, Message); -- Wait for all parsers to fail or succeed
         if ID /= Null_Id then
            Raise_Exception (ID, -Message);
         end if;
      end;

      --  Ensure all tasks terminate before proceeding; otherwise local
      --  variables disappear while task is still trying to access them.
      for I in Worker_Tasks'Range loop
         if Worker_Tasks (I)'Callable then
            Worker_Tasks (I).Done;
         end if;
      end loop;

      --  Adjust parser state for each successful recovery.
      --
      --  One option here would be to keep only the parser with the least
      --  cost fix. However, the normal reason for having multiple parsers
      --  is to resolve a grammar ambiguity; the least cost fix might
      --  resolve the ambiguity the wrong way. As could any other fix, of
      --  course.

      --  Spawn new parsers for multiple solutions
      declare
         use Parser_Lists;
         Cur : Cursor := Parsers.First;
      begin
         loop
            declare
               Data : McKenzie_Data renames State_Ref (Cur).Recover;
            begin
               if Data.Success then
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (Integer'Image (Label (Cur)) &
                          ": succeed" & SAL.Base_Peek_Type'Image (Data.Results.Count) &
                          ", enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost: " & Integer'Image (Data.Results.Min_Key));
                  end if;

                  --  Restore pre-error parser verb before spawning, just for
                  --  simplicity.
                  Cur.Set_Verb (Cur.Prev_Verb);
                  Update_Shared_Verb (Cur.Prev_Verb);

                  if Data.Results.Count > 1 then
                     if Parsers.Count + Data.Results.Count > Shared_Parser.Max_Parallel then
                        raise WisiToken.Parse_Error with Error_Message
                          ("", Shared_Parser.Lexer.Line, Shared_Parser.Lexer.Column,
                           ": too many parallel parsers required in grammar state" &
                             State_Index'Image (Cur.State_Ref.Stack.Peek.State) &
                             "; simplify grammar, or increase max-parallel (" &
                             SAL.Base_Peek_Type'Image (Shared_Parser.Max_Parallel) & ")");
                     end if;

                     for I in 1 .. Data.Results.Count - 1 loop
                        Parsers.Prepend_Copy (Cur); --  does not copy recover
                        if Trace_McKenzie > Outline then
                           Trace.Put_Line
                             ("spawn parser" & Integer'Image (Parsers.First.Label) & " from " &
                                Int_Image (Cur.Label) & " (" & Int_Image (Integer (Parsers.Count)) & " active)");
                           Put ("", Trace, Parsers.First.Label, Shared_Parser.Terminals,
                                Data.Results.Peek, Task_ID => False);
                        end if;

                        State_Ref (Parsers.First).Recover.Results.Add (Data.Results.Remove);
                        State_Ref (Parsers.First).Recover.Success := True;
                     end loop;
                  end if;

                  if Trace_McKenzie > Outline then
                     Put ("", Trace, Cur.State_Ref.Label, Shared_Parser.Terminals, Data.Results.Peek,
                          Task_ID => False);
                  end if;
               else
                  if Trace_McKenzie > Outline then
                     Trace.Put_Line
                       (Integer'Image (Cur.Label) &
                          ": fail, enqueue" & Integer'Image (Data.Enqueue_Count) &
                          ", check " & Integer'Image (Data.Check_Count) &
                          ", cost_limit: " & Integer'Image (Shared_Parser.Table.McKenzie_Param.Cost_Limit));
                  end if;
               end if;

            end;
            Next (Cur);
            exit when Is_Done (Cur);
         end loop;
      end;

      --  Edit Parser_State to apply solutions.
      for Parser_State of Parsers loop
         if Parser_State.Recover.Success then
            declare
               use all type Ada.Containers.Count_Type;
               use all type Syntax_Trees.Node_Index;
               use Parser_Lists;

               Descriptor : WisiToken.Descriptor renames Shared_Parser.Trace.Descriptor.all;
               Tree       : Syntax_Trees.Tree renames Parser_State.Tree;
               Data       : McKenzie_Data renames Parser_State.Recover;
               Result     : Configuration renames Data.Results.Peek;

               Shared_Token_Changed : Boolean := False;
               Virtuals_Inserted    : Boolean := False;
               Fast_Forward_Seen    : Boolean := False;
               Insert_Seen          : Boolean := False;
            begin
               --  We apply Push_Back ops to Parser_State.Stack up to the first
               --  Fast_Forward, and enqueue Insert and Delete ops on
               --  Parser_State.Recover_Insert_Delete. Then the main parser parses
               --  the edited input stream.
               --
               --  There's no need to modify Parser_State.Tree. The tree nodes
               --  created by the failed parse are useful for error repair, and will
               --  just be ignored in future parsing. This also avoids enlarging a
               --  non-flushed branched tree, which saves time and space.

               if Trace_McKenzie > Extra then
                  Put_Line (Trace, Parser_State.Label, "before Ops applied:", Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser stack " & Image (Parser_State.Stack, Descriptor, Tree),
                     Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser Shared_Token  " & Image
                       (Parser_State.Shared_Token, Shared_Parser.Terminals, Descriptor),
                     Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser Current_Token " & Parser_State.Tree.Image
                       (Parser_State.Current_Token, Descriptor),
                     Task_ID => False);
               end if;

               for Op of Result.Ops loop
                  case Op.Op is
                  when Fast_Forward =>
                     Fast_Forward_Seen := True;

                  when Undo_Reduce =>
                     if Fast_Forward_Seen then
                        --  The real parser stack will not be in this state, so we don't need
                        --  to do anything with this.
                        null;
                     else
                        Undo_Reduce (Parser_State.Stack, Tree);
                     end if;

                  when Push_Back =>
                     if Fast_Forward_Seen then
                        --  The real parser stack will not be in this state, so we don't need
                        --  to do anything with this.
                        null;
                     else
                        Parser_State.Stack.Pop;
                        if Op.Token_Index /= Invalid_Token_Index then
                           Parser_State.Shared_Token := Op.Token_Index;
                        end if;
                        Shared_Token_Changed := True;
                     end if;

                  when Insert =>
                     Insert_Seen := True;
                     Parser_State.Recover_Insert_Delete.Put (Op);
                     Virtuals_Inserted := True;

                  when Delete =>
                     if not Insert_Seen and then Op.Token_Index = Parser_State.Shared_Token then
                        pragma Assert (not Fast_Forward_Seen);
                        Parser_State.Shared_Token := Op.Token_Index + 1;
                     else
                        Parser_State.Recover_Insert_Delete.Put (Op);
                     end if;
                     Shared_Token_Changed := True;

                  end case;
               end loop;

               if Virtuals_Inserted then
                  declare
                     Op : constant Config_Op := Parser_State.Recover_Insert_Delete.Peek;
                  begin
                     if Op.Op = Insert and then Op.Token_Index <= Parser_State.Shared_Token then
                        --  "<" because Op.Token_Index may have been deleted.
                        Parser_State.Recover_Insert_Delete.Drop;

                        Parser_State.Current_Token := Parser_State.Tree.Add_Terminal (Op.ID);
                     end if;
                  end;
               end if;

               if Shared_Token_Changed and not Virtuals_Inserted then
                  Parser_State.Current_Token := Parser_State.Tree.Add_Terminal
                    (Parser_State.Shared_Token, Shared_Parser.Terminals);
               end if;

               if Shared_Token_Changed or Virtuals_Inserted then
                  Parser_State.Set_Verb (Shift_Recover);
                  Update_Shared_Verb (Shift_Recover);
               end if;

               if Virtuals_Inserted or (Shared_Token_Changed and Parser_State.Verb = Reduce) then
                  --  On exit from McKenzie_Recover, Verb gives the action that will be
                  --  performed immediately, if it is the same as Shared_Verb.
                  --  Shared_Verb is checked in a separate loop below.
                  --
                  --  If Verb is Reduce, the main parser will do reduce until it gets to
                  --  Shift. At that point, it would normally increment
                  --  Parser_State.Shared_Terminal to get to the next token. However, we have
                  --  set Shared_Terminal to the next token, so we don't want it to increment.
                  --  We could set Shared_Terminal to 1 less, but this way the debug messages
                  --  all show the expected shared_terminal.
                  --
                  --  If Verb is Shift or Shift_Recover, then Shared_Token will be
                  --  shifted immediately, and we want it to increment on the next
                  --  shift.
                  Parser_State.Inc_Shared_Token := False;
               else
                  Parser_State.Inc_Shared_Token := True;
               end if;

               Parser_State.Errors (Parser_State.Errors.Last).Recover := Result;

               if Trace_McKenzie > Extra then
                  Put_Line (Trace, Parser_State.Label, "after Ops applied:", Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser stack " & Image (Parser_State.Stack, Descriptor, Tree),
                     Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser Shared_Token  " & Image
                       (Parser_State.Shared_Token, Shared_Parser.Terminals, Descriptor), Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser Current_Token " & Parser_State.Tree.Image
                       (Parser_State.Current_Token, Descriptor), Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser recover_insert_delete " & Image
                       (Parser_State.Recover_Insert_Delete, Descriptor), Task_ID => False);
                  Put_Line
                    (Trace, Parser_State.Label, "parser inc_shared_token " & Boolean'Image
                       (Parser_State.Inc_Shared_Token) & " parser verb " & All_Parse_Action_Verbs'Image
                         (Parser_State.Verb), Task_ID => False);
               end if;
            end;
         end if;
      end loop;

      for Parser_State of Parsers loop
         if Parser_State.Verb /= Shared_Verb then
            --  Shared_Token will not be shifted immediately.
            Parser_State.Inc_Shared_Token := False;
         end if;
      end loop;

      Cleanup;

      return Super.Recover_Result;

   exception
   when others =>
      Cleanup;
      raise;
   end Recover;

   ----------
   --  Spec private subprograms not declared above; for language-specific
   --  child packages.

   procedure Find_ID
     (Config         : in     Configuration;
      IDs            : in     Token_ID_Set;
      Matching_Index : in out SAL.Peek_Type)
   is
      use all type SAL.Peek_Type;
   begin
      loop
         exit when Matching_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            ID : Token_ID renames Config.Stack (Matching_Index).Token.ID;
         begin
            exit when ID in IDs'First .. IDs'Last and then IDs (ID);
         end;
         Matching_Index := Matching_Index + 1;
      end loop;
   end Find_ID;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      loop
         exit when Matching_Name_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token : Recover_Token renames Config.Stack (Matching_Name_Index).Token;
         begin
            exit when Token.Name /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Token.Name))
               else Lexer.Buffer_Text (Token.Name));

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_ID            : in     Token_ID;
      Other_Count         :    out Integer;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Count := 0;

      loop
         exit when Matching_Name_Index = Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token : Recover_Token renames Config.Stack (Matching_Name_Index).Token;
         begin
            exit when Token.Name /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Lexer.Buffer_Text (Token.Name))
               else Lexer.Buffer_Text (Token.Name));

            if Other_ID = Token.ID then
               Other_Count := Other_Count + 1;
            end if;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

end WisiToken.LR.McKenzie_Recover;
