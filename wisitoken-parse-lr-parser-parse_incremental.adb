--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);
separate (WisiToken.Parse.LR.Parser)
procedure Parse_Incremental
  (Shared_Parser : in out LR.Parser.Parser;
   Edits         : in     KMN_Lists.List)
is
   --  [Wagner Graham 1998] inc_parse, except we factored out edit_tree
   --  as in [lahav 2008]. Expanded to handle generalize parse for
   --  conflicts.
   --
   --  "la" in [Wagner Graham 1998] is the token after Stack_Top in the
   --  parse stream; here we have Current_Parser.State_Ref.Current_Token,
   --  which is in the parse stream if it is inserted by error recover,
   --  or if its the result of Right_Breakdown, otherwise in the shared
   --  stream.

   use Syntax_Trees;
   Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
   Tree  : Syntax_Trees.Tree renames Shared_Parser.Tree;
begin
   if Trace_Time then
      Trace.Put_Clock ("start");
   end if;

   Edit_Tree (Shared_Parser, Edits);

   Shared_Parser.Parsers := Parser_Lists.New_List (Shared_Parser.Tree);

   if Trace_Parse > Detail then
      Trace.New_Line;
      Trace.Put_Line ("edited stream:");
      Trace.Put_Line
        (Shared_Parser.Tree.Image
           (Non_Grammar => Trace_Parse > Extra,
            Children    => Trace_Parse > Extra));
      Trace.New_Line;
   end if;

   --  FIXME: Shared_Parser.String_Quote_Checked must be a line_array_boolean

   Shared_Parser.Tree.Start_Parse (Shared_Parser.Parsers.First.State_Ref.Stream, Shared_Parser.Table.State_First);

   declare
      Parser_State : Parser_Lists.Parser_State renames Shared_Parser.Parsers.First_State_Ref;
   begin
      Parser_State.Shared_Token := Tree.Stream_First (Tree.Shared_Stream);

      if Tree.ID (Parser_State.Shared_Token.Node) = Shared_Parser.Descriptor.Accept_ID then
         --  Parsed tree was not changed in Edit_Tree.
         Tree.Shift (Parser_State.Stream, Unknown_State, Parser_State.Shared_Token.Element, Shared_Parser.User_Data);
         Tree.Finish_Parse (Parser_State.Stream, Tree.Stream_Last (Tree.Shared_Stream), Shared_Parser.User_Data);
         return;
      end if;

      Parser_State.Current_Token := Parser_State.Shared_Token;
   end;

   declare
      --  Cursor requires initialization
      Current_Parser : constant Parser_Lists.Cursor := Shared_Parser.Parsers.First;
      --  FIXME: add loop over parsers; for now we abort on conflicts, errors

      Action : Parse_Action_Node_Ptr;
   begin
      Main_Loop :
      loop
         if Tree.Label (Current_Parser.State_Ref.Current_Token.Node) in Terminal_Label then
            declare
               Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
            begin
               Action := Action_For
                 (Table => Shared_Parser.Table.all,
                  State => Tree.State (Parser_State.Stream),
                  ID    => Tree.ID (Parser_State.Current_Token.Node));

               if Action.Next /= null then
                  raise SAL.Not_Implemented with "need generalized incremental parser";
               end if;

               Do_Action (Action.Item, Current_Parser, Shared_Parser);

               case Current_Parser.Verb is
               when Shift =>
                  --  FIXME: handle insert_delete from error recovery
                  Tree.Stream_Next (Tree.Shared_Stream, Parser_State.Shared_Token);
                  Parser_State.Current_Token := Parser_State.Shared_Token;

               when Accept_It =>
                  exit Main_Loop;

               when Pause | Reduce =>
                  null;

               when Error =>
                  --  FIXME: need error recovery
                  raise Syntax_Error;
               end case;
            end;
         else
            --  Current_Token.Element is a nonterminal. [Wagner Graham 1998] does
            --  Left_Breakdown here; we do that in Edit_Tree above.

            declare
               Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
               New_State    : constant Unknown_State_Index := Goto_For
                 (Shared_Parser.Table.all,
                  Tree.State (Parser_State.Stream, Tree.Stack_Top (Parser_State.Stream)),
                  Tree.ID (Tree.Shared_Stream, Parser_State.Current_Token.Element));
            begin
               pragma Assert
                 (Parser_State.Current_Token = Parser_State.Shared_Token and
                    Tree.Get_Node (Tree.Shared_Stream, Parser_State.Shared_Token.Element) =
                    Parser_State.Shared_Token.Node);

               --  [Wagner Graham 1998] has "if shiftable (la)" here; but 'shiftable'
               --  is not defined. Apparently it means Goto_For returns a valid state.
               if New_State /= Unknown_State then
                  Tree.Shift
                    (Parser_State.Stream, New_State, Parser_State.Current_Token.Element, Shared_Parser.User_Data);

                  if Trace_Parse > Detail then
                     Trace.Put_Line
                       --  Leading space for compatibility with existing tests.
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                          Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": " &
                          Shared_Parser.Tree.Image (Parser_State.Current_Token) & " : shift and goto state" &
                          New_State'Image);

                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": parse stream: " &
                             Tree.Image (Parser_State.Stream));
                     end if;
                  end if;

                  --  FIXME: if is_fragile then left_breakdown;

                  Tree.Stream_Next (Tree.Shared_Stream, Parser_State.Shared_Token);

                  Parser_State.Current_Token := Parser_State.Shared_Token;

               else
                  --  Current_Token nonterm is not immediately shiftable.

                  --  FIXME: First_Terminal traverses the tree, so need to optimize
                  --  this. [Wagner Graham 1998] suggests enhancing the parse table to
                  --  store reduce actions on nonterms as well as terminals, or storing
                  --  a first_terminal pointer in each node (but Ada LR1 is already
                  --  enormous). First_Shared_Terminal is _not_ correct; nonterms can
                  --  contain virtual terminals from previous error recovery.

                  --  FIXME: [Wagner Graham 1998] uses some
                  --  information about null productions cached in the parse table to
                  --  avoid calling Right_Breakdown while also avoiding finding
                  --  Tree.First_Terminal (next token).

                  declare
                     First_In_Current : constant Terminal_Ref := Tree.First_Terminal
                       (Tree.Shared_Stream, Parser_State.Shared_Token.Element);

                     Next_Terminal : constant Terminal_Ref :=
                       (if First_In_Current = Invalid_Terminal_Ref
                        then Tree.Next_Shared_Terminal (Tree.Shared_Stream, Parser_State.Shared_Token)
                        else First_In_Current);
                  begin
                     Action := Action_For
                       (Shared_Parser.Table.all,
                        Tree.State (Parser_State.Stream),
                        Tree.ID (Next_Terminal.Node));

                     case Action.Item.Verb is
                     when Accept_It =>
                        raise SAL.Programmer_Error;

                     when Reduce =>
                        --  We need to reduce the stack before we can shift Next_Terminal;
                        --  test_incremental.adb Edit_Code_2 shifting binary_adding_operator.
                        Do_Action (Action.Item, Current_Parser, Shared_Parser);

                     when Shift =>
                        --  Breakdown nonterms to allow shifting Next_Terminal.

                        --  FIXME: this affects all parsers; pause until all get here? or copy
                        --  tokens to Parse_Stream input, then breakdown; but that copies large
                        --  portions of input.

                        if Next_Terminal.Element = Parser_State.Shared_Token.Element then
                           Parser_State.Shared_Token.Node := Next_Terminal.Node;
                        else
                           loop
                              --  Shared_Token.element is empty; delete it
                              declare
                                 Temp : Stream_Index := Parser_State.Shared_Token.Element;
                              begin
                                 Tree.Stream_Next (Tree.Shared_Stream, Parser_State.Shared_Token);
                                 Tree.Stream_Delete (Tree.Shared_Stream, Temp);
                              end;

                              exit when Next_Terminal.Element = Parser_State.Shared_Token.Element;
                           end loop;
                        end if;

                        Tree.Left_Breakdown (Tree.Shared_Stream, Parser_State.Shared_Token);

                        Parser_State.Current_Token := Parser_State.Shared_Token;

                        if Parser_State.Current_Token = Invalid_Terminal_Ref then
                           --  FIXME: document use case
                           raise SAL.Not_Implemented;
                           --  FIXME: need error recovery
                           --  raise Syntax_Error with "left_breakdown returned Invalid_Stream_Index";
                        end if;

                     when Error =>
                        --  Next_Terminal is not shiftable either.

                        if Tree.Label (Parser_State.Stream) in Terminal_Label then
                           raise Syntax_Error with "need error_recovery";
                        else
                           --  [Wagner Graham 1998] has Right_Breakdown here, but that is
                           --  overkill; we only need Undo_Reduce until Current_Token is
                           --  shiftable. We don't loop on that locally; Main_Loop does the right
                           --  things.
                           --
                           --  IMPROVEME: if error recovery is correct here, we do more
                           --  Undo_Reduce than necessary (and it will never happen in
                           --  Error_Recovery).

                           Tree.Undo_Reduce (Parser_State.Stream);
                        end if;
                     end case;
                  end;
               end if;
            end;
         end if;
      end loop Main_Loop;
   end;
exception
when Syntax_Error | WisiToken.Parse_Error | Partial_Parse =>
   if Trace_Time then
      Trace.Put_Clock ("finish - error");
   end if;
   raise;

when E : others =>
   declare
      Msg : constant String := Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E);
   begin
      if Shared_Parser.Parsers.Count > 0 then
         --  Emacs displays errors in the *syntax-errors* buffer
         Shared_Parser.Parsers.First_State_Ref.Errors.Append
           ((Label          => LR.Message,
             First_Terminal => Shared_Parser.Descriptor.First_Terminal,
             Last_Terminal  => Shared_Parser.Descriptor.Last_Terminal,
             Recover        => <>,
             Msg            => +Msg));
      end if;

      if Debug_Mode then
         Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E)); -- includes Prefix
         Trace.New_Line;
      end if;

      --  Emacs displays the exception message in the echo area; easy to miss
      raise WisiToken.Parse_Error with Msg;
   end;
end Parse_Incremental;
