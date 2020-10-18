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
   Trace : WisiToken.Trace'Class renames Shared_Parser.Trace.all;
begin
   if Trace_Time then
      Trace.Put_Clock ("start");
   end if;

   Shared_Parser.Parsers := Parser_Lists.New_List (Shared_Parser.Tree);

   Edit_Tree (Shared_Parser, Edits);

   if Trace_Incremental_Parse > Detail then
      Trace.New_Line;
      Trace.Put_Line ("edited tree:");
      Trace.Put_Line
        (Shared_Parser.Tree.Image
           (Non_Grammar => Trace_Incremental_Parse > Extra,
            Children    => Trace_Incremental_Parse > Extra));
      Trace.New_Line;
   end if;

   --  FIXME: Shared_Parser.String_Quote_Checked must be a line_array_boolean

   declare
      --  [Wagner Graham 1998] inc_parse, except we factored out edit_tree
      --  as in [lahav 2008]. Expanded to handle generalize parse for
      --  conflicts.

      use Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Shared_Parser.Tree;

      Current_Parser : constant Parser_Lists.Cursor := Shared_Parser.Parsers.First;
      --  "la" in [Wagner Graham 1998] is
      --  Current_Parser.State_Ref.Current_Token, which is the token after
      --  Stack_Top in the parse stream.

      Action : Parse_Action_Node_Ptr;
   begin
      declare
         Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
      begin
         Parser_State.Current_Token := Tree.Stream_First (Parser_State.Stream); -- state 0, invalid node
         Shared_Parser.Tree.Set_Stack_Top (Parser_State.Stream, Parser_State.Current_Token);
         Parser_State.Current_Token := Tree.Stream_Next (Parser_State.Stream, @);

         if Tree.ID (Parser_State.Stream, Parser_State.Current_Token) = Shared_Parser.Descriptor.Accept_ID then
            --  Parsed tree was not changed in Edit_Tree.
            Tree.Set_Stack_Top (Parser_State.Stream, Parser_State.Current_Token);
            return;
         end if;
      end;

      Main_Loop :
      loop
         --  FIXME: add loop over parsers; for now we abort on conflicts, errors

         if Tree.Label (Current_Parser.State_Ref.Current_Token) in Terminal_Label then
            --  FIXME: handle insert_delete from error recovery
            declare
               Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref;
            begin
               Action := Action_For
                 (Table => Shared_Parser.Table.all,
                  State => Tree.State (Parser_State.Stream),
                  ID    => Tree.ID (Parser_State.Stream, Parser_State.Current_Token));

               if Action.Next /= null then
                  raise SAL.Not_Implemented with "need generalized incremental parser";
               end if;

               Do_Action (Action.Item, Current_Parser, Shared_Parser);

               case Current_Parser.Verb is
               when Shift =>
                  Parser_State.Current_Token := Tree.Stream_Next (Parser_State.Stream, @);

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
            --  Current_Token is a nonterminal. [Wagner Graham 1998] does
            --  Left_Breakdown here; we do that in Edit_Tree above.

            declare
               Parser_State : Parser_Lists.Parser_State renames Current_Parser.State_Ref.Element.all;
            begin
               --  [Wagner Graham 1998] has "if shiftable (la)" here; but 'shiftable'
               --  is not defined. Apparently it means In_Goto.
               if In_Goto
                 (Shared_Parser.Table.all, Tree.State (Parser_State.Stream),
                  Tree.ID (Parser_State.Stream, Parser_State.Current_Token))
               then
                  if Trace_Parse > Detail then
                     Trace.Put
                       --  Leading space for compatibility with existing tests.
                       (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": " &
                          (if Trace_Parse_No_State_Numbers
                           then "-- : "
                           else Trimmed_Image (Shared_Parser.Tree.State (Parser_State.Stream)) & ": ") &
                          Shared_Parser.Tree.Image (Parser_State.Current_Token, Terminal_Node_Numbers => True) & " : ");
                     Trace.Put (" shift, right_breakdown");
                  end if;
                  Tree.Shift_Nonterm (Parser_State.Stream, Parser_State.Current_Token);

                  --  FIXME: only need to do Right_Breakdown if next token is not
                  --  shiftable; move this up?. [Wagner Graham 1998] uses some information about null
                  --  productions cached in the parse table to avoid calling
                  --  Right_Breakdown while also avoiding finding Tree.First_Terminal
                  --  (next token). However, we cache First_Shared_Terminal in the tree;
                  --  is that good enough? Keep the simplest algorithm for now.
                  Tree.Right_Breakdown (Parser_State.Stream);
                  Parser_State.Current_Token := Tree.Stream_Next
                    (Parser_State.Stream, Tree.Stack_Top (Parser_State.Stream));
                  --  FIXME: if is_fragile then left_breakdown;

                  if Trace_Parse > Detail then
                     Trace.Put_Line (" and goto state" & Tree.State (Parser_State.Stream)'Image);
                     if Trace_Parse > Extra then
                        Trace.Put_Line
                          (" " & Shared_Parser.Tree.Trimmed_Image (Parser_State.Stream) & ": parse stream: " &
                             Tree.Image (Parser_State.Stream));
                     end if;
                  end if;
               else
                  --  We might need to reduce the stack before we can shift the next
                  --  nonterm; test_incremental.adb Edit_Code_2 shifting
                  --  binary_adding_operator.
                  --
                  --  FIXME: First_Terminal traverses the tree, so need to optimize
                  --  this. [Wagner Graham 1998] suggests enhancing the parse table to
                  --  store reduce actions on nonterms as well as terminals, or storing
                  --  a first_terminal pointer in each node.
                  Action := Action_For
                    (Shared_Parser.Table.all,
                     Tree.State (Parser_State.Stream),
                     Tree.ID (Tree.First_Terminal (Tree.Get_Node (Parser_State.Stream, Parser_State.Current_Token))));

                  if Action.Item.Verb = Reduce then
                     Do_Action (Action.Item, Current_Parser, Shared_Parser);
                  else
                     Parser_State.Current_Token := Tree.Left_Breakdown_Parse
                       (Parser_State.Stream, Parser_State.Current_Token);
                  end if;
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
