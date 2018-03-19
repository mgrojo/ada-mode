--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Characters.Handling;
with Ada_Lite;
package body WisiToken.LR.McKenzie_Recover.Ada_Lite is

   use all type Standard.Ada_Lite.Token_Enum_ID; -- token names

   Descriptor : WisiToken.Descriptor renames Standard.Ada_Lite.Descriptor;

   subtype Token_ID_Set is WisiToken.Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal);

   --  Status.Tokens (1) is <begin_name_token>, (2) <end_name_token>.
   --
   --  From ada_lite.wy, <begin_name_token>, <end_name_token> are one of:
   --  block_label_opt           identifier_opt in block_statement
   --  name                      name_opt       in package_body
   --  subprogram_specification  name_opt       in subprogram_body

   Begin_Name_IDs : constant Token_ID_Set := To_Token_ID_Set
     ((+block_label_opt_ID & (+name_ID) & (+subprogram_specification_ID)), Descriptor);

   End_Name_IDs : constant Token_ID_Set := To_Token_ID_Set ((+identifier_opt_ID & (+name_opt_ID)), Descriptor);

   Nonterm_IDs : constant Token_ID_Set := To_Token_ID_Set
     ((+block_statement_ID & (+package_body_ID) & (+subprogram_body_ID)), Descriptor);

   Begin_IDs : constant Token_ID_Set := To_Token_ID_Set ((1 => +BEGIN_ID), Descriptor);

   type Token_ID_Set_Array is array (Positive range <>) of Token_ID_Set;
   type Natural_Array is array (Positive range <>) of Natural;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_IDs           : in     Token_ID_Set_Array;
      Other_Counts        :    out Natural_Array;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      use all type SAL.Peek_Type;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Counts := (others => 0);

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

            for I in Other_IDs'Range loop
               if Other_IDs (I)(Token.ID) then
                  Other_Counts (I) := Other_Counts (I) + 1;
               end if;
            end loop;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   ----------
   --  Public subprograms

   function Semantic_Check_Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : in     WisiToken.Lexer.Handle;
      Parser_Label      : in     Natural;
      McKenzie_Param    : in     McKenzie_Param_Type;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Branched.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration;
      Action            : in     Reduce_Action_Rec;
      Nonterm           : in     Recover_Token;
      Status            : in     Semantic_Checks.Error_Check_Status)
     return Boolean
   is
      --  A reduce on Config failed a semantic check; Config.Stack is in the
      --  pre-reduce state, Nonterm is the result of the reduce. Enqueue
      --  fixes for the failure indicated by Code. Return True if Config
      --  (which ignores the error) is a viable solution, False otherwise.

      pragma Unreferenced (McKenzie_Param); --  we may add costs at some point

      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      procedure Put (Message : in String; Config  : in Configuration)
      is begin
         Put (Message, Trace, Parser_Label, Terminals, Config);
      end Put;

      procedure Append_Check
        (Ops         : in out Config_Op_Arrays.Vector;
         Op          : in     Config_Op_Label;
         ID          : in     Token_ID;
         Expected_ID : in     Token_ID)
      is

      begin
         if ID /= Expected_ID then
            raise Programmer_Error with "semantic_check_fixes: got " & Image (ID, Descriptor) &
              "expected " & Image (Expected_ID, Descriptor);
         end if;
         Ops.Append ((Op, ID));
      end Append_Check;

      Begin_Name_Token : Recover_Token renames Status.Tokens (Status.Tokens.First_Index);
      End_Token : Recover_Token renames Status.Tokens (Status.Tokens.Last_Index);
   begin
      if Trace_McKenzie > Detail then
         Put_Line (Trace, Parser_Label, "Ada_Lite Semantic_Check_Fixes");
      end if;

      if not (Begin_Name_IDs (Begin_Name_Token.ID) and
                End_Name_IDs (End_Token.ID) and
                Nonterm_IDs (Nonterm.ID))
      then
         raise Programmer_Error with "unregognized begin/end/nonterm token id";
      end if;

      case Status.Code is
      when Semantic_Checks.Match_Names_Error =>
         --  0. User name error. The input looks like:
         --
         --  "<begin_name_token> ... <end_name_token> ;"
         --
         --  where the names do not match, because the user is changing them.
         --
         --  The fix is to ignore the error; return True. See
         --  propagate_names.ada_lite Proc_1.
         --
         --  1. The mismatch indicates one or more missing 'end's. The input
         --  looks like:
         --
         --  "<correct_begin_name_token> ... <bad_begin_name_token> ... <end_name_token> ;"
         --
         --  where <correct_begin_name_token> matches <end_name_token>, but
         --  <bad_begin_name_token> does not.
         --
         --  The fix is to insert one or more 'end ;' before <end_name_token>.
         --  See propagate_names.ada_lite Proc_2, Proc_3.
         --
         --
         --  It is not possible for the mismatch to indicate an extra 'end';
         --  that would generate either a Missing_Name_Error, or a syntax
         --  error.
         --
         --  To distinguish between case 0 and 1, we search the stack for
         --  unpaired 'begin' tokens; one will be <bad_begin_name_token>. If
         --  one or more, it's case 0, otherwise case 1.

         declare
            End_Name : constant String := Lexer.Buffer_Text (End_Token.Name);

            Matching_Name_Index : SAL.Peek_Type   := 2; -- start search before <end_name_token>
            Begin_Count         : Integer         := 0;
         begin
            Find_Matching_Name
              (Config, Lexer, End_Name, Matching_Name_Index, +BEGIN_ID, Begin_Count, Case_Insensitive => True);

            if Begin_Count = 0 then
               --  No 'begin's; case 0. Matching_Name_Index is > Config.Stack.Depth.
               return True;

            else
               --  Case 1.
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
                  Stack      : Recover_Stacks.Stack renames New_Config.Stack;
                  Item       : Recover_Stack_Item;
               begin
                  --  This is a not a guess, so it has zero cost.
                  --
                  --  First push back thru 'end'.

                  Ops.Append ((Undo_Reduce, Nonterm.ID)); -- the failed reduce

                  Append_Check (Ops, Push_Back, Stack.Pop.Token.ID, +SEMICOLON_ID);

                  Append_Check
                    (Ops, Push_Back, Stack.Pop.Token.ID,
                     (if Nonterm.ID = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID));

                  Item := Stack.Pop;
                  Append_Check (Ops, Push_Back, Item.Token.ID, +END_ID);

                  New_Config.Current_Shared_Token := Item.Token.Min_Terminal_Index;

                  --  Insert 'end ;' for each extra 'begin'
                  for I in 1 .. Begin_Count loop
                     Ops.Append ((Insert, +END_ID));
                     Ops.Append ((Insert, +SEMICOLON_ID));

                     New_Config.Inserted.Append (+END_ID);
                     New_Config.Inserted.Append (+SEMICOLON_ID);
                  end loop;
                  New_Config.Current_Inserted := 1;

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Match_Names_Error 1 " & Image (Nonterm.ID, Descriptor), New_Config.all);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;
               return False;
            end if;
         end;

      when Semantic_Checks.Missing_Name_Error =>
         --  0. User name error. The input looks like:
         --
         --  "<begin_name_token> ... <end_name_token> ;"
         --
         --  where <end_name_token> is empty, because the user is changing it.
         --
         --  The fix is to ignore the error; return True. See
         --  test_mckenzie_recover Missing_Name_*.
         --
         --  1. missing 'begin' or extra 'end'. The input looks like:
         --
         --  1a. "<begin_named_token> ... begin handled_sequence_of_statements end <end_name_token> ;"
         --  or
         --  1b. "<begin_named_token> ... is declarative_part_opt end <end_name_token> ;"
         --
         --  where the <end_name_token> is empty. See test_mckenzie_recover.adb
         --  Missing_Name_*.
         --
         --  There are two subcases:
         --
         --  1c. The 'end <end_name_token> ;' is left over from editing, and
         --  should be deleted
         --
         --  1d. There is a missing 'begin'.
         --
         --  We can distinguish between 1c and 1d by looking for 'exception';
         --  if it is present, it is more likely there is a missing 'begin'. We
         --  cannot distinguish between 0 and 1, other than by parsing ahead.
         --  So we return two solutions; 'ignore error' and either 'insert
         --  begin' or 'delete end;'.

         declare
            End_Name : constant String := Lexer.Buffer_Text (End_Token.Name);

            Matching_Name_Index : SAL.Peek_Type := 2; -- start search before <end_name_token>
            Exception_Count     : Integer       := 0;
         begin
            Find_Matching_Name
              (Config, Lexer, End_Name, Matching_Name_Index, +EXCEPTION_ID, Exception_Count,
               Case_Insensitive => True);

            if Exception_Count = 0 then
               --  assume extra 'end ;'; delete it.
               declare
                  New_Config    : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops           : Config_Op_Arrays.Vector renames New_Config.Ops;
                  Stack         : Recover_Stacks.Stack renames New_Config.Stack;
                  End_Name_Item : Recover_Stack_Item;
               begin
                  --  This is a guess, but it is equally as likely as 'ignore error', so
                  --  it has the same cost.

                  Ops.Append ((Undo_Reduce, Nonterm.ID)); -- the failed reduce

                  Append_Check (Ops, Push_Back, Stack.Pop.Token.ID, +SEMICOLON_ID);

                  End_Name_Item := Stack.Pop;
                  Append_Check
                    (Ops, Push_Back, End_Name_Item.Token.ID,
                     (if Nonterm.ID = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID));

                  Append_Check (Ops, Push_Back, Stack.Pop.Token.ID, +END_ID);
                  Ops.Append ((Delete, +END_ID));

                  for T of Tree.Get_Terminal_IDs (End_Name_Item.Tree_Index) loop
                     Ops.Append ((Delete, T));
                  end loop;

                  --  Don't change New_Config.Current_Shared_Token

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Missing_Name_Error 1 " & Image (Nonterm.ID, Descriptor), New_Config.all);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;

            else
               --  assume missing 'begin'; insert it before 'end'
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
                  Stack      : Recover_Stacks.Stack renames New_Config.Stack;
                  End_Item   : Recover_Stack_Item;
               begin
                  --  This is a guess, but it is equally as likely as 'ignore error', so
                  --  it has the same cost.

                  Ops.Append ((Undo_Reduce, Nonterm.ID)); -- the failed reduce

                  Append_Check (Ops, Push_Back, Stack.Pop.Token.ID, +SEMICOLON_ID);

                  Append_Check
                    (Ops, Push_Back, Stack.Pop.Token.ID,
                     (if Nonterm.ID = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID));

                  End_Item := Stack.Pop;
                  Append_Check (Ops, Push_Back, End_Item.Token.ID, +END_ID);

                  New_Config.Current_Shared_Token := End_Item.Token.Min_Terminal_Index;

                  Ops.Append ((Insert, +BEGIN_ID));
                  New_Config.Inserted.Append (+BEGIN_ID);
                  New_Config.Current_Inserted := 1;

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Missing_Name_Error 1 " & Image (Nonterm.ID, Descriptor), New_Config.all);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;
            end if;
            return True; -- 'ignore error'.
         end;

      when Semantic_Checks.Extra_Name_Error =>
         --  The input looks like
         --
         --  "<begin_name_token> ... block_label_opt begin ... <end_name_token> ;"
         --
         --  where the erroneous reduce matches the empty 'block_label_opt'
         --  with '<end_name_Token>'.
         --
         --  0. If a matching <begin_name_token> is found, this is not a
         --  plausible user name error; return False. If it is not found, the
         --  user could be adding/deleting names; return True.
         --
         --  1. There is at least one missing 'end' before 'begin'. See
         --  test_mckenzie_recover.adb Extra_Name_1, Extra_Name_2,
         --  Two_Missing_Ends. The solution is to insert 'end ;' before the
         --  'begin'.
         --
         --  2. There is at least one missing 'end' after 'begin'. See
         --  test_mckenzie_recover.adb Extra_Name_3.
         --
         --  We can distinguish between 1 and 2 by counting the number of
         --  Begin_Name_IDs and Begin_IDs.
         --
         --  If there is more than one missing 'end', a later recover operation
         --  will fix the others.

         declare
            use Ada.Characters.Handling;

            End_Name : constant String := To_Lower (Lexer.Buffer_Text (End_Token.Name));

            Token_Count : constant SAL.Peek_Type := SAL.Peek_Type (Action.Token_Count);

            Matching_Name_Index : SAL.Peek_Type := Token_Count + 1;
            --  start search before 'block_label_opt'

            Other_Tokens : constant Token_ID_Set_Array := (1 => Begin_Name_IDs, 2 => Begin_IDs);
            Other_Counts : Natural_Array (1 .. 2);
         begin
            Find_Matching_Name
              (Config, Lexer, End_Name, Matching_Name_Index, Other_Tokens, Other_Counts, Case_Insensitive => True);

            if Matching_Name_Index > Config.Stack.Depth then
               --  case 0
               return True;
            end if;

            if Other_Counts (1) = 0 or Other_Counts (2) = 0 then
               raise Programmer_Error with "unrecognized Extra_Name_Error case";
            end if;

            if Other_Counts (1) >= Other_Counts (2) then
               --  Case 1
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
                  Stack      : Recover_Stacks.Stack renames New_Config.Stack;
               begin
                  --  We don't increase the cost, because this is not a guess.

                  Ops.Append ((Push_Back, Nonterm.ID)); -- the failed reduce

                  --  We know the 'block_label_opt' is empty, so set
                  --  Current_Shared_Token to point to 'begin'.
                  New_Config.Current_Shared_Token := Stack (Token_Count - 1).Token.Min_Terminal_Index;
                  Stack.Pop (Token_Count);

                  Ops.Append ((Insert, +END_ID));
                  Ops.Append ((Insert, +SEMICOLON_ID));

                  New_Config.Inserted.Append (+END_ID);
                  New_Config.Inserted.Append (+SEMICOLON_ID);
                  New_Config.Current_Inserted := 1;

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Extra_Name_Error 1 " & Image
                            (Nonterm.ID, Trace.Descriptor.all), New_Config.all);
                  end if;
               end;

            else
               --  Case 2
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
                  Stack      : Recover_Stacks.Stack renames New_Config.Stack;
                  End_Item   : Recover_Stack_Item;
               begin
                  --  We don't increase the cost, because this is not a guess.
                  --
                  --  Push back thru 'end':
                  Ops.Append ((Undo_Reduce, Nonterm.ID)); -- the failed reduce

                  Append_Check (Ops, Push_Back, Stack.Pop.Token.ID, +SEMICOLON_ID);

                  Append_Check
                    (Ops, Push_Back, Stack.Pop.Token.ID,
                     (if Nonterm.ID = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID));

                  End_Item := Stack.Pop;
                  Append_Check (Ops, Push_Back, End_Item.Token.ID, +END_ID);

                  New_Config.Current_Shared_Token := End_Item.Token.Min_Terminal_Index;

                  Ops.Append ((Insert, +END_ID));
                  Ops.Append ((Insert, +SEMICOLON_ID));

                  New_Config.Inserted.Append (+END_ID);
                  New_Config.Inserted.Append (+SEMICOLON_ID);
                  New_Config.Current_Inserted := 1;

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Extra_Name_Error 2 " & Image
                            (Nonterm.ID, Trace.Descriptor.all), New_Config.all);
                  end if;
               end;
            end if;
            return False;
         end;

      end case;
   end Semantic_Check_Fixes;

end WisiToken.LR.McKenzie_Recover.Ada_Lite;
