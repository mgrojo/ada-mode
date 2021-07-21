--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2018 - 2021 Stephen Leake All Rights Reserved.
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
#if ADA_LITE = "Ada_Lite" then
with Ada_Lite_Actions;
#elsif ADA_LITE = "Ada_Lite_Ebnf" then
with Ada_Lite_Ebnf_Actions;
#end if;
package body WisiToken.Parse.LR.McKenzie_Recover.$ADA_LITE is
#if ADA_LITE = "Ada_Lite" then
   package Actions renames Ada_Lite_Actions;
#elsif ADA_LITE = "Ada_Lite_Ebnf" then
   package Actions renames Ada_Lite_Ebnf_Actions;
#end if;

   use all type Actions.Token_Enum_ID; -- token names
   use all type In_Parse_Actions.Status_Label;

   Descriptor : WisiToken.Descriptor renames Actions.Descriptor;

   subtype Grammar_Token_ID_Set is WisiToken.Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal);

   --  From ada_lite.wy, <begin_name_token>, <end_name_token> are one of:
   --  block_label_opt           identifier_opt in block_statement
   --  name                      name_opt       in package_body
   --  subprogram_specification  name_opt       in subprogram_body

   Begin_Name_IDs : constant Grammar_Token_ID_Set := To_Token_ID_Set
     (Descriptor.First_Terminal, Descriptor.Last_Nonterminal,
      (+block_label_opt_ID & (+name_ID) & (+subprogram_specification_ID)));

   End_Name_IDs : constant Grammar_Token_ID_Set := To_Token_ID_Set
     (Descriptor.First_Terminal, Descriptor.Last_Nonterminal,
      (+identifier_opt_ID & (+name_opt_ID)));

   Begin_IDs : constant Grammar_Token_ID_Set := To_Token_ID_Set
     (Descriptor.First_Terminal, Descriptor.Last_Nonterminal,
      (1 => +BEGIN_ID));

   type Grammar_Token_ID_Set_Array is array (Positive range <>) of Grammar_Token_ID_Set;
   type Natural_Array is array (Positive range <>) of Natural;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Tree                : in     Syntax_Trees.Tree;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_IDs           : in     Grammar_Token_ID_Set_Array;
      Other_Counts        :    out Natural_Array;
      Case_Insensitive    : in     Boolean)
   is
      use Ada.Characters.Handling;
      Match_Name : constant String := (if Case_Insensitive then To_Lower (Name) else Name);
   begin
      Other_Counts := (others => 0);

      loop
         exit when Matching_Name_Index >= Config.Stack.Depth; -- Depth has Invalid_Token_ID
         declare
            Token : Syntax_Trees.Recover_Token renames Config.Stack.Peek (Matching_Name_Index).Token;
            Token_Name : constant Buffer_Region := Tree.Name (Token);
         begin
            exit when Token_Name /= Null_Buffer_Region and then
              Match_Name =
              (if Case_Insensitive
               then To_Lower (Tree.Lexer.Buffer_Text (Token_Name))
               else Tree.Lexer.Buffer_Text (Token_Name));

            for I in Other_IDs'Range loop
               if Other_IDs (I)(Tree.ID (Token)) then
                  Other_Counts (I) := Other_Counts (I) + 1;
               end if;
            end loop;

            Matching_Name_Index := Matching_Name_Index + 1;
         end;
      end loop;
   end Find_Matching_Name;

   procedure Handle_Check_Fail
     (Super             : not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Table       : in              WisiToken.Parse.LR.Parse_Table;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in              Configuration)
   with Pre => Config.User_Parse_Action_Status.Label /= Ok
   is
      use Syntax_Trees;

      Tree         : Syntax_Trees.Tree renames Super.Tree.all;
      Parser_Label : constant Syntax_Trees.Stream_ID := Super.Stream (Parser_Index);

      procedure Put (Message : in String; Config : in Configuration)
      is begin
         Put (Message, Super.Trace.all, Tree, Parser_Label, Config);
      end Put;

      Begin_Name_Token : Recover_Token renames Config.User_Parse_Action_Status.Begin_Name;
      End_Name_Token   : Recover_Token renames Config.User_Parse_Action_Status.End_Name;
   begin
      if not Begin_Name_IDs (Tree.ID (Begin_Name_Token)) then
         raise SAL.Programmer_Error with "unrecognized begin_name_token id " &
           Image (Tree.ID (Begin_Name_Token), Descriptor);
      end if;

      if not End_Name_IDs (Tree.ID (End_Name_Token)) then
         raise SAL.Programmer_Error with "unrecognized begin_name_token id " &
           Image (Tree.ID (End_Name_Token), Descriptor);
      end if;

      case Config.User_Parse_Action_Status.Label is
      when Ok =>
         raise SAL.Programmer_Error;

      when Match_Names_Error =>
         --  0. User name error. The input looks like:
         --
         --  "<begin_name_token> ... <end_name_token> ;"
         --
         --  where the names do not match, because the user is changing them.
         --
         --  The fix is to ignore the error.
         --
         --  1. The mismatch indicates one or more missing 'end's. The input
         --  looks like:
         --
         --  "<correct_begin_name_token> ... <bad_begin_name_token> ... <end_name_token> ;"
         --
         --  where <correct_begin_name_token> matches <end_name_token>, but
         --  <bad_begin_name_token> does not, and the erroneous reduce has
         --  matched <bad_begin_name_token> with <end_name_token>.
         --
         --  The fix is to insert one or more 'end ;' before <end_name_token>.
         --  See test_mckenzie_recover.adb Block_Match_Names_1, Extra_Name_2.
         --
         --
         --  It is not possible for the mismatch to indicate an extra 'end';
         --  that would generate either a Missing_Name_Error, or a syntax
         --  error.
         --
         --  To distinguish between case 0 and 1, we search the stack for
         --  <correct_begin_name_token>. If found, it's case 1, otherwise case 0.
         --
         --  If there is more than one missing 'end', a later recover operation
         --  will fix the others. For example, in test_mckenzie_recover
         --  Extra_Name_2, we get here on a second semantic check error.

         --  This case doesn't use Tree, and it can handle some virtual tokens.

         declare
            End_Name : constant String := Tree.Lexer.Buffer_Text (Tree.Name (End_Name_Token));

            Matching_Name_Index : SAL.Peek_Type := 3; -- start search before <end_name_token>
         begin
            Find_Matching_Name (Config, Tree, End_Name, Matching_Name_Index, Case_Insensitive => True);

            if Matching_Name_Index = Config.Stack.Depth then
               --  case 0.
               return;

            else
               --  Case 1.
               declare
                  New_Config : Configuration := Config;
                  --  We don't use Configuration_Access here because we might abandon
                  --  this config.
               begin
                  --  This is a not a guess, so it has zero cost.

                  New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

                  New_Config.Error_Token  := Invalid_Recover_Token;
                  New_Config.User_Parse_Action_Status := (Label   => Ok);

                  Push_Back_Check
                    (Super, New_Config,
                     (+SEMICOLON_ID,
                      (if Tree.ID (Config.Error_Token) = +block_statement_ID
                       then +identifier_opt_ID
                       else +name_opt_ID)));

                  if New_Config.Stack.Peek (1).Token.Virtual then
                     --  'end' is on top of stack. We want to set Current_Shared_Token to
                     --  'end'; we can't if it is invalid (which it is if it was pushed
                     --  after a previous fix).
                     --
                     --  We don't check earlier for Invalid_Indices, because we can handle
                     --  other tokens having invalid indices.

                     return;
                  end if;

                  Push_Back_Check (Super, New_Config, +END_ID);
                  Insert (Tree, New_Config, (+END_ID, +SEMICOLON_ID));

                  Local_Config_Heap.Add (New_Config);

                  if Trace_McKenzie > Detail then
                     Put ("Language_Fixes Match_Names_Error 1 " & Image (Tree.ID (Config.Error_Token), Descriptor),
                          New_Config);
                     if Trace_McKenzie > Extra then
                        Super.Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Tree));
                     end if;
                  end if;
               end;
            end if;
         end;

      when Missing_Name_Error =>
         --  0. User name error. The input looks like:
         --
         --  0a. "<begin_name_token> ... <end_name_token> ;"
         --
         --  0b. "<begin_named_token> is declarative_part end <end_name_token> ;"
         --
         --  where <end_name_token> is empty, because the user is changing it.
         --  0a looks like a subprogram or named block; 0b looks like a package
         --  body.
         --
         --  The fix is to ignore the error. See test_mckenzie_recover
         --  Missing_Name_*.
         --
         --  1. missing 'begin' or extra 'end'. The stack looks like:
         --
         --   "<begin_named_token> ... begin handled_sequence_of_statements end <end_name_token> ;"
         --
         --  where the <end_name_token> is empty. See test_mckenzie_recover.adb
         --  Missing_Name_*.
         --
         --  There are two subcases:
         --
         --  1a. The 'end <end_name_token> ;' is left over from editing, and
         --  should be deleted
         --
         --  1b. There is a missing 'begin'.
         --
         --  We can distinguish between 1a and 1b by looking for 'exception';
         --  if it is present, it is more likely there is a missing 'begin'.
         --  However, 'exception' is contained by
         --  'handled_sequence_of_statements' on the stack, so we have to look
         --  inside that using the syntax tree.
         --
         --  We cannot distinguish between cases 0 and 1, other than by parsing
         --  ahead, except in case 0b. So we enqueue two solutions; 'ignore
         --  error' and either 'insert begin' or 'delete end;'.

         if not Valid_Tree_Indices (Config.Stack, SAL.Base_Peek_Type (Config.User_Parse_Action_Token_Count)) then
            --  Invalid tree indices happens when recover enqueues a config that
            --  contains tokens pushed during recover.

            return;
         end if;

         if Tree.ID (Config.Error_Token) = +package_body_ID then
            --  case 0b.
            return;
         end if;

         if Config.Stack.Depth >= 4 and then
           (declare
               Handled_Sequence : Syntax_Trees.Recover_Token renames Config.Stack.Peek (4).Token;
            begin
               not Handled_Sequence.Virtual and then
                  Handled_Sequence.Element_Node /= Syntax_Trees.Invalid_Node_Access and then
                  Tree.Find_Child (Handled_Sequence.Element_Node, +EXCEPTION_ID) =
                  Syntax_Trees.Invalid_Node_Access)
         then
            --  'exception' not found; case 1a - assume extra 'end ;'; delete it.
            declare
               New_Config : aliased Configuration := Config;
            begin
               New_Config.Error_Token  := Invalid_Recover_Token;
               New_Config.User_Parse_Action_Status := (Label => Ok);

               New_Config.Cost := New_Config.Cost + 1;

               New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

               Push_Back_Check
                 (Super, New_Config,
                  (+SEMICOLON_ID,
                   (if Tree.ID (Config.Error_Token) = +block_statement_ID
                    then +identifier_opt_ID
                    else +name_opt_ID),
                   +END_ID));

               Undo_Reduce_Check
                 (Super, Parse_Table, New_Config,
                  (+handled_sequence_of_statements_ID,
                   +sequence_of_statements_ID));

#if ADA_LITE = "Ada_Lite" then
               Check (Tree.ID (New_Config.Stack.Peek (1).Token), +sequence_of_statements_list_ID);
#elsif ADA_LITE = "Ada_Lite_Ebnf" then
               Check (Tree.ID (New_Config.Stack.Peek (1).Token), +statement_list_ID);
#end if;
               --  This is handling Missing_Name_Error, so we know the identifier_opt
               --  or name_opt is empty.
               Delete_Check (Tree, New_Config, (+END_ID, +SEMICOLON_ID));

               if Trace_McKenzie > Detail then
                  Put
                    ("Language_Fixes Missing_Name_Error 1a " & Image (Tree.ID (Config.Error_Token), Descriptor),
                     New_Config);
                  if Trace_McKenzie > Extra then
                     Super.Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Tree));
                  end if;
               end if;
               Local_Config_Heap.Add (New_Config);
            end;

         else
            --  'exception' found; case 1b - assume missing 'begin'; insert it
            --  before 'handled_sequence_of_statements'
            declare
               New_Config : Configuration := Config;
            begin
               New_Config.Error_Token  := Invalid_Recover_Token;
               New_Config.User_Parse_Action_Status := (Label => Ok);

               New_Config.Cost := New_Config.Cost + 1;

               New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

               Push_Back_Check
                 (Super, New_Config,
                  (+SEMICOLON_ID,
                   (if Tree.ID (Config.Error_Token) = +block_statement_ID
                    then +identifier_opt_ID
                    else +name_opt_ID),
                   +END_ID, +handled_sequence_of_statements_ID));

               Insert (Tree, New_Config, +BEGIN_ID);

               if Trace_McKenzie > Detail then
                  Put ("Language_Fixes Missing_Name_Error 1b " &
                         Image (Tree.ID (Config.Error_Token), Descriptor), New_Config);
                  if Trace_McKenzie > Extra then
                     Super.Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Tree));
                  end if;
               end if;
               Local_Config_Heap.Add (New_Config);
            end;
         end if;

      when Extra_Name_Error =>
         --  The input looks like
         --
         --  "<begin_name_token> ... block_label_opt begin ... end <end_name_token> ;"
         --
         --  where the erroneous reduce matches the empty 'block_label_opt'
         --  with '<end_name_Token>'.
         --
         --  0. If a matching <begin_name_token> is found, this is not a
         --  plausible user name error. If it is not found, the user could be
         --  adding/deleting names.
         --
         --  1. There is at least one missing 'end' before 'begin'. See
         --  test_mckenzie_recover.adb Extra_Name_1, Extra_Name_2,
         --  Two_Missing_Ends. The solution is to insert 'end ;' before the
         --  'begin'.
         --
         --  2. There is at least one missing 'end' after 'begin'. See
         --  test_mckenzie_recover.adb Extra_Name_3, Block_Match_Names_1. The
         --  solution is to insert 'end ;' before the 'end'.
         --
         --  We can distinguish between 1 and 2 by counting the number of
         --  Begin_Name_IDs and Begin_IDs.
         --
         --  If there is more than one missing 'end', a later recover operation
         --  will fix the others.

         --  This case can handle Config.Error_Token.Virtual = True, and it doesn't use
         --  Tree.
         declare
            use Ada.Characters.Handling;

            End_Name : constant String := To_Lower (Tree.Lexer.Buffer_Text (Tree.Name (End_Name_Token)));

            Token_Count : constant SAL.Peek_Type := SAL.Peek_Type (Config.User_Parse_Action_Token_Count);

            Matching_Name_Index : SAL.Peek_Type := Token_Count;
            --  'block_label_opt' is empty; start on token before 'begin'.

            Other_Tokens : constant Grammar_Token_ID_Set_Array := (1 => Begin_Name_IDs, 2 => Begin_IDs);
            Other_Counts : Natural_Array (1 .. 2);
         begin
            Find_Matching_Name
              (Config, Tree, End_Name, Matching_Name_Index, Other_Tokens, Other_Counts, Case_Insensitive => True);

            if Matching_Name_Index > Config.Stack.Depth then
               --  case 0, not found.
               return;
            end if;

            if Other_Counts (1) = 0 or Other_Counts (2) = 0 then
               if Trace_McKenzie > Detail then
                  Put ("Language_Fixes unrecognized Extra_Name_Error case", Config);
               end if;
               return;
            end if;

            if Other_Counts (1) > Other_Counts (2) then
               --  Case 1.
               declare
                  New_Config : Configuration := Config;
               begin
                  --  We don't increase the cost, because this is not a guess.
                  New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

                  New_Config.Error_Token  := Invalid_Recover_Token;
                  New_Config.User_Parse_Action_Status := (Label => Ok);

                  --  Push_Back the failed reduce tokens.
                  for I in 1 .. Token_Count loop
                     Push_Back (Super, New_Config, Push_Back_Undo_Reduce => True);
                  end loop;

                  Insert (Tree, New_Config, +END_ID);
                  --  We don't insert ';' here, because we may need to insert other
                  --  stuff first; let Minimal_Complete_Actions handle it. See
                  --  test_mckenzie_recover Two_Missing_Ends.

                  if Trace_McKenzie > Detail then
                     Put ("Language_Fixes Extra_Name_Error 1 " & Image
                            (Tree.ID (Config.Error_Token), Descriptor), New_Config);
                     if Trace_McKenzie > Extra then
                        Super.Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Tree));
                     end if;
                  end if;
                  Local_Config_Heap.Add (New_Config);
               end;

            else
               --  Case 2.
               declare
                  Label      : constant String := "Extra_Name_Error 2";
                  New_Config : Configuration   := Config;
               begin
                  --  We don't increase the cost, because this is not a guess.
                  New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

                  New_Config.Error_Token  := Invalid_Recover_Token;
                  New_Config.User_Parse_Action_Status := (Label   => Ok);

                  Push_Back_Check
                    (Super, New_Config,
                     (+SEMICOLON_ID,
                     (if Tree.ID (Config.Error_Token) = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID),
                      +END_ID));

                  Insert (Tree, New_Config, +END_ID);
                  --  Let Minimal_Complete_Actions do the rest of the insert; see
                  --  comment in case 1.

                  if Trace_McKenzie > Detail then
                     Put ("Language_Fixes " & Label & Image (Tree.ID (Config.Error_Token), Descriptor), New_Config);
                  end if;
                  Local_Config_Heap.Add (New_Config);
               end;
            end if;
         end;
      end case;
   exception
   when Bad_Config =>
      null;
   end Handle_Check_Fail;

   procedure Handle_Parse_Error
     (Super             : not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Table       : in              WisiToken.Parse.LR.Parse_Table;
      Local_Config_Heap : in out          Config_Heaps.Heap_Type;
      Config            : in              Configuration)
   with Pre => Config.User_Parse_Action_Status.Label = Ok
   is
      use Syntax_Trees;

      Tree         : Syntax_Trees.Tree renames Super.Tree.all;
      Parser_Label : constant Syntax_Trees.Stream_ID := Super.Stream (Parser_Index);

      procedure Put (Message : in String; Config : in Configuration)
      is begin
         Put (Message, Super.Trace.all, Tree, Parser_Label, Config);
      end Put;
   begin
      if Tree.ID (Config.Error_Token) = +DOT_ID then
         --  We've encountered a Selected_Component when we were expecting a
         --  simple IDENTIFIER or a name. If the name is preceded by 'end', then
         --  this similar to a semantic check Extra_Name_Error, and the
         --  solutions are similar.

         if Config.Stack.Depth >= 5 and then
           (Tree.ID (Config.Stack.Peek (1).Token) = +IDENTIFIER_ID and
              Tree.ID (Config.Stack.Peek (2).Token) = +END_ID)
         then
            --  The input looks like one of:
            --
            --  1. "<begin_name_token_1> ... <begin_name_token_2> ... begin ... begin ... end <end_name_token_1> ;"
            --
            --  2. "<begin_name_token_1> ... begin ... declare ... begin ... end <end_name_token_1> ;"
            --
            --  Case 1) is missing 'end <end_name_token_2> ;' between the
            --  'begin's, so parsing expects <end_name_token_1> to match the
            --  second 'begin', which looks like an unnamed block. See
            --  test_mckenzie_recover Match_Selected_Component_1. 'declare ...'
            --  may _not_ be present on the second begin.
            --
            --  The solution is to insert 'end ;' before the second 'begin'.
            --
            --  Case 2) is missing 'end;' after the second 'begin'. See
            --  test_mckenzie_recover Match_Selected_Component_2. 'declare ...'
            --  may be absent on the second begin, or a name may be present. The
            --  solution is to insert 'end;' after the second 'begin' (ie before
            --  the last 'end').
            --
            --  Note that it's _not_ possible the user is just editing names; that
            --  would generate a semantic check fail, not a parse table error,
            --  since a "." would be permitted.
            --
            --  If a 'declare' is present on the second begin, we have case 2.
            --  Otherwise we cannot distinguish between the two, so enqueue both
            --  solutions.

            declare
               Label        : constant String := "Selected_Component 1 ";
               New_Config_1 : Configuration   := Config;
            begin
               New_Config_1.Error_Token := Invalid_Recover_Token;

               Push_Back_Check (Super, New_Config_1, (+IDENTIFIER_ID, +END_ID));

               case To_Token_Enum (Tree.ID (New_Config_1.Stack.Peek (3).Token)) is
               when block_label_opt_ID =>
                  --  no 'declare'; either case 1 or 2

                  declare
                     New_Config_2 : Configuration := New_Config_1;
                  begin
                     Insert (Tree, New_Config_2, (+END_ID, +SEMICOLON_ID));

                     New_Config_2.Strategy_Counts (Language_Fix) := New_Config_2.Strategy_Counts (Language_Fix) + 1;

                     if Trace_McKenzie > Detail then
                        Put ("Language_Fixes " & Label & Image (Tree.ID (Config.Error_Token), Descriptor),
                             New_Config_2);
                     end if;
                     Local_Config_Heap.Add (New_Config_2);
                  end;

                  Push_Back_Check
                    (Super, New_Config_1,
                     (+handled_sequence_of_statements_ID, +BEGIN_ID, +block_label_opt_ID));
                  Insert (Tree, New_Config_1, (+END_ID, +SEMICOLON_ID));

               when declarative_part_ID =>
                  --  case 2
                  Insert (Tree, New_Config_1, (+END_ID, +SEMICOLON_ID));

               when others =>
                  if Trace_McKenzie > Outline then
                     Put ("Language_Fixes " & Label & "missing case " & Image
                            (Tree.ID (New_Config_1.Stack.Peek (3).Token), Descriptor), Config);
                  end if;
                  return;
               end case;

               New_Config_1.Strategy_Counts (Language_Fix) := New_Config_1.Strategy_Counts (Language_Fix) + 1;

               if Trace_McKenzie > Detail then
                  Put ("Language_Fixes " & Label & Image (Tree.ID (Config.Error_Token), Descriptor),
                       New_Config_1);
               end if;
               Local_Config_Heap.Add (New_Config_1);
            end;
         end if;

      elsif Tree.ID (Config.Error_Token) in +IDENTIFIER_ID and
        Tree.ID (Config.Stack.Peek.Token) = +END_ID
      then
         --  We've encountered an identifier after 'end' when
         --  expecting a keyword. See test_mckenzie_recover.adb
         --  Forbid_Minimal_Complete.
         --
         --  If a matching 'begin name' is found on the stack, the input looks
         --  like:
         --
         --  1) "<begin_name_token> ... begin ... <compound_statement_begin> ... end <end_name_token> ;"
         --
         --  There is a missing 'end <compound_statement_id> ;' before the
         --  'end'. We can get the ID to insert from Parse_Table
         --  Minimal_Complete_Actions.
         --
         --  Minimal_Complete_Actions does not handle this case well; it
         --  ignores the name.
         declare
            End_ID_Actions : constant Minimal_Action_Arrays.Vector := Parse_Table.States
              (Config.Stack.Peek.State).Minimal_Complete_Actions;
            End_Name       : constant String := Tree.Lexer.Buffer_Text (Tree.Byte_Region (Config.Error_Token));

            Matching_Name_Index : SAL.Peek_Type := 2; -- start search before 'end'
         begin
            Find_Matching_Name (Config, Tree, End_Name, Matching_Name_Index, Case_Insensitive => True);

            if Matching_Name_Index < Config.Stack.Depth and then
              End_ID_Actions.Length = 1 and then
              End_ID_Actions (End_ID_Actions.First_Index).Verb = Shift
            then
               declare
                  Label      : constant String := "missing end keyword";
                  New_Config : Configuration   := Config;
               begin
                  New_Config.Error_Token := Invalid_Recover_Token;

                  New_Config.Strategy_Counts (Language_Fix) := New_Config.Strategy_Counts (Language_Fix) + 1;

                  Push_Back_Check (Super, New_Config, +END_ID);

                  --  Inserting the end keyword and semicolon here avoids the costs added by
                  --  Insert_Minimal_Complete_Actions.
                  Insert (Tree, New_Config, (+END_ID, End_ID_Actions (End_ID_Actions.First_Index).ID, +SEMICOLON_ID));

                  Local_Config_Heap.Add (New_Config);
                  if Trace_McKenzie > Detail then
                     Put ("Language_Fixes " & Label, New_Config);
                  end if;
               exception
               when Bad_Config =>
                  null;
               end;
            end if;
         end;

      end if;
   exception
   when Bad_Config =>
      null;
   end Handle_Parse_Error;

   ----------
   --  Public subprograms

   procedure Fixes
     (Super             : not null access WisiToken.Parse.LR.McKenzie_Recover.Base.Supervisor;
      Parser_Index      : in              SAL.Peek_Type;
      Parse_Table       : in              WisiToken.Parse.LR.Parse_Table;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
   is begin
      if Trace_McKenzie > Extra then
         Put_Line
           (Super.Trace.all, Super.Tree.all, Super.Stream (Parser_Index), "Language_Fixes stack: " & LR.Image
              (Config.Stack, Super.Tree.all));
      end if;

      case Config.User_Parse_Action_Status.Label is
      when Ok =>
         Handle_Parse_Error (Super, Parser_Index, Parse_Table, Local_Config_Heap, Config);

      when others =>
         Handle_Check_Fail (Super, Parser_Index, Parse_Table, Local_Config_Heap, Config);
      end case;
   end Fixes;

   procedure Matching_Begin_Tokens
     (Tree                    : in     Syntax_Trees.Tree;
      Tokens                  : in     Token_ID_Array_1_3;
      Config                  : in     Configuration;
      Matching_Tokens         :    out Token_ID_Arrays.Vector;
      Forbid_Minimal_Complete :    out Boolean)
   is
      use Token_ID_Arrays;

      function Matching_Begin_For_End (Next_Index : in Positive) return Token_ID_Arrays.Vector
      is begin
         return Result : Token_ID_Arrays.Vector do
            if Tokens (Next_Index) = Invalid_Token_ID then
               Result := To_Vector (+BEGIN_ID);
            else
               case To_Token_Enum (Tokens (Next_Index)) is
               when IF_ID =>
                  Result := To_Vector (+IF_ID);

               when IDENTIFIER_ID =>
                  if Tokens (Next_Index + 1) /= Invalid_Token_ID and then
                    To_Token_Enum (Tokens (Next_Index + 1)) = DOT_ID
                  then
                     Result := To_Vector ((+PACKAGE_ID, +IDENTIFIER_ID, +IS_ID));
                     --  Deliberately leaving out BODY_ID, so we encounter a conflict
                     --  between package_specification and generic_instantiation; see
                     --  test_mckenzie_recover.adb Check_Parse_All_Conflicts.
                  else
                     Result := To_Vector ((+IDENTIFIER_ID, +COLON_ID, +BEGIN_ID));
                  end if;

               when SEMICOLON_ID =>
                  Result := To_Vector (+BEGIN_ID);

               when LOOP_ID =>
                  Result := To_Vector (+LOOP_ID);

               when others =>
                  null;
               end case;
            end if;
         end return;
      end Matching_Begin_For_End;

   begin
      case Actions.Token_Enum_ID'(-Tokens (1)) is
      when BEGIN_ID | END_ID | EXCEPTION_ID | IS_ID | SEMICOLON_ID =>

         case Actions.Token_Enum_ID'(-Tokens (1)) is
         when END_ID =>
            Matching_Tokens := Matching_Begin_For_End (2);

         when EXCEPTION_ID =>
            Matching_Tokens := To_Vector (+BEGIN_ID);

         when IS_ID =>
            Matching_Tokens := To_Vector (+PROCEDURE_ID);

         when SEMICOLON_ID =>
            Matching_Tokens := To_Vector (+IDENTIFIER_ID);

         when others =>
            null;
         end case;

      when Wisi_EOI_ID =>
         Matching_Tokens := Empty_Vector;

      when others =>
         Matching_Tokens := Empty_Vector;
      end case;

      if Tree.ID (Config.Stack.Peek.Token) = +END_ID and
        Tokens (1) = +IDENTIFIER_ID and
            (Tokens (2) /= Invalid_Token_ID and then
               -Tokens (2) in DOT_ID | SEMICOLON_ID)

           --  We should add this here, but we don't to preserve existing tests.
           --  (-Tokens (1) in BEGIN_ID | END_ID | EXCEPTION_ID | IS_ID | SEMICOLON_ID)
      then
         Forbid_Minimal_Complete := True;
      else
         Forbid_Minimal_Complete := False;
      end if;
   end Matching_Begin_Tokens;

   function String_ID_Set
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set
   is begin
      pragma Assert (Descriptor.String_1_ID = Invalid_Token_ID);
      pragma Assert (Descriptor.String_2_ID = String_Literal_ID);

      return Result : Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal) := (others => False) do
         Result (Descriptor.String_2_ID) := True;
         Result (+name_ID) := True;
         Result (+primary_ID) := True;
         Result (+factor_ID) := True;
         Result (+term_ID) := True;
#if ADA_LITE = "Ada_Lite" then
         Result (+term_list_ID) := True;
#end if;
         Result (+simple_expression_ID) := True;
         Result (+relation_ID) := True;
         Result (+expression_ID) := True;
      end return;
   end String_ID_Set;

end WisiToken.Parse.LR.McKenzie_Recover.$ADA_LITE;
--  Local Variables:
--  ada-case-strict: t
--  End:
