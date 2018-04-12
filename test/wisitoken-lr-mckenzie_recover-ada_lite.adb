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
with System.Assertions;
package body WisiToken.LR.McKenzie_Recover.Ada_Lite is

   use all type Standard.Ada_Lite.Token_Enum_ID; -- token names
   use all type Semantic_Checks.Check_Status_Label;

   Descriptor : WisiToken.Descriptor renames Standard.Ada_Lite.Descriptor;

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

   Nonterm_IDs : constant Grammar_Token_ID_Set := To_Token_ID_Set
     (Descriptor.First_Terminal, Descriptor.Last_Nonterminal,
      (+block_statement_ID & (+package_body_ID) & (+subprogram_body_ID)));

   Begin_IDs : constant Grammar_Token_ID_Set := To_Token_ID_Set
     (Descriptor.First_Terminal, Descriptor.Last_Nonterminal,
      (1 => +BEGIN_ID));

   type Grammar_Token_ID_Set_Array is array (Positive range <>) of Grammar_Token_ID_Set;
   type Natural_Array is array (Positive range <>) of Natural;

   procedure Find_Matching_Name
     (Config              : in     Configuration;
      Lexer               : in     WisiToken.Lexer.Handle;
      Name                : in     String;
      Matching_Name_Index : in out SAL.Peek_Type;
      Other_IDs           : in     Grammar_Token_ID_Set_Array;
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

   procedure Check
     (ID          :    Token_ID;
      Expected_ID : in Token_ID)
   is begin
      pragma Assert (ID = Expected_ID);
   end Check;

   procedure Push_Back_Check
     (Config      : in out Configuration;
      Expected_ID : in     Token_ID)
   is
      Item        : constant Recover_Stack_Item         := Config.Stack.Pop;
      Token_Index : constant WisiToken.Base_Token_Index := Item.Token.Min_Terminal_Index;
   begin
      pragma Assert (Item.Token.ID = Expected_ID);

      if Token_Index /= Invalid_Token_Index then
         Config.Current_Shared_Token := Token_Index;
      end if;

      Config.Ops.Append ((Push_Back, Item.Token.ID, Config.Current_Shared_Token));
   end Push_Back_Check;

   procedure Push_Back_Check
     (Config   : in out Configuration;
      Expected : in     Token_ID_Array)
   is begin
      for ID of Expected loop
         Push_Back_Check (Config, ID);
      end loop;
   end Push_Back_Check;

   procedure Push_Back_Check
     (Config   : in Configuration_Access;
      Expected : in Token_ID_Array)
   is begin
      for ID of Expected loop
         Push_Back_Check (Config.all, ID);
      end loop;
   end Push_Back_Check;

   procedure Undo_Reduce_Check
     (Config   : in Configuration_Access;
      Tree     : in Syntax_Trees.Tree;
      Expected : in Token_ID)
   is begin
      Check (Config.Stack (1).Token.ID, Expected);
      Config.Ops.Append ((Undo_Reduce, Expected, Undo_Reduce (Config.Stack, Tree)));
   end Undo_Reduce_Check;

   procedure Undo_Reduce_Check
     (Config   : in Configuration_Access;
      Tree     : in Syntax_Trees.Tree;
      Expected : in Token_ID_Array)
   is begin
      for ID of Expected loop
         Undo_Reduce_Check (Config, Tree, ID);
      end loop;
   end Undo_Reduce_Check;

   procedure Insert
     (Config : in out Configuration;
      ID     : in     Token_ID)
   is begin
      Config.Ops.Append ((Insert, ID, Config.Current_Shared_Token));
      Config.Inserted.Append (ID);
      Config.Current_Inserted := 1;
   end Insert;

   procedure Insert
     (Config : in out Configuration;
      IDs    : in     Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Config, ID);
      end loop;
   end Insert;

   procedure Insert
     (Config : in Configuration_Access;
      ID     : in Token_ID)
   is begin
      Insert (Config.all, ID);
   end Insert;

   procedure Insert
     (Config : in Configuration_Access;
      IDs    : in Token_ID_Array)
   is begin
      for ID of IDs loop
         Insert (Config, ID);
      end loop;
   end Insert;

   function Handle_Check_Fail
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : in     WisiToken.Lexer.Handle;
      Parser_Label      : in     Natural;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
     return Non_Success_Status
   with Pre => Config.Check_Status.Label /= Ok
   is
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type Syntax_Trees.Node_Index;

      procedure Put (Message : in String; Config : in Configuration)
      is begin
         Put (Message, Trace, Parser_Label, Terminals, Config);
      end Put;

      Begin_Name_Token : Recover_Token renames Config.Check_Status.Begin_Name;
      End_Name_Token   : Recover_Token renames Config.Check_Status.End_Name;
   begin
      if not Begin_Name_IDs (Begin_Name_Token.ID) then
         raise Programmer_Error with "unrecognized begin_name_token id " & Image (Begin_Name_Token.ID, Descriptor);
      end if;

      if not End_Name_IDs (End_Name_Token.ID) then
         raise Programmer_Error with "unrecognized begin_name_token id " & Image (End_Name_Token.ID, Descriptor);
      end if;

      if not Nonterm_IDs (Config.Error_Token.ID) then
         raise Programmer_Error with "unrecognized begin_name_token id " & Image (Config.Error_Token.ID, Descriptor);
      end if;

      if Config.Ops_Insert_Point /= Config_Op_Arrays.No_Index then
         if Trace_McKenzie > Outline then
            Put ("Handle_Check_Fail test case for Ops_Insert_Point", Config);
         end if;
         raise Programmer_Error with "Handle_Check_Fail found test case for Ada_Lite Language_Fixes Ops_Insert_Point.";
      end if;

      case Config.Check_Status.Label is
      when Ok =>
         raise Programmer_Error;

      when Match_Names_Error =>
         --  0. User name error. The input looks like:
         --
         --  "<begin_name_token> ... <end_name_token> ;"
         --
         --  where the names do not match, because the user is changing them.
         --
         --  The fix is to ignore the error; return Continue. See
         --  propagate_names.ada_lite Proc_1.
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
            End_Name : constant String := Lexer.Buffer_Text (End_Name_Token.Name);

            Matching_Name_Index : SAL.Peek_Type := 2; -- start search before <end_name_token>
         begin
            Find_Matching_Name (Config, Lexer, End_Name, Matching_Name_Index, Case_Insensitive => True);

            if Matching_Name_Index = Config.Stack.Depth then
               --  case 0.
               return Continue;

            else
               --  Case 1.
               declare
                  New_Config : Configuration := Config;
                  --  We don't use Configuration_Access here because we might abandon
                  --  this config.
               begin
                  --  This is a not a guess, so it has zero cost.

                  New_Config.Error_Token.ID := Invalid_Token_ID;
                  New_Config.Check_Status   := (Label => Ok);

                  Push_Back_Check
                    (New_Config,
                     (+SEMICOLON_ID,
                      (if Config.Error_Token.ID = +block_statement_ID
                       then +identifier_opt_ID
                       else +name_opt_ID)));

                  if New_Config.Stack (1).Token.Min_Terminal_Index = Invalid_Token_Index then
                     --  'end' is on top of stack. We want to set Current_Shared_Token to
                     --  'end'; we can't if it has an invalid index (which it has if it was
                     --  pushed after a previous fix).
                     --
                     --  We don't check earlier for Invalid_Indices, because we can handle
                     --  other tokens having invalid indices.

                     return Abandon; -- ignore error is not valid
                  end if;

                  Push_Back_Check (New_Config, +END_ID);
                  Insert (New_Config, (+END_ID, +SEMICOLON_ID));

                  Local_Config_Heap.Add (New_Config);

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Match_Names_Error 1 " & Image (Config.Error_Token.ID, Descriptor),
                          New_Config);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;
               return Abandon;
            end if;
         end;

      when Missing_Name_Error =>
         --  0. User name error. The input looks like:
         --
         --  0a. "<begin_name_token> ... <end_name_token> ;"
         --
         --  0b. "<begin_named_token> is declarative_part_opt end <end_name_token> ;"
         --
         --  where <end_name_token> is empty, because the user is changing it.
         --  0a looks like a subprogram or named block; 0b looks like a package
         --  body.
         --
         --  The fix is to ignore the error; return Continue. See
         --  test_mckenzie_recover Missing_Name_*.
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
         --  ahead, except in case 0b. So we return two solutions; 'ignore
         --  error' and either 'insert begin' or 'delete end;'.

         if not Valid_Tree_Indices (Config.Stack, SAL.Base_Peek_Type (Config.Check_Token_Count)) then
            --  Invalid tree indices happens when recover enqueues a config that
            --  contains tokens pushed during recover.

            return Abandon; -- ignore error is not valid in this case.
         end if;

         if Config.Error_Token.ID = +package_body_ID then
            --  case 0b.
            return Continue;
         end if;

         if Syntax_Trees.Invalid_Node_Index = Tree.Find_Child (Config.Stack (4).Tree_Index, +EXCEPTION_ID) then
            --  'exception' not found; case 1b - assume extra 'end ;'; delete it.
            declare
               New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
               Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
               Stack      : Recover_Stacks.Stack renames New_Config.Stack;
               End_Item   : constant Recover_Stack_Item := Stack.Peek (3);
            begin
               --  This is a guess, but it is equally as likely as 'ignore error', so
               --  it has the same cost.

               New_Config.Error_Token.ID := Invalid_Token_ID;
               New_Config.Check_Status   := (Label => Ok);

               Push_Back_Check
                 (New_Config,
                  (+SEMICOLON_ID,
                   (if Config.Error_Token.ID = +block_statement_ID
                    then +identifier_opt_ID
                    else +name_opt_ID),
                   +END_ID));

               Undo_Reduce_Check
                 (New_Config, Tree,
                  (+handled_sequence_of_statements_ID,
                   +sequence_of_statements_opt_ID));

               Check (New_Config.Stack (1).Token.ID, +sequence_of_statements_ID);

               Ops.Append ((Delete, +END_ID, Token_Index => End_Item.Token.Min_Terminal_Index));
               Ops.Append ((Delete, +SEMICOLON_ID, Token_Index => End_Item.Token.Min_Terminal_Index + 1));

               New_Config.Current_Shared_Token := Config.Current_Shared_Token; --  After pushed_back SEMICOLON.

               if Trace_McKenzie > Detail then
                  Put ("Semantic_Check Missing_Name_Error 1b " & Image (Config.Error_Token.ID, Descriptor),
                       New_Config.all);
                  if Trace_McKenzie > Extra then
                     Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                  end if;
               end if;
            end;

         else
            --  'exception' found; case 1a - assume missing 'begin'; insert it
            --  before 'handled_sequence_of_statements'
            declare
               New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
            begin
               --  This is a guess, but it is equally as likely as 'ignore error', so
               --  it has the same cost.
               New_Config.Error_Token.ID := Invalid_Token_ID;
               New_Config.Check_Status   := (Label => Ok);

               Push_Back_Check
                 (New_Config,
                  (+SEMICOLON_ID,
                   (if Config.Error_Token.ID = +block_statement_ID
                    then +identifier_opt_ID
                    else +name_opt_ID),
                   +END_ID, +handled_sequence_of_statements_ID));

               Insert (New_Config, +BEGIN_ID);

               if Trace_McKenzie > Detail then
                  Put ("Semantic_Check Missing_Name_Error 1a " & Image (Config.Error_Token.ID, Descriptor),
                       New_Config.all);
                  if Trace_McKenzie > Extra then
                     Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                  end if;
               end if;
            end;
         end if;
         return Continue; -- 'ignore error'.

      when Extra_Name_Error =>
         --  The input looks like
         --
         --  "<begin_name_token> ... block_label_opt begin ... end <end_name_token> ;"
         --
         --  where the erroneous reduce matches the empty 'block_label_opt'
         --  with '<end_name_Token>'.
         --
         --  0. If a matching <begin_name_token> is found, this is not a
         --  plausible user name error; return Abandon. If it is not found, the
         --  user could be adding/deleting names; return Continue.
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

            End_Name : constant String := To_Lower (Lexer.Buffer_Text (End_Name_Token.Name));

            Token_Count : constant SAL.Peek_Type := SAL.Peek_Type (Config.Check_Token_Count);

            Matching_Name_Index : SAL.Peek_Type := Token_Count;
            --  'block_label_opt' is empty; start on token before 'begin'.

            Other_Tokens : constant Grammar_Token_ID_Set_Array := (1 => Begin_Name_IDs, 2 => Begin_IDs);
            Other_Counts : Natural_Array (1 .. 2);
         begin
            Find_Matching_Name
              (Config, Lexer, End_Name, Matching_Name_Index, Other_Tokens, Other_Counts, Case_Insensitive => True);

            if Matching_Name_Index > Config.Stack.Depth then
               --  case 0
               return Continue;
            end if;

            if Other_Counts (1) = 0 or Other_Counts (2) = 0 then
               raise Programmer_Error with "unrecognized Extra_Name_Error case " & Image
                 (Config.Error_Token, Descriptor);
            end if;

            if Other_Counts (1) > Other_Counts (2) then
               --  Case 1
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
                  Ops        : Config_Op_Arrays.Vector renames New_Config.Ops;
               begin
                  --  We don't increase the cost, because this is not a guess.
                  New_Config.Error_Token.ID := Invalid_Token_ID;
                  New_Config.Check_Status   := (Label => Ok);

                  --  Push_Back the failed reduce tokens.
                  for I in 1 .. New_Config.Check_Token_Count loop
                     declare
                        Item : constant Recover_Stack_Item := New_Config.Stack.Pop;
                     begin
                        Ops.Append ((Push_Back, Item.Token.ID, Item.Token.Min_Terminal_Index));
                     end;
                  end loop;
                  New_Config.Current_Shared_Token := New_Config.Error_Token.Min_Terminal_Index;

                  Insert (New_Config, (+END_ID, +SEMICOLON_ID));

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Extra_Name_Error 1 " & Image
                            (Config.Error_Token.ID, Descriptor), New_Config.all);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;

            else
               --  Case 2
               declare
                  New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
               begin
                  --  We don't increase the cost, because this is not a guess.

                  New_Config.Error_Token.ID := Invalid_Token_ID;
                  New_Config.Check_Status   := (Label => Ok);

                  Push_Back_Check
                    (New_Config,
                     (+SEMICOLON_ID,
                     (if Config.Error_Token.ID = +block_statement_ID
                      then +identifier_opt_ID
                      else +name_opt_ID),
                      +END_ID));

                  Insert (New_Config, (+END_ID, +SEMICOLON_ID));

                  if Trace_McKenzie > Detail then
                     Put ("Semantic_Check Extra_Name_Error 2 " & Image
                            (Config.Error_Token.ID, Descriptor), New_Config.all);
                     if Trace_McKenzie > Extra then
                        Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                     end if;
                  end if;
               end;
            end if;
            return Abandon;
         end;
      end case;
   end Handle_Check_Fail;

   function Handle_Parse_Error
     (Trace             : in out WisiToken.Trace'Class;
      Parser_Label      : in     Natural;
      Terminals         : in     Base_Token_Arrays.Vector;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
     return Non_Success_Status
   with Pre => Config.Check_Status.Label = Ok
   is
      use all type SAL.Base_Peek_Type;
      procedure Put (Message : in String; Config : in Configuration)
      is begin
         Put (Message, Trace, Parser_Label, Terminals, Config);
      end Put;

      procedure Check_Ops_Insert_Point
      is begin
         if Config.Ops_Insert_Point /= Config_Op_Arrays.No_Index then
            if Trace_McKenzie > Outline then
               Put ("Handle_Parse_Error test case for Ops_Insert_Point", Config);
            end if;
            raise Programmer_Error with
              "Handle_Parse_Error found test case for Ada_Lite Language_Fixes Ops_Insert_Point.";
         end if;
      end Check_Ops_Insert_Point;

   begin
      --  This is simple enough to use 'case'; full Ada needs 'if'.
      case Standard.Ada_Lite.Token_Enum_ID'(-Config.Error_Token.ID) is
      when DOT_ID =>
         --  We've encountered a Selected_Component when we were expecting a
         --  simple IDENTIFIER or a name. If the name is preceded by 'end', then
         --  this similar to a semantic check Extra_Name_Error, and the
         --  solutions are similar.

         if Config.Stack (1).Token.ID = +IDENTIFIER_ID and
           Config.Stack (2).Token.ID = +END_ID
         then
            Check_Ops_Insert_Point;

            --  The input looks like
            --
            --  "<begin_name_token_1> ... <begin_name_token_2> ... begin ... begin ... end <end_name_token_1> ;"
            --
            --  'end <end_name_token_2> ;' is missing between the 'begin's, so
            --  parsing expects <end_name_token_1> to match the second 'begin',
            --  which looks like an unnamed block. See test_mckenzie_recover
            --  Match_Selected_Component_1.
            --
            --  The solution is to insert 'end ;' before the second 'begin'.
            --
            --  Note that it's _not_ possible the user is just editing names; that
            --  would generate a semantic check fail, not a parse table error,
            --  since a "." would be permitted.

            declare
               Label      : constant String               := "selected_component 1";
               New_Config : constant Configuration_Access := Local_Config_Heap.Add (Config);
            begin
               New_Config.Error_Token.ID := Invalid_Token_ID;

               Push_Back_Check
                 (New_Config,
                  (+IDENTIFIER_ID, +END_ID, +handled_sequence_of_statements_ID, +BEGIN_ID, +block_label_opt_ID));

               Insert (New_Config, (+END_ID, +SEMICOLON_ID));

               if Trace_McKenzie > Detail then
                  Put ("Language_Fixes selected_component 1 " & Image (Config.Error_Token.ID, Descriptor),
                       New_Config.all);
                  if Trace_McKenzie > Extra then
                     Trace.Put_Line ("config stack: " & Image (New_Config.Stack, Descriptor));
                  end if;
               end if;
               return Abandon;
            exception
            when System.Assertions.Assert_Failure =>
               --  From *_Check
               Put ("Language_Fixes " & Label & " ID mismatch " & Image (Config.Error_Token.ID, Descriptor), Config);
               Trace.Put_Line ("... new_config stack: " & Image (New_Config.Stack, Descriptor));

               --  We don't re-raise the exception here; so far, this has only
               --  happened while exploring a high-cost config (which only occurs in
               --  a race condition with the lower cost solution), and the correct
               --  thing to do would be to abandon it. If we are trying to fix a
               --  particular use case, the trace messages will be enough.
               return Abandon;
            end;
         end if;

      when others =>
         null;
      end case;
      return Continue;
   end Handle_Parse_Error;

   function Member (ID : in Token_ID; Item : in Token_ID_Arrays.Vector) return Boolean
   is begin
      for I of Item loop
         if I = ID then return True; end if;
      end loop;
      return False;
   end Member;

   ----------
   --  Public subprograms

   function Language_Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : in     WisiToken.Lexer.Handle;
      Parser_Label      : in     Natural;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
     return Non_Success_Status
   is begin
      if Trace_McKenzie > Extra then
         Put ("Ada_Lite Language_Fixes", Trace, Parser_Label, Terminals, Config);
         Put_Line (Trace, Parser_Label, "config stack: " & Image (Config.Stack, Descriptor));
      end if;

      case Config.Check_Status.Label is
      when Ok =>
         return Handle_Parse_Error (Trace, Parser_Label, Terminals, Local_Config_Heap, Config);

      when others =>
         return Handle_Check_Fail (Trace, Lexer, Parser_Label, Terminals, Tree, Local_Config_Heap, Config);
      end case;
   end Language_Fixes;

   function Constrain_Terminals
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Table        : in     Parse_Table;
      Config       : in     Configuration)
     return WisiToken.Token_ID_Set
   is
      All_Ok : constant WisiToken.Token_ID_Set := (Table.First_Terminal .. Table.Last_Terminal => True);
   begin
      if Config.Error_Token.ID = Invalid_Token_ID then
         --  no error
         return All_Ok;
      end if;

      case Standard.Ada_Lite.Token_Enum_ID'(-Config.Error_Token.ID) is
      when END_ID =>
         declare
            Temp_Config : Configuration := Config;
            Result      : WisiToken.Token_ID_Set  := (Table.First_Terminal .. Table.Last_Terminal => False);
         begin
            Reduce_To_Shift :
            loop
               declare
                  use all type Ada.Containers.Count_Type;
                  State       : State_Index renames Temp_Config.Stack (1).State;
                  Shift_Count : Integer;
                  Reductions  : constant Reduce_Action_Array := Table.Reductions (State, Shift_Count);
               begin
                  if Reductions'Length = 1 and then Reductions (1).Token_Count > 0 then
                     declare
                        Action : Reduce_Action_Rec renames Reductions (1);
                     begin
                        Temp_Config.Stack.Pop (SAL.Base_Peek_Type (Action.Token_Count));
                        Temp_Config.Stack.Push
                          ((Goto_For (Table, Temp_Config.Stack (1).State, Action.LHS),
                            Syntax_Trees.Invalid_Node_Index,
                            (Action.LHS, others => <>)));
                     end;

                  elsif Shift_Count > 0 then
                     declare
                        Item        : Recover_Stack_Item renames Temp_Config.Stack (1);
                        Table_Entry : Parse_State renames Table.States (Item.State);
                        I           : Action_List_Iterator                 := First (Table_Entry);
                        Prods       : constant Production_ID_Arrays.Vector := Table.States (Item.State).Productions;
                        LHS         : constant Token_ID                    := Table.Productions (Prods (1)).LHS;
                        One_LHS     : Boolean                              := True;
                     begin
                        for P of Prods loop
                           if LHS /= Table.Productions (P).LHS then
                              One_LHS := False;
                           end if;
                        end loop;

                        if One_LHS then
                           To_Set (Table.Terminal_Sequences (LHS), Result);
                           --  FIXME: merge multiple sequences?
                        else
                           loop
                              exit when Is_Done (I);
                              if I.Action.Verb = Shift then
                                 declare
                                    Action : Parse_Action_Rec renames I.Action;
                                 begin
                                    for J of Action.Productions loop
                                       if Member
                                         (I.Symbol, Table.Terminal_Sequences (Table.Productions (J).LHS))
                                       then
                                          --  FIXME: change terminal_sequences to set?
                                          --  or find position in it?
                                          Result (I.Symbol) := True;
                                       end if;
                                    end loop;
                                 end;
                              end if;

                              Next (I);
                           end loop;
                        end if;
                     end;
                     exit Reduce_To_Shift;
                  else
                     --  FIXME: conflicts
                     raise Programmer_Error with "conflicts in Constrain_Terminals; state" & State_Index'Image (State);
                  end if;
               end;
            end loop Reduce_To_Shift;

            if Trace_McKenzie > Detail then
               Put_Line (Trace, Parser_Label, "constrain_terminals: " & Image (Result, Trace.Descriptor.all));
            end if;
            return Result;
         end;

      when others =>
         return All_Ok;
      end case;
   end Constrain_Terminals;

end WisiToken.LR.McKenzie_Recover.Ada_Lite;
