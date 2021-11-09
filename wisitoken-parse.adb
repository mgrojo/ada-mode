--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2021 Free Software Foundation, Inc.
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

package body WisiToken.Parse is

   --  Body subprograms

   procedure Process_Grammar_Token
     (Parser : in out Base_Parser'Class;
      Token  : in     Lexer.Token;
      Node   : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Syntax_Trees.User_Data_Access;
   begin
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Node);
      end if;
   end Process_Grammar_Token;

   procedure Process_Non_Grammar_Token
     (Parser       : in out Base_Parser'Class;
      Grammar_Node : in     Syntax_Trees.Valid_Node_Access;
      Token        : in     Lexer.Token)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Tree.Non_Grammar_Var (Grammar_Node).Append (Token);
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Grammar_Node);
      end if;
   end Process_Non_Grammar_Token;

   ----------
   --  Package public subprograms, declaration order

   overriding function Dispatch_Equal (Left : in Lexer_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is
      use all type WisiToken.Lexer.Error;
   begin
      return Right in Lexer_Error and then Left.Error = Lexer_Error (Right).Error;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node),
         Recover_Ops  => <>,
         Recover_Cost => 0);
   end To_Message;

   overriding function Image
     (Data       : in Lexer_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      Append (Result, "lexer error:" & Data.Error.Char_Pos'Image & ", '");
      for C of Data.Error.Recover_Char loop
         if C /= ASCII.NUL then
            Append (Result, C);
         end if;
      end loop;
      Append (Result, "'");
      return To_String (Result);
   end Image;

   overriding function Dispatch_Equal (Left : in Parse_Error; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is begin
      if not (Right in Parse_Error) then
         return False;
      else
         declare
            Right_Parse : Parse_Error renames Parse_Error (Right);
         begin
            --  Allow updating recover info after error recovery; current value
            --  may have no or different recovery information, so don't check
            --  that.
            return Left.Expecting = Right_Parse.Expecting;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node), Data.Recover_Ops, Data.Recover_Cost);
   end To_Message;

   overriding function Image
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.Valid_Node_Access;

      Item_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Error_Node);
      Msg : constant String :=
        "syntax_error: expecting " & Image (Data.Expecting, Tree.Lexer.Descriptor.all) &
        ", found '" & Tree.Lexer.Buffer_Text (Item_Byte_Region) &
        "'";
   begin
      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         return Msg & ASCII.LF & "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all);
      else
         return Msg;
      end if;
   end Image;

   overriding function Dispatch_Equal
     (Left  : in In_Parse_Action_Error;
      Right : in Syntax_Trees.Error_Data'Class)
     return Boolean
   is begin
      if not (Right in In_Parse_Action_Error) then
         return False;
      else
         declare
            use all type WisiToken.In_Parse_Actions.Status;
            Right_In_Parse : In_Parse_Action_Error renames In_Parse_Action_Error (Right);
         begin
            --  Allow updating recover info after error recovery.
            return Left.Status = Right_In_Parse.Status;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is begin
      return Error_Message'
        (+Image (Data, Tree, Error_Node), Data.Recover_Ops, Data.Recover_Cost);
   end To_Message;

   overriding function Image
     (Data       : in In_Parse_Action_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Ada.Strings.Unbounded;
      use all type Ada.Containers.Count_Type;

      Result : Unbounded_String;
   begin
      Result := +"in_parse_action_error: " & In_Parse_Actions.Image (Data.Status, Tree, Error_Node);

      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         Append (Result, ASCII.LF & "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all));
      end if;

      return -Result;
   end Image;

   overriding function Dispatch_Equal (Left : in Error_Message; Right : in Syntax_Trees.Error_Data'Class) return Boolean
   is begin
      if not (Right in Error_Message) then
         return False;
      else
         declare
            use all type Ada.Strings.Unbounded.Unbounded_String;
            Right_Message : Error_Message renames Error_Message (Right);
         begin
            --  Allow updating recover info after error recovery.
            return Left.Msg = Right_Message.Msg;
         end;
      end if;
   end Dispatch_Equal;

   overriding function To_Message
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      pragma Unreferenced (Tree, Error_Node);
   begin
      return Data;
   end To_Message;

   overriding function Image
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is begin
      return "message: " & (-Data.Msg);
   end Image;

   function Error_Pred_Parse (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Parse_Error then True
         else False);
   end Error_Pred_Parse;

   function Error_Pred_Lexer_Parse_Message (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Lexer_Error then False
         --  Lexer errors are only cleared by re-lexing in Edit_Tree.
         --  test_incremental.adb Lexer_Errors_1

         elsif Element (Cur) in Parse_Error then True
         --  A previous Parse_Error; test_incremental.adb Recover_1,
         --  test_incremental.adb Multiple_Errors_On_One_Token_1, _2,
         --  ada_mode-interactive_06.adb

         elsif Element (Cur) in Error_Message then True
         --  A moved In_Parse_Error.

         else raise SAL.Programmer_Error);
   end Error_Pred_Lexer_Parse_Message;

   function Find_Parse_In_Parse_Action_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      use Syntax_Trees;
      Found : Boolean := False;
   begin
      --  test_mckenzie_recover.adb String_Quote_0 has lexer and parse error
      --  on same node. There should only be one Parse or In_Parse_Action
      --  error on a node.
      for Err of Tree.Error_List (Node) loop
         if Err in Parse_Error or Err in In_Parse_Action_Error then
            Found := True;
         end if;
      end loop;

      if not Found then
         raise SAL.Programmer_Error;
      end if;

      for Err of Tree.Error_List (Node) loop
         if Err in Parse_Error or Err in In_Parse_Action_Error then
            return Err;
         end if;
      end loop;

      raise SAL.Programmer_Error; -- keep the compiler happy
   end Find_Parse_In_Parse_Action_Error;

   function Find_Non_Lexer_Error
     (Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Syntax_Trees.Error_Data'Class
   is
      use Syntax_Trees;
      Found : Boolean := False;
   begin
      --  FIXME: use a Cursor
      for Err of Tree.Error_List (Node) loop
         if not (Err in Lexer_Error) then
            Found := True;
         end if;
      end loop;

      if not Found then
         raise SAL.Programmer_Error;
      end if;

      for Err of Tree.Error_List (Node) loop
         if not (Err in Lexer_Error) then
            return Err;
         end if;
      end loop;

      raise SAL.Programmer_Error; -- keep the compiler happy
   end Find_Non_Lexer_Error;

   function Next_Grammar_Token
     (Parser            : in out Base_Parser'Class;
      Last_Grammar_Node : in out WisiToken.Syntax_Trees.Node_Access)
     return Token_ID
   is
      use Syntax_Trees;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;

      Token : WisiToken.Lexer.Token;
      Error : Boolean;
      Ref   : Terminal_Ref;
   begin
      loop
         Error := Lexer.Find_Next (Token);

         if Trace_Lexer > Outline then
            Parser.Trace.Put_Line (WisiToken.Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
         end if;

         if Token.ID >= Lexer.Descriptor.First_Terminal then
            Ref := Tree.Add_Terminal
              (Parser.Tree.Shared_Stream, Token,
               Error =>
                 (if Error
                  then Lexer_Error'(Error => Lexer.Errors (Lexer.Errors.Last))
                  else No_Error));

            Process_Grammar_Token (Parser, Token, Ref.Node);
            Last_Grammar_Node := Ref.Node;
         else
            if Trace_Lexer > Detail then
               Parser.Trace.Put_Line ("non-grammar in " & Parser.Tree.Image (Last_Grammar_Node));
            end if;
            if Error then
               raise SAL.Programmer_Error with "lexer error in non_grammar";
            end if;

            Process_Non_Grammar_Token (Parser, Last_Grammar_Node, Token);
            Ref := Invalid_Stream_Node_Ref;
         end if;

         exit when Token.ID >= Lexer.Descriptor.First_Terminal;
      end loop;

      return Token.ID;
   end Next_Grammar_Token;

   procedure Lex_All (Parser : in out Base_Parser'Class)
   is
      EOI_ID : constant Token_ID := Parser.Tree.Lexer.Descriptor.EOI_ID;

      Last_Grammar_Node : WisiToken.Syntax_Trees.Node_Access;
   begin
      Parser.Tree.Start_Lex;

      Last_Grammar_Node := Parser.Tree.SOI;

      loop
         exit when EOI_ID = Next_Grammar_Token (Parser, Last_Grammar_Node);
      end loop;
      if Trace_Parse > Outline then
         Parser.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Last_Grammar_Node)'Image & " tokens lexed");
      end if;

   end Lex_All;

   function Equal (Left : in Recover_Op; Right : in Insert_Op) return Boolean
   is
      use all type WisiToken.Syntax_Trees.Sequential_Index;
   begin
      return Left.Op = Insert and then
        Left.Ins_ID = Right.Ins_ID and then
        Left.Ins_Before = Right.Ins_Before;
   end Equal;

   function None (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
   begin
      for I in First_Index (Ops) .. Last_Index (Ops) loop
         if Constant_Ref (Ops, I).Op = Op then
            return False;
         end if;
      end loop;
      return True;
   end None;

   function None_Since_FF (Ops : aliased in Recover_Op_Arrays.Vector; Op : in Recover_Op_Label) return Boolean
   is
      use Recover_Op_Arrays, Recover_Op_Array_Refs;
   begin
      for I in reverse First_Index (Ops) .. Last_Index (Ops) loop
         declare
            O : Recover_Op renames Constant_Ref (Ops, I);
         begin
            exit when O.Op = Fast_Forward;
            if O.Op = Op then
               return False;
            end if;
         end;
      end loop;
      return True;
   end None_Since_FF;

   function Image (KMN : in WisiToken.Parse.KMN) return String
   is begin
      return "(" & KMN.Stable_Bytes'Image & "," &
        KMN.Stable_Chars'Image & "," &
        KMN.Inserted_Bytes'Image & "," &
        KMN.Inserted_Chars'Image & "," &
        KMN.Deleted_Bytes'Image & "," &
        KMN.Deleted_Chars'Image & ")";
   end Image;

   procedure Validate_KMN
     (KMN                       : in WisiToken.Parse.KMN;
      Initial_Stable_Byte_First : in Buffer_Pos;
      Initial_Stable_Char_First : in Buffer_Pos;
      Edited_Stable_Byte_First  : in Buffer_Pos;
      Edited_Stable_Char_First  : in Buffer_Pos;
      Initial_Text_Byte_Region  : in Buffer_Region;
      Initial_Text_Char_Region  : in Buffer_Region;
      Edited_Text_Byte_Region   : in Buffer_Region;
      Edited_Text_Char_Region   : in Buffer_Region)
   is
      Stable_Byte_Region : constant Buffer_Region :=
        (Initial_Stable_Byte_First, Initial_Stable_Byte_First + KMN.Stable_Bytes - 1);
      Stable_Char_Region : constant Buffer_Region :=
        (Initial_Stable_Char_First, Initial_Stable_Char_First + KMN.Stable_Chars - 1);

      Inserted_Byte_Region : constant Buffer_Region :=
        (Edited_Stable_Byte_First + KMN.Stable_Bytes,
         Edited_Stable_Byte_First + KMN.Stable_Bytes + KMN.Inserted_Bytes - 1);
      Inserted_Char_Region : constant Buffer_Region :=
        (Edited_Stable_Char_First + KMN.Stable_Chars,
         Edited_Stable_Char_First + KMN.Stable_Chars + KMN.Inserted_Chars - 1);

      Deleted_Byte_Region : constant Buffer_Region :=
        (Stable_Byte_Region.Last + 1, Stable_Byte_Region.Last + KMN.Deleted_Bytes);
      Deleted_Char_Region : constant Buffer_Region :=
        (Stable_Char_Region.Last + 1, Stable_Char_Region.Last + KMN.Deleted_Chars);
   begin
      if not Contains (Outer => Initial_Text_Byte_Region, Inner => Stable_Byte_Region) then
         raise User_Error with "KMN stable byte region outside initial source text";
      end if;
      if not Contains (Outer => Initial_Text_Char_Region, Inner => Stable_Char_Region) then
         raise User_Error with "KMN stable char region outside initial source text";
      end if;

      if KMN.Inserted_Bytes > 0 then
         if not Contains (Outer => Edited_Text_Byte_Region, Inner => Inserted_Byte_Region) then
            raise User_Error with "KMN inserted byte region outside initial source text";
         end if;
         if not Contains (Outer => Edited_Text_Char_Region, Inner => Inserted_Char_Region) then
            raise User_Error with "KMN inserted char region outside edited source text";
         end if;
      end if;


      if KMN.Deleted_Bytes > 0 then
         if not Contains (Outer => Initial_Text_Byte_Region, Inner => Deleted_Byte_Region) then
            raise User_Error with "KMN deleted byte region outside initial source text";
         end if;
         if not Contains (Outer => Initial_Text_Char_Region, Inner => Deleted_Char_Region) then
            raise User_Error with "KMN deleted char region outside initial source text";
         end if;
      end if;
   end Validate_KMN;

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region)
   is
      Initial_Byte_First : Base_Buffer_Pos := Initial_Text_Byte_Region.First;
      Initial_Char_First : Base_Buffer_Pos := Initial_Text_Char_Region.First;
      Edited_Byte_First  : Base_Buffer_Pos := Edited_Text_Byte_Region.First;
      Edited_Char_First  : Base_Buffer_Pos := Edited_Text_Char_Region.First;
   begin
      for KMN of List loop
         Validate_KMN
           (KMN,
            Initial_Stable_Byte_First => Initial_Byte_First,
            Initial_Stable_Char_First => Initial_Char_First,
            Edited_Stable_Byte_First  => Edited_Byte_First,
            Edited_Stable_Char_First  => Edited_Char_First,
            Initial_Text_Byte_Region  => Initial_Text_Byte_Region,
            Initial_Text_Char_Region  => Initial_Text_Char_Region,
            Edited_Text_Byte_Region   => Edited_Text_Byte_Region,
            Edited_Text_Char_Region   => Edited_Text_Char_Region);

         Initial_Byte_First := @ + KMN.Stable_Bytes + KMN.Deleted_Bytes;
         Initial_Char_First := @ + KMN.Stable_Chars + KMN.Deleted_Chars;
         Edited_Byte_First  := @ + KMN.Stable_Bytes + KMN.Inserted_Bytes;
         Edited_Char_First  := @ + KMN.Stable_Chars + KMN.Inserted_Chars;
      end loop;

      if Initial_Byte_First - 1 /= Initial_Text_Byte_Region.Last then
         raise User_Error with "KMN list (deleted last" & Base_Buffer_Pos'Image (Initial_Byte_First - 1) &
           ") does not match initial text (last" & Initial_Text_Byte_Region.Last'Image & ")";
      end if;
      if Edited_Byte_First - 1 /= Edited_Text_Byte_Region.Last then
         raise User_Error with "KMN list (inserted last" & Base_Buffer_Pos'Image (Edited_Byte_First - 1) &
           ") does not match edited text (last" & Edited_Text_Byte_Region.Last'Image & ")";
      end if;
   end Validate_KMN;

   procedure Edit_Tree
     (Parser : in out Base_Parser'Class;
      Edits  : in     KMN_Lists.List)
   is
      --  Similar to [Lahav 2004] Algorithms 3, 4. That assumes creating a
      --  separate temp list of new tokens, and then merging that into the
      --  parse tree, is faster than merging new tokens in one by one; we
      --  just do the latter. We also don't modify the edit list.
      --
      --  Parser.Lexer contains the edited text; the initial text is not
      --  available.
      use WisiToken.Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type KMN_Lists.Cursor;

      Tree : Syntax_Trees.Tree renames Parser.Tree;

      KMN_Node         : KMN_Lists.Cursor := Edits.First;
      Old_Byte_Pos     : Base_Buffer_Pos  := 0;
      Old_Char_Pos     : Base_Buffer_Pos  := 0;
      New_Byte_Pos     : Base_Buffer_Pos  := 0;
      New_Char_Pos     : Base_Buffer_Pos  := 0;
      Scanned_Byte_Pos : Base_Buffer_Pos  := 0; -- Last pos scanned by lexer
      Shift_Bytes      : Base_Buffer_Pos  := 0;
      Shift_Chars      : Base_Buffer_Pos  := 0;

      Shift_Lines : Base_Line_Number_Type := 0;
      --  Whenever a non_grammar is deleted from Tree (either permanently,
      --  or moved to Floating_Non_Grammar), Shift_Lines is decremented by
      --  New_Line_Count (non_grammar_token). Then if a token is restored
      --  from Floating_Non_Grammar to the Tree, Shift_Lines is incremented;
      --  if a token is deleted from Floating_Non_Grammar, Shift_Lines is
      --  not changed.

      Floating_Non_Grammar : Lexer.Token_Arrays.Vector;
      --  Non_grammar that are detached from a node (for various reasons).
      --  These are not shifted, because the correct shift is
      --  unknown at the time they are detached. ada_mode-recover_42.adb.
      --
      --  If a non_grammar is floated from a scanned node, it is unshifted
      --  to be consistent.

      Delayed_Scan           : Boolean             := False;
      Delayed_Floating_Index : Positive_Index_Type := Positive_Index_Type'Last;
      Delayed_Lex_Start_Byte : Buffer_Pos          := Buffer_Pos'Last;
      Delayed_Lex_Start_Char : Buffer_Pos          := Buffer_Pos'Last;
      Delayed_Lex_Start_Line : Line_Number_Type    := Line_Number_Type'Last;
      --  When multiple edits occur in a non_grammar token, the last one may
      --  insert or delete a comment end, so it is not possible to compute
      --  the correct scan end when handling the first edit. So the scan is
      --  delayed.

      Comment_End_Deleted : Boolean         := False;
      New_Comment_End     : Base_Buffer_Pos := Invalid_Buffer_Pos;
      --  If Comment_End_Deleted, a comment end was deleted; delete all tree
      --  tokens thru New_Comment_End (which is not shifted).

      Comment_Start_Deleted : Boolean         := False;
      Comment_End_Inserted  : Boolean         := False;
      New_Code_End          : Base_Buffer_Pos := Invalid_Buffer_Pos;
      --  If Comment_End_Inserted, a comment end was inserted; scan all
      --  exposed code thru New_Code_End (which is shifted).
      --  Similarly for Comment_Start_Deleted.

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;
      --  Node being considered for shift or delete.

      Terminal_Non_Grammar_Next : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
      --  Next non_grammar in Terminal to be shifted or deleted.

      procedure Breakdown (Terminal : in out Terminal_Ref; To_Single : in Boolean := False)
      is begin
         if Tree.Label (Terminal.Element) = Nonterm then
            if Trace_Incremental_Parse > Detail then
               Parser.Trace.Put_Line
                 ("breakdown " & (if To_Single then "single " else "") & Tree.Image
                    (Tree.Get_Node (Terminal.Stream, Terminal.Element), Node_Numbers => True) &
                    " target " & Tree.Image (Terminal.Node, Node_Numbers => True));
            end if;
            Tree.Breakdown (Terminal, Parser.User_Data);

            if To_Single and then Tree.Label (Terminal.Element) = Nonterm then
               Tree.Left_Breakdown (Terminal);
            end if;
            if Trace_Incremental_Parse > Extra then
               Parser.Trace.Put_Line
                 ("... result " & Tree.Image (Stream));
            end if;
         end if;
      end Breakdown;

   begin
      Tree.Start_Edit;

      if Edits.Length = 0 then
         return;
      end if;

      Stream := Tree.Shared_Stream;

      --  First undo all error recover insert/delete, in case this is needed
      --  as part of an edit in another place; test_incremental.adb
      --  Preserve_Parse_Errors_2.
      --
      --  FIXME: This algorithm visits every terminal; not incremental.
      --  Cache Has_Following_Deleted in nonterms.

      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));
      Undo_Recover :
      loop
         declare
            Next_Terminal_Done : Boolean := True;
         begin
            Next_Recover :
            loop
               if Next_Terminal_Done then
                  Next_Terminal_Done := False;
               else
                  Tree.Next_Terminal (Terminal);
               end if;
               exit Undo_Recover when Terminal.Node = Invalid_Node_Access;
               exit Next_Recover when Tree.Label (Terminal.Node) in Virtual_Terminal;
               exit Next_Recover when Tree.Label (Terminal.Node) = Source_Terminal and then
                 Tree.Has_Following_Deleted (Terminal.Node);
            end loop Next_Recover;

            case Terminal_Label'(Tree.Label (Terminal.Node)) is
            when Source_Terminal =>
               declare
                  --  Don't restore before a virtual terminal; they are deleted as part
                  --  of this loop. ada_mode-interactive_01.adb
                  Insert_Before : Terminal_Ref := Terminal;
               begin
                  Tree.Next_Source_Terminal (Insert_Before, Trailing_Non_Grammar => False);

                  loop
                     exit when Terminal.Element /= Insert_Before.Element;
                     pragma Assert (Terminal.Node /= Insert_Before.Node);
                     if Terminal.Node = Tree.First_Terminal (Get_Node (Terminal.Element)) then
                        --  test_incremental.adb Modify_Deleted_Element
                        Tree.Prev_Terminal (Terminal);
                        Breakdown (Insert_Before);
                        Tree.Next_Terminal (Terminal);
                        Next_Terminal_Done := True;
                     else
                        Breakdown (Terminal);
                        Insert_Before := Terminal;
                        Tree.Next_Source_Terminal (Insert_Before, Trailing_Non_Grammar => False);
                     end if;
                  end loop;

                  Breakdown (Insert_Before);

                  for Deleted_Node of reverse Tree.Following_Deleted (Terminal.Node) loop
                     if Tree.Label (Deleted_Node) in Virtual_Terminal_Label then
                        --  This would be deleted in the next step, so don't bother restoring
                        --  it.
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("drop virtual deleted node " & Tree.Image (Deleted_Node, Node_Numbers => True));
                        end if;

                     else
                        declare
                           Deleted_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Deleted_Node);

                           Terminal_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var
                             (Terminal.Node);
                           First_To_Move : Positive_Index_Type := Positive_Index_Type'Last;
                           Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Deleted_Node);
                           pragma Assert (Deleted_Non_Grammar.Length = 0);
                        begin
                           if Terminal_Non_Grammar.Length > 0 and then
                             Deleted_Byte_Region.First < Terminal_Non_Grammar
                               (Terminal_Non_Grammar.Last_Index).Byte_Region.Last
                           then
                              --  Move some Terminal_Non_Grammar to Deleted_Non_Grammar
                              --  test_incremental.adb Modify_Deleted_Element, Lexer_Errors_1,
                              --  Restore_Deleted_01
                              for I in Terminal_Non_Grammar.First_Index .. Terminal_Non_Grammar.Last_Index loop
                                 if Deleted_Byte_Region.First < Terminal_Non_Grammar (I).Byte_Region.Last then
                                    First_To_Move := I;
                                    exit;
                                 end if;
                              end loop;
                              for I in First_To_Move .. Terminal_Non_Grammar.Last_Index loop
                                 Deleted_Non_Grammar.Append (Terminal_Non_Grammar (I));
                              end loop;
                              if First_To_Move = Terminal_Non_Grammar.First_Index then
                                 Terminal_Non_Grammar.Clear;
                              else
                                 Terminal_Non_Grammar.Set_First_Last
                                   (Terminal_Non_Grammar.First_Index, First_To_Move - 1);
                              end if;
                           end if;
                        end;

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("restore deleted node " & Tree.Image
                                (Deleted_Node, Node_Numbers => True, Non_Grammar => True) &
                                " before " & Tree.Image
                                  (Insert_Before, Node_Numbers => True, Non_Grammar => True));
                        end if;

                        Tree.Set_Sequential_Index (Deleted_Node, Invalid_Sequential_Index);
                        Insert_Before := Tree.Stream_Insert (Stream, Deleted_Node, Insert_Before.Element);

                        if Trace_Incremental_Parse > Extra then
                           Parser.Trace.Put_Line
                             ("stream:" & Tree.Image
                                (Stream,
                                 Children    => Trace_Incremental_Parse > Detail,
                                 Non_Grammar => True,
                                 Augmented   => True));
                        end if;
                     end if;
                  end loop;
                  Tree.Following_Deleted (Terminal.Node).Clear;
               end;

            when Virtual_Terminal_Label =>
               --  Delete Terminal.
               Breakdown (Terminal, To_Single => True);

               declare
                  Terminal_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const (Terminal.Node);
                  Prev_Non_Terminal    : constant Stream_Node_Ref := Tree.Prev_Terminal (Terminal);
                  Next_Non_Terminal    : constant Stream_Node_Ref := Tree.Next_Terminal (Terminal);
                  Next_Byte_Region     : constant Buffer_Region   := Tree.Byte_Region (Next_Non_Terminal.Node);

                  To_Delete    : Stream_Index := Terminal.Element;
                  Next_Element : Stream_Index := Tree.Stream_Next (Terminal.Stream, Terminal.Element);
               begin
                  --  Terminal_Non_Grammar is non-empty only if User_Data.Insert_Token
                  --  moved some non_grammar to it. If the terminal they were moved from
                  --  was subsequently deleted, it may now be Next_Non_Terminal.
                  --  ada_mode-interactive_09.adb new_line after 'for'.
                  if (for some N of Terminal_Non_Grammar => Next_Byte_Region.First < N.Byte_Region.Last) then
                     Tree.Non_Grammar_Var (Next_Non_Terminal.Node).Append (Terminal_Non_Grammar);
                  else
                     Tree.Non_Grammar_Var (Prev_Non_Terminal.Node).Append (Terminal_Non_Grammar);
                  end if;

                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line
                       ("delete virtual " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
                  end if;

                  Tree.Next_Terminal (Terminal);
                  Next_Terminal_Done := True;
                  Tree.Stream_Delete (Terminal.Stream, To_Delete);

                  --  Delete immediately following empty nonterms. For example, in Ada,
                  --  error recover often inserts 'end <name_opt> ;', where name_opt is
                  --  empty; delete all three tokens.
                  loop
                     exit when Next_Element = Invalid_Stream_Index;
                     declare
                        Node : constant Valid_Node_Access := Tree.Get_Node (Terminal.Stream, Next_Element);
                        To_Delete : Stream_Index;
                     begin
                        if Tree.Label (Node) = Nonterm and then Tree.Child_Count (Node) = 0 then
                           To_Delete := Next_Element;

                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("delete empty nonterm " & Tree.Image
                                   (To_Delete, Node_Numbers => True, Non_Grammar => True));
                           end if;

                           Next_Element := Tree.Stream_Next (Terminal.Stream, @);
                           Tree.Stream_Delete (Terminal.Stream, To_Delete);
                        else
                           exit;
                        end if;
                     end;
                  end loop;
               end;
            end case;
         end;
      end loop Undo_Recover;

      --  Now process source edits.
      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream));

      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Edits (KMN_Node);

            Stable_Region : constant Buffer_Region :=
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Stable_Region_Chars : constant Buffer_Region :=
              (Old_Char_Pos + 1, Old_Char_Pos + KMN.Stable_Chars);

            Deleted_Region : constant Buffer_Region :=
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);
            --  Inserted_Region.First is the first char after the stable region in
            --  the edited text.
            --
            --  If Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
            --  then this is the final stable region

            Inserted_Region_Chars : constant Buffer_Region :=
              (New_Char_Pos + KMN.Stable_Chars + 1, New_Char_Pos + KMN.Stable_Chars + KMN.Inserted_Chars);

            Next_KMN : constant WisiToken.Parse.KMN :=
              (if KMN_Node = Edits.Last
               then Invalid_KMN
               else Edits (KMN_Lists.Next (KMN_Node)));

            Next_KMN_Stable_First : constant Buffer_Pos := Stable_Region.Last + KMN.Deleted_Bytes + 1;
            Next_KMN_Stable_Last  : constant Buffer_Pos := Next_KMN_Stable_First - 1 + Next_KMN.Stable_Bytes;

         begin
            --  Parser.Lexer contains the edited text, so we can't check that
            --  stable, deleted are inside the initial text. Caller should use
            --  Validate_KMN.

            if Trace_Incremental_Parse > Detail then
               Parser.Trace.New_Line;
               Parser.Trace.Put_Line
                 ("KMN: " & Image (Stable_Region) & Image (Inserted_Region) & Image (Deleted_Region));
               Parser.Trace.Put_Line ("old  :" & Old_Byte_Pos'Image & Old_Char_Pos'Image);
               Parser.Trace.Put_Line
                 ("shift:" & Shift_Bytes'Image & " " & Shift_Chars'Image & " " & Shift_Lines'Image);
               Parser.Trace.Put_Line ("scanned_byte_pos:" & Scanned_Byte_Pos'Image);
               Parser.Trace.Put_Line
                 ("stream:" & Tree.Image
                    (Stream,
                     Children    => True,
                     Non_Grammar => True,
                     Augmented   => True));

               Parser.Trace.Put_Line ("terminal: " & Tree.Image (Terminal, Non_Grammar => True, Augmented => True));
               if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                  Parser.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
               end if;

               if Floating_Non_Grammar.Length > 0 then
                  Parser.Trace.Put_Line
                    ("floating_non_grammar: " & Lexer.Full_Image (Floating_Non_Grammar, Tree.Lexer.Descriptor.all));
               end if;

               Parser.Trace.New_Line;
            end if;

            if not Contains (Outer => Parser.Tree.Lexer.Buffer_Region_Byte, Inner => Inserted_Region) then
               raise User_Error with "KMN insert region " & Image (Inserted_Region) & " outside edited source text " &
                 Image (Parser.Tree.Lexer.Buffer_Region_Byte);
            end if;

            if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
               --  We only shift EOI after all KMN are processed; it may need to be
               --  shifted for more than one edit point. test_incremental.adb
               --  Edit_Comment_3.
               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line
                    ("nothing left to shift; terminal:" & Tree.Image
                       (Terminal, Non_Grammar => True, Augmented => True));
               end if;

            elsif not Delayed_Scan then
               --  It is tempting to skip Unchanged_Loop if Shift_Bytes = 0 and
               --  Shift_Chars = 0 and Shift_Lines = 0. But we need to scan all
               --  Non_Grammar for Floating_Non_Grammar, which changes Shift_Lines.
               --  FIXME: only need to scan trailing stable terminal?

               Unchanged_Loop :
               loop
                  exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
                  exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                     --  If there was a Delayed_Scan, some tokens may be before
                     --  Stable_Region. Exit when Terminal may be changed by the current
                     --  KMN edit; it is partly past or adjacent to Stable_Region.Last.
                     exit Unchanged_Loop when
                       (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                        then Tree.Byte_Region (Terminal.Node).Last > Stable_Region.Last
                        else Tree.Byte_Region (Terminal.Node).Last >= Stable_Region.Last);

                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("stable shift " & Tree.Image
                             (Terminal.Node,
                              Non_Grammar => True, Terminal_Node_Numbers => True, Augmented => True));
                     end if;
                     Tree.Shift
                       (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines, Stable_Region.Last,
                        Terminal_Non_Grammar_Next);

                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("  => " & Tree.Image
                             (Terminal.Node, Non_Grammar => True, Terminal_Node_Numbers => True,
                              Augmented => True));
                     end if;

                     if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line ("Terminal_Non_Grammar_Next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                        exit Unchanged_Loop;
                     end if;
                     Tree.Next_Terminal (Terminal);

                  else
                     --  The previous KMN left Terminal_Non_Grammar_Next /= No_Index
                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line ("Terminal_Non_Grammar_Next:" & Terminal_Non_Grammar_Next'Image);
                     end if;

                     --  Shift remaining non_grammar in Stable_Region
                     Non_Grammar_Loop :
                     loop
                        declare
                           Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        begin
                           exit Unchanged_Loop when Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.Last >
                             Stable_Region.Last;

                           Lexer.Shift (Non_Grammar (Terminal_Non_Grammar_Next), Shift_Bytes, Shift_Chars, Shift_Lines);

                           Terminal_Non_Grammar_Next := @ + 1;

                           if Terminal_Non_Grammar_Next = Non_Grammar.Last_Index then
                              Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                              Tree.Next_Terminal (Terminal);
                              exit Non_Grammar_Loop;
                           end if;
                        end;
                     end loop Non_Grammar_Loop;

                     if Trace_Incremental_Parse > Detail then
                        if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                           Parser.Trace.Put_Line ("Terminal_Non_Grammar_Next cleared");
                        else
                           Parser.Trace.Put_Line ("Terminal_Non_Grammar_Next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                     end if;
                  end if;
               end loop Unchanged_Loop;
            end if;

            --  Unchanged_Loop exited because Terminal or Terminal.Non_Grammar is
            --  at least partly out of Stable_Region or adjacent to the edit
            --  start, or because it reached Tree.EOI. Therefore the edit start is
            --  in Terminal, or in a non-grammar token or whitespace before
            --  Terminal (or after Tree.EOI).
            --
            --  If two edit regions affect the same token, scanning the first will
            --  also scan the second.
            declare
               Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                 (Terminal.Node, Trailing_Non_Grammar => Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index);

               Do_Scan        : Boolean          := False;
               Lex_Start_Byte : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Char : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Line : Line_Number_Type := Line_Number_Type'Last;

               Last_Grammar : Stream_Node_Ref :=
                 (if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index
                  then Tree.Prev_Terminal (Terminal) -- Shifted
                  else Terminal); -- Not shifted

               procedure Check_Comment_End (Token : in Lexer.Token)
               is begin
                  if KMN.Deleted_Bytes > 0 and Tree.Lexer.Is_Comment (Token.ID) then
                     if Token.Byte_Region.First < Deleted_Region.First and
                       Token.Byte_Region.Last <= Deleted_Region.Last
                     then
                        --  test_incremental.adb Edit_Comment_*, Delete_Comment_End,
                        --  ada_mode-interactive_05.adb Ada_Identifier in comment.
                        Comment_End_Deleted := True;

                        New_Comment_End := Tree.Lexer.Find_Comment_End (Token.ID, Token.Byte_Region.First);

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("comment_end_deleted:" &
                                Token.Byte_Region.First'Image & " .." &
                                New_Comment_End'Image);
                        end if;
                     end if;
                  end if;

                  if (KMN.Inserted_Bytes > 0 and
                        Tree.Lexer.Is_Comment (Token.ID) and
                        Inserted_Region.First < Token.Byte_Region.Last)
                    and then Tree.Lexer.Contains_New_Line (Inserted_Region)
                  then
                     --  test_incremental.adb Edit_Comment_5, ada_mode-interactive_02.adb
                     Comment_End_Inserted := True;
                     New_Code_End := KMN.Inserted_Bytes + Token.Byte_Region.Last
                       + (if Delayed_Scan then Shift_Bytes else 0);
                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("comment_end_inserted:" &
                             Token.Byte_Region.First'Image & " .." &
                             New_Code_End'Image);
                     end if;
                  end if;
               end Check_Comment_End;

            begin
               if Delayed_Scan then
                  --  A previous edit start affected Floating_Non_Grammar (Delayed_Floating_Index)
                  declare
                     Byte_Region : Buffer_Region renames Floating_Non_Grammar (Delayed_Floating_Index).Byte_Region;
                  begin
                     if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                       Next_KMN_Stable_First < Byte_Region.Last
                     then
                        --  Next change also edits the token; more delay.
                        null;
                     else
                        Do_Scan        := True;
                        Lex_Start_Byte := Delayed_Lex_Start_Byte;
                        Lex_Start_Char := Delayed_Lex_Start_Char;
                        Lex_Start_Line := Delayed_Lex_Start_Line;

                        Check_Comment_End (Floating_Non_Grammar (Delayed_Floating_Index));
                        Delayed_Scan   := False;
                     end if;
                  end;

               elsif Terminal_Byte_Region.Last + Shift_Bytes > Scanned_Byte_Pos
                 --  else Terminal was scanned by a previous KMN.
                 --  ada_mode-recover_align_1.adb, ada_mode-interactive_02.adb
                 and Next_KMN_Stable_First + Shift_Bytes > Scanned_Byte_Pos
                 --  else all of current edit has been scanned. test_incremental.adb Edit_Comment_2
               then
                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     --  Edit start is in Terminal_Non_Grammar_Next.
                     --  test_incremental.adb Edit_Comment*
                     declare
                        Non_Grammar  : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        Last_Floated : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
                        Token        : Lexer.Token renames Non_Grammar (Terminal_Non_Grammar_Next);
                     begin
                        if Token.Byte_Region.First + Shift_Bytes >
                          Inserted_Region.First
                        then
                           --  Edit start is in whitespace preceding Token
                           Lex_Start_Byte := Inserted_Region.First;
                           Lex_Start_Char := Inserted_Region_Chars.First;
                           Lex_Start_Line :=
                             (if Terminal_Non_Grammar_Next > Non_Grammar.First_Index
                              then Non_Grammar (Terminal_Non_Grammar_Next - 1).Line_Region.Last
                              else Tree.Line_Region
                                (Tree.Prev_Source_Terminal (Terminal, Trailing_Non_Grammar => True)).Last);
                           Do_Scan := True;
                        else
                           --  Edit start is in Token
                           Lex_Start_Byte := Token.Byte_Region.First + Shift_Bytes;
                           Lex_Start_Char := Token.Char_Region.First + Shift_Chars;
                           Lex_Start_Line := Token.Line_Region.First + Shift_Lines;
                           Do_Scan := True;

                           if KMN.Deleted_Bytes > 0 and
                             Tree.Lexer.Is_Comment (Token.ID) and
                             Token.Byte_Region.Last > Deleted_Region.Last
                           then
                              --  test_incremental.adb Delete_Comment_Start
                              Comment_Start_Deleted := True;
                              New_Code_End          := Token.Byte_Region.Last + Shift_Bytes;
                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("comment_start_deleted:" &
                                      Token.Byte_Region.First'Image & " .." & New_Code_End'Image);
                              end if;
                           end if;
                           Check_Comment_End (Token);
                        end if;

                        --  Remaining Non_Grammar will either be scanned, or moved to
                        --  a new grammar token, so delete or move to floating now.
                        for I in Terminal_Non_Grammar_Next .. Non_Grammar.Last_Index loop
                           declare
                              Byte_Region : Buffer_Region renames Non_Grammar (I).Byte_Region;
                           begin
                              if (KMN.Deleted_Bytes > 0 and then Byte_Region.First <= Deleted_Region.Last)
                                or
                                (KMN.Inserted_Bytes > 0 and then
                                   Byte_Region.First + Shift_Bytes <= Inserted_Region.Last)
                              then
                                 if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                                   Next_KMN_Stable_Last < Byte_Region.Last
                                 then
                                    --  Next change is an actual change (not just last placeholder KMN),
                                    --  and it also overlaps this token. It may insert or delete a comment
                                    --  end, so we don't know when to end a scan; handle it then.
                                    --  test_incremental.adb Edit_Comment_7, ada_mode-partial_parse.adb.
                                    Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    Floating_Non_Grammar.Append (Non_Grammar (I));
                                    Last_Floated := I;
                                    Do_Scan := False;
                                    Delayed_Scan := True;
                                    Delayed_Floating_Index := Floating_Non_Grammar.Last_Index;
                                    Delayed_Lex_Start_Byte := Lex_Start_Byte;
                                    Delayed_Lex_Start_Char := Lex_Start_Char;
                                    Delayed_Lex_Start_Line := Lex_Start_Line;
                                    if Trace_Incremental_Parse > Detail then
                                       Parser.Trace.Put_Line ("scan delayed");
                                    end if;
                                 else
                                    --  Token overlaps the change region; it will be rescanned. Delete it
                                    --  here (ie don't copy to floating). It may contain New_Lines.
                                    --  test_incremental.adb Delete_Comment_End.
                                    declare
                                       New_Line_Count : constant Base_Line_Number_Type := WisiToken.New_Line_Count
                                         (Non_Grammar (I).Line_Region);
                                       --  FIXME: why "after the edit region"? ada_mode-interactive_01.adb
                                       --  auto-case in comment.
                                       --
                                       --  Tree.Lexer.New_Line_Count
                                       --  (Byte_Region =>
                                       --     (First => Buffer_Pos'Max
                                       --        ((if KMN.Deleted_Bytes > 0 then Deleted_Region.Last else 0),
                                       --         (if KMN.Inserted_Bytes > 0 then Inserted_Region.Last else 0)),
                                       --      Last => Byte_Region.Last));
                                    begin
                                       Shift_Lines := @ - New_Line_Count;
                                    end;
                                 end if;
                              else
                                 --  Token does not overlap the edit region; handle it later.
                                 Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                 Floating_Non_Grammar.Append (Non_Grammar (I));
                                 Last_Floated := I;
                              end if;
                           end;
                        end loop;

                        if Terminal_Non_Grammar_Next = Non_Grammar.First_Index then
                           Non_Grammar.Clear;
                        else
                           Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Terminal_Non_Grammar_Next - 1);
                        end if;

                        if Trace_Incremental_Parse > Detail then
                           if Last_Floated /= Lexer.Token_Arrays.No_Index then
                              Parser.Trace.Put_Line
                                ("float or delete non_grammar" & Terminal_Non_Grammar_Next'Image & " .." &
                                   Last_Floated'Image);
                           end if;
                        end if;

                        Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;

                        Tree.Next_Terminal (Terminal);
                     end;


                  elsif Terminal_Byte_Region.First + Shift_Bytes <= Inserted_Region.First then
                     --  Edit start is in Terminal.

                     if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
                        if Length (Inserted_Region) > 0 then
                           Do_Scan := True;

                           Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                           Lex_Start_Char := Tree.Char_Region (Terminal.Node).First + Shift_Chars;

                           --  Line_Region.First is from prev_terminal.non_grammar, which is shifted
                           Lex_Start_Line := Tree.Line_Region (Terminal).First;
                        else
                           --  We never re-scan eoi; we just shift it.
                           null;
                        end if;
                     else
                        Do_Scan := True;

                        Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                        Lex_Start_Char := Tree.Char_Region (Terminal.Node).First + Shift_Chars;

                        --  Line_Region.First is from prev_terminal.non_grammar, which is shifted
                        Lex_Start_Line := Tree.Line_Region (Terminal).First;
                     end if;

                  else
                     --  Edit start is in or adjacent to some non_grammar token or
                     --  whitespace preceding Terminal; delete non_grammar tokens adjacent
                     --  to, containing or after the edit start; they will be rescanned
                     --  (the scan loop exits on terminals, not non_grammars). Deleted
                     --  New_Lines decrement Shift_Lines.
                     declare
                        procedure Handle_Non_Grammar
                          (Non_Grammar : in out WisiToken.Lexer.Token_Arrays.Vector;
                           Floating    : in     Boolean)
                        is
                           Last_Byte : constant Buffer_Pos :=
                             (if Non_Grammar.Length = 0
                              then Buffer_Pos'Last
                              else Non_Grammar (Non_Grammar.Last_Index).Byte_Region.Last +
                                (if Floating then Shift_Bytes else 0));

                           Delete : SAL.Base_Peek_Type := 0;
                        begin
                           if Non_Grammar.Length = 0 then
                              --  Edit start is in whitespace before Terminal.
                              --  test_incremental.adb Edit_Whitespace_1, _2
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Tree.Line_Region (Last_Grammar).Last;
                              --  start_line test case ada_mode-incremental_02.adb
                              Do_Scan        := True;

                           elsif Last_Byte <= Scanned_Byte_Pos then
                              --  Edit start is in whitespace before Terminal.
                              --  ada_mode-incremental_04.adb
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
                              Do_Scan        := True;

                           else
                              for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                 declare
                                    Byte_Last : constant Buffer_Pos := Non_Grammar (I).Byte_Region.Last +
                                      (if Floating then Shift_Bytes else 0);
                                 begin
                                    if Byte_Last + 1 >= Inserted_Region.First and
                                      Byte_Last > Scanned_Byte_Pos
                                      --  test case: ada_mode-recover_align_1.adb, test_incremental.adb Edit_Comment_2
                                    then
                                       Delete  := I;
                                       Do_Scan := True;
                                       exit;
                                    end if;
                                 end;
                              end loop;

                              if Delete > 0 and then Non_Grammar (Delete).ID = Tree.Lexer.Descriptor.SOI_ID then
                                 if Delete = Non_Grammar.Last_Index then
                                    Delete := 0;
                                 else
                                    Delete := Delete + 1;
                                 end if;
                              end if;

                              if Delete > 0 then
                                 --  Edit is in or before Non_Grammar (Delete) (ie a comment); set
                                 --  Lex_Start_* to scan from edit start or start of Token, whichever
                                 --  is earlier.

                                 declare
                                    Token : WisiToken.Lexer.Token renames Non_Grammar (Delete);
                                 begin
                                    if (Tree.Lexer.Is_Comment (Token.ID) and
                                          Inserted_Region.First < Token.Byte_Region.Last)
                                      --  Inserting in middle of Token, not adding to end.
                                      and then Tree.Lexer.Contains_New_Line (Inserted_Region)
                                    then
                                       Comment_End_Inserted := True;
                                       --  The exposed code is in the inserted text after the new comment
                                       --  end, plus in the old comment, terminated by the old comment
                                       --  end. test_incremental.adb Edit_Comment_4, Edit_Comment_7
                                       --  New_Code_End is shifted.
                                       New_Code_End := Token.Byte_Region.Last + KMN.Inserted_Bytes + Shift_Bytes;
                                       if Trace_Incremental_Parse > Detail then
                                          Parser.Trace.Put_Line
                                            ("comment_end_inserted:" &
                                               Token.Byte_Region.First'Image & " .." &
                                               New_Code_End'Image);
                                       end if;
                                    end if;

                                    Lex_Start_Byte := Buffer_Pos'Min
                                      (Token.Byte_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region.First);

                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region_Chars.First);

                                    Lex_Start_Line := Token.Line_Region.First +
                                      (if Floating
                                       then
                                          --  If this token contributed to Shift_Lines, ignore that part.
                                          --  ada_mode-recover_14 comment after extra 'begin'.
                                          Shift_Lines + New_Line_Count (Token.Line_Region)
                                       else 0);
                                 end;

                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ((if Floating
                                        then "delete floating_non_grammar"
                                        else "delete non_grammar" & Tree.Get_Node_Index (Last_Grammar.Node)'Image) &
                                         Delete'Image & " .." & Non_Grammar.Last_Index'Image);
                                 end if;

                                 if not Floating then
                                    for I in Delete .. Non_Grammar.Last_Index loop
                                       Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    end loop;
                                 end if;

                                 Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Delete - 1);
                              else
                                 --  Edit is in whitespace between last non_grammar and Terminal
                                 Lex_Start_Byte := Inserted_Region.First;
                                 Lex_Start_Char := Inserted_Region_Chars.First;
                                 Lex_Start_Line := Tree.Line_Region (Terminal).First;
                                 Do_Scan        := True;
                              end if;
                           end if;
                        end Handle_Non_Grammar;
                     begin
                        loop
                           --  Set Last_Grammar to the token containing the non_grammar that may
                           --  contain the edit start.
                           exit when Tree.Label (Last_Grammar.Node) = Source_Terminal;

                           if Tree.Non_Grammar_Const (Last_Grammar.Node).Length > 0 then
                              --  This token will be deleted below. Ensure we scan all of this
                              --  non_grammar.
                              declare
                                 Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const
                                   (Last_Grammar.Node);
                                 Token : Lexer.Token renames Non_Grammar (Non_Grammar.First_Index);
                              begin
                                 Do_Scan        := True;
                                 Lex_Start_Byte := Buffer_Pos'Min
                                   (Token.Byte_Region.First, Inserted_Region.First);
                                 Lex_Start_Char := Buffer_Pos'Min
                                   (Token.Char_Region.First, Inserted_Region_Chars.First);
                                 Lex_Start_Line := Token.Line_Region.First;
                              end;
                           end if;

                           Tree.Prev_Terminal (Last_Grammar);
                        end loop;

                        if not Do_Scan then
                           if Floating_Non_Grammar.Length > 0 and then
                             Floating_Non_Grammar (Floating_Non_Grammar.First_Index).Byte_Region.First <
                             Inserted_Region.First
                             --  FIXME: floating_non_grammar is not shifted, inserted_region is shifted.
                           then
                              --  The edit start is in a floated non_grammar.
                              --  test_incremental.adb Edit_Comment_7
                              --  ada_mode-recover_14.adb comment after deleted "begin".
                              Handle_Non_Grammar (Floating_Non_Grammar, Floating => True);
                           else
                              Handle_Non_Grammar
                                (Tree.Non_Grammar_Var (Last_Grammar.Node), Floating => False);
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               if Do_Scan then
                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line
                       ("lexer.set_position" & Lex_Start_Byte'Image & Lex_Start_Char'Image & Lex_Start_Line'Image);
                  end if;

                  Parser.Tree.Lexer.Set_Position
                    (Byte_Position => Lex_Start_Byte,
                     Char_Position => Lex_Start_Char,
                     Line          => Lex_Start_Line);

                  --  Ensure Terminal.Node is first in Terminal.Element, so we can
                  --  insert before it.
                  Breakdown (Terminal);

                  Last_Grammar := Tree.Prev_Terminal (Terminal);

                  Scan_Changed_Loop :
                  loop
                     declare
                        Token : Lexer.Token; -- Newly scanned, and thus shifted
                        Error : constant Boolean := Tree.Lexer.Find_Next (Token);
                        Ref   : Terminal_Ref;
                     begin
                        if Trace_Lexer > Outline then
                           Parser.Trace.Put_Line ("lex: " & Lexer.Image (Token, Parser.Tree.Lexer.Descriptor.all));
                           if Error then
                              declare
                                 Error : Lexer.Error renames Tree.Lexer.Errors (Tree.Lexer.Errors.Last);
                              begin
                                 Parser.Trace.Put_Line
                                   (" ... error: " & Error.Char_Pos'Image &
                                      (if Error.Recover_Char (1) /= ASCII.NUL
                                       then "'" & Error.Recover_Char (1) & "'"
                                       else ""));
                              end;
                           end if;
                        end if;

                        exit Scan_Changed_Loop when Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID;

                        if Comment_End_Inserted or Comment_Start_Deleted then
                           if Token.Byte_Region.Last > New_Code_End then
                              --  test_incremental.adb Edit_Comment_4, _5, _7, Delete_Comment_Start
                              Comment_End_Inserted  := False;
                              Comment_Start_Deleted := False;
                              New_Code_End          := Invalid_Buffer_Pos;
                              exit Scan_Changed_Loop;
                           end if;
                        else
                           exit Scan_Changed_Loop when
                             Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal and then
                             not (Token.Byte_Region.First - Shift_Bytes <= Stable_Region.Last or
                                    --  Token started in stable region

                                    (KMN.Inserted_Bytes > 0 and then
                                       Token.Byte_Region.First <= Inserted_Region.Last + 1
                                       --  Token starts in or immediately after inserted region
                                       --  test_incremental.adb Edit_Code_4 '1 +', Edit_Code_8 ';'

                                    ) or
                                    (KMN.Deleted_Bytes > 0 and then
                                       Token.Byte_Region.First - (Shift_Bytes + KMN.Inserted_Bytes) =
                                       Deleted_Region.First
                                       --  Previously existing Token starts immediately after deleted region;
                                       --  it may have been truncated (test_incremental.adb Edit_Code_4 'Cc')
                                    ));
                        end if;

                        Scanned_Byte_Pos := Token.Byte_Region.Last;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal then
                           --  grammar token
                           Ref := Tree.Insert_Source_Terminal
                             (Stream, Token,
                              Before => Terminal.Element,
                              Error =>
                                (if Error
                                 then Lexer_Error'(Error => Tree.Lexer.Errors (Tree.Lexer.Errors.Last))
                                 else No_Error));

                           Process_Grammar_Token (Parser, Token, Ref.Node);
                           Last_Grammar := Ref;

                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line ("scan new " & Tree.Image (Ref));
                           end if;

                        else
                           --  non_grammar token
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("scan new " & Lexer.Full_Image (Token, Parser.Tree.Lexer.Descriptor.all));
                           end if;

                           Process_Non_Grammar_Token (Parser, Last_Grammar.Node, Token);
                           Shift_Lines := @ + New_Line_Count (Token.Line_Region);
                        end if;
                     end;
                  end loop Scan_Changed_Loop;
               end if;

               --  Do this here so Shift_Bytes is correct in Delete_Loop
               Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
               Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

               Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
               Old_Char_Pos := Stable_Region_Chars.Last + KMN.Deleted_Chars;
               New_Byte_Pos := Inserted_Region.Last;
               New_Char_Pos := Inserted_Region_Chars.Last;
               pragma Assert (New_Byte_Pos - Old_Byte_Pos = Shift_Bytes);

               if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                  loop
                     declare
                        Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                     begin
                        exit when
                          Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.First + Shift_Bytes >
                          Inserted_Region.Last;

                        Lexer.Shift (Non_Grammar (Terminal_Non_Grammar_Next), Shift_Bytes, Shift_Chars, Shift_Lines);

                        Terminal_Non_Grammar_Next := @ + 1;

                        if Terminal_Non_Grammar_Next = Non_Grammar.Last_Index then
                           Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                           Tree.Next_Terminal (Terminal);
                           exit;
                        end if;
                     end;
                  end loop;
               end if;

               --  Delete tokens that were deleted or modified. Deleted non_grammar
               --  New_Lines decrement Shift_Lines. Check for deleted comment end
               --  (deleted comment start handled in Scan_Changed_Loop).
               --
               --  Need delete even if not Do_Scan, to handle KMN.Deleted_Bytes > 0;
               --  test_incremental.adb Edit_Code_4, ada_skel.adb ada-skel-return

               Delete_Loop :
               loop
                  --  This loop is done even when KMN.Deleted_Bytes = KMN.Inserted_Bytes
                  --  = 0, because one scan may span several KMN.

                  exit Delete_Loop when Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index;

                  exit Delete_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

                  exit Delete_Loop when not Comment_End_Deleted and then
                    (Tree.Label (Terminal.Node) = Syntax_Trees.Source_Terminal and then
                       not
                       ((KMN.Deleted_Bytes > 0 and
                           Tree.Byte_Region (Terminal.Node).First <= Deleted_Region.Last + 1)  -- deleted or modified
                          or
                          (KMN.Inserted_Bytes > 0 and
                             Tree.Byte_Region (Terminal.Node).First <= Stable_Region.Last + 1) --  modified
                          or
                          (Tree.Byte_Region (Terminal.Node).First <= Next_KMN_Stable_Last and then
                             --  Shift_Bytes is valid
                             Tree.Byte_Region (Terminal.Node).First + Shift_Bytes <= Scanned_Byte_Pos)
                             --  Token region was scanned, possibly due to inserted comment start
                             --  or similar small edit that affects a large region. ada_mode-interactive_04.adb
                       ));

                  --  Ensure Terminal is Single, so we can delete it.
                  Breakdown (Terminal, To_Single => True);

                  declare
                     To_Delete : Stream_Node_Ref := Terminal;
                  begin
                     if Comment_End_Deleted and then
                       Tree.Byte_Region (To_Delete, Trailing_Non_Grammar => True).First > New_Comment_End
                       --  test_incremental.adb Edit_Comment_9
                     then
                        Comment_End_Deleted := False;
                        New_Comment_End     := Invalid_Buffer_Pos;
                        exit Delete_Loop;
                     end if;

                     Tree.Next_Terminal (Terminal);
                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("delete deleted or modified " &
                             Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => True));
                     end if;

                     for Token of Tree.Non_Grammar_Const (To_Delete.Node) loop
                        Shift_Lines := @ - New_Line_Count (Token.Line_Region);

                        if (Token.Byte_Region.Last + Shift_Bytes <= Scanned_Byte_Pos and
                              Token.Byte_Region.First + Shift_Bytes <= Next_KMN_Stable_Last + Shift_Bytes)
                          --  Token was scanned, and is in current KMN
                          or
                          Token.Byte_Region.Last <= Deleted_Region.Last
                          --  token was deleted. test/ada_mode-interactive_03.adb delete text end of buffer.
                        then
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("delete non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                           end if;
                        else
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("float non_grammar " & Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                           end if;
                           Floating_Non_Grammar.Append (Token);
                        end if;
                     end loop;

                     pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                     Tree.Stream_Delete (Stream, To_Delete.Element);
                  end;
               end loop Delete_Loop;

               --  If any Floating_Non_Grammar are this KMN's change region or next
               --  KMN stable, they can be handled here.
               declare
                  Last_Handled_Non_Grammar : SAL.Base_Peek_Type := Lexer.Token_Arrays.No_Index;

                  function Find_Element
                    (Target_Bytes : in Buffer_Pos;
                     After        : in Boolean)
                    return Terminal_Ref
                  --  If Target_Bytes < Terminal.Byte_Region.First: If After
                  --  is True, return terminal node that is after or contains
                  --  Target_Bytes, where prev terminal is before Target_Bytes. Else
                  --  return terminal that is before Target_Bytes, where next is after.
                  --
                  --  Otherwise similar, but searching forward.
                  --
                  --  Target_Bytes is unshifted.
                  is
                     Terminal_First : constant Buffer_Pos := Tree.Byte_Region (Terminal.Node).First;
                     Searching_Back : constant Boolean := Terminal_First > Target_Bytes;

                     Before_1 : Terminal_Ref := -- before Before
                       (if Searching_Back
                        then Tree.Prev_Terminal (Terminal)
                        else Terminal);
                     Before      : Terminal_Ref :=
                       (if Searching_Back
                        then Terminal
                        else Tree.Next_Terminal (Terminal));
                  begin
                     loop
                        if Searching_Back then
                           declare
                              --  Target_Bytes is unshifted. If searching forward, all nodes are
                              --  also unshifted. If searching back, all nodes except Terminal are
                              --  shifted. Compare Target_Bytes to unshifted region.
                              --
                              --  region bounds test case: ada_mode-recover_partial_14.adb
                              --  contains; ada_mode-recover_42.adb lexer_error string_literal
                              Shift_First : constant Base_Buffer_Pos := -Shift_Bytes;
                              Shift_Last : constant Base_Buffer_Pos := (if Before = Terminal then 0 else -Shift_Bytes);

                              First : constant Buffer_Pos := Tree.Byte_Region (Before_1.Node).Last + 1 + Shift_First;
                              Last  : constant Buffer_Pos :=
                                (if After
                                 then Tree.Byte_Region (Before).Last
                                 else Tree.Byte_Region (Before).First - 1) + Shift_Last;
                           begin
                              exit when Target_Bytes in First .. Last;
                           end;
                        else
                           declare
                              First : constant Buffer_Pos := Tree.Byte_Region (Before_1.Node).Last + 1;
                              Last  : constant Buffer_Pos :=
                                (if After
                                 then Tree.Byte_Region (Before).Last
                                 else Tree.Byte_Region (Before).First - 1);
                           begin
                              exit when Target_Bytes in First .. Last;
                           end;
                        end if;
                        if Terminal_First > Target_Bytes then
                           Before := Before_1;
                           Tree.Prev_Terminal (Before_1);
                        else
                           Before_1 := Before;
                           Tree.Next_Terminal (Before);
                        end if;
                     end loop;
                     return (if After then Before else Before_1);
                  end Find_Element;

                  procedure Restore (I : in Positive_Index_Type)
                  --  Restore Floating_Non_Grammar (I)
                  is
                     Token : Lexer.Token renames Floating_Non_Grammar (I);

                     Containing_Terminal : constant Terminal_Ref := Find_Element
                       (Token.Byte_Region.First, After => False);

                     Old_Token : constant Lexer.Token := Token; -- for trace message

                     Temp_Shift_Lines : Base_Line_Number_Type := Shift_Lines;
                  begin
                     if Token.Byte_Region.First < Tree.Byte_Region (Terminal.Node).First then
                        --  Only shift if inserted before Terminal. ada_mode-recover_14
                        --
                        --  Ignore this and remaining Floating_Non_Grammar's contribution to
                        --  Shift_Lines; we are inserting it before those. (new_lines in
                        --  Floating_Non_Grammar were previously subtracted from Shift_Lines).
                        --  ada_mode-interactive_01.adb, ada_mode-recover_33.adb,
                        --  ada_mode-recover_extra_end_loop.adb
                        for J in I .. Floating_Non_Grammar.Last_Index loop
                           Temp_Shift_Lines := @ + New_Line_Count (Floating_Non_Grammar (J).Line_Region);
                        end loop;
                        Lexer.Shift (Token, Shift_Bytes, Shift_Chars, Temp_Shift_Lines);
                     end if;

                     Shift_Lines := @ + New_Line_Count (Token.Line_Region);

                     Tree.Non_Grammar_Var (Containing_Terminal.Node).Append (Token);

                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("restore floating_non_grammar " & Lexer.Image (Old_Token, Tree.Lexer.Descriptor.all));
                        Parser.Trace.Put_Line
                          (" ... to " & Tree.Image (Containing_Terminal, Non_Grammar => True));
                     end if;
                  end Restore;

               begin
                  if Delayed_Scan and then Floating_Non_Grammar.First_Index = Delayed_Floating_Index then
                     null;
                  else
                     for I in Floating_Non_Grammar.First_Index ..
                       (if Delayed_Scan then Delayed_Floating_Index - 1 else Floating_Non_Grammar.Last_Index)
                     loop
                        exit when Floating_Non_Grammar (I).Byte_Region.First > Next_KMN_Stable_Last;
                        --  If token is in next KMN edit region, shift_bytes is wrong here.

                        if Floating_Non_Grammar (I).Byte_Region.First + Shift_Bytes <= Scanned_Byte_Pos then
                           --  Non_grammar token was rescanned; delete the old one.
                           --  test case: ada_mode-recover_align_1.adb, test_incremental.adb Edit_Whitespace

                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("delete floating_non_grammar " & Lexer.Image
                                   (Floating_Non_Grammar (I), Tree.Lexer.Descriptor.all));
                           end if;
                           Last_Handled_Non_Grammar := I;

                        elsif Floating_Non_Grammar (I).Byte_Region.Last <= Next_KMN_Stable_Last then
                           --  Non_Grammar is in next KMN stable region; find terminal to append
                           --  non_grammar to. ada_mode-recover_18.adb
                           Restore (I);
                           Last_Handled_Non_Grammar := I;
                        else
                           exit;
                        end if;
                     end loop;
                  end if;
                  if Last_Handled_Non_Grammar /= Lexer.Token_Arrays.No_Index then
                     if Last_Handled_Non_Grammar = Floating_Non_Grammar.Last_Index then
                        Floating_Non_Grammar.Clear;
                     else
                        Floating_Non_Grammar.Set_First_Last
                          (Last_Handled_Non_Grammar + 1, Floating_Non_Grammar.Last_Index);
                     end if;
                  end if;
               end;
            end;

            KMN_Node := KMN_Lists.Next (KMN_Node);

            if not KMN_Lists.Has_Element (KMN_Node) then
               --  Finally shift EOI.
               pragma Assert
                 (Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID and
                    Tree.Non_Grammar_Const (Terminal.Node).Length = 1); -- EOI_ID

               Tree.Shift
                 (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines, Buffer_Pos'Last, Terminal_Non_Grammar_Next);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("final shift " & Tree.Image (Terminal, Non_Grammar => True));
               end if;

               exit KMN_Loop;
            end if;
         end;
      end loop KMN_Loop;

      if Tree.ID (Terminal.Node) /= Parser.Tree.Lexer.Descriptor.EOI_ID then
         raise User_Error with "edit list does not cover entire tree";
      end if;

      if not Floating_Non_Grammar.Is_Empty then
         raise SAL.Programmer_Error with "floating_non_grammar not emptied: " & Lexer.Image
           (Floating_Non_Grammar, Tree.Lexer.Descriptor.all);
      end if;

      if Debug_Mode then
         declare
            Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
         begin
            Parser.Tree.Validate_Tree (Parser.User_Data.all, Error_Reported, Node_Index_Order => False);
            if Error_Reported.Count /= 0 then
               raise WisiToken.Parse_Error with "edit_tree: validate_tree failed";
            end if;
         end;
      end if;
   end Edit_Tree;

   procedure Put_Errors (Parser : in Base_Parser'Class)
   is
      use WisiToken.Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      for Err in Tree.Error_Iterate loop
         declare
            Error_Node : constant Valid_Node_Access := Tree.Error_Node (Err);
         begin
            Ada.Text_IO.Put_Line
              (Tree.Error_Message
               (Error_Node, Error (Err).Image (Tree, Error_Node)));
         end;
      end loop;
   end Put_Errors;

   procedure Put_Errors (Parser : in Base_Parser'Class; Stream : in Syntax_Trees.Stream_ID)
   is
      use WisiToken.Syntax_Trees;
      Tree : Syntax_Trees.Tree renames Parser.Tree;
   begin
      for Cur in Tree.Stream_Error_Iterate (Stream) loop
         declare
            Error_Ref : constant Stream_Error_Ref := Error (Cur);
            Error_Node : constant Valid_Node_Access := Tree.Error_Node (Error_Ref);
         begin
            for Err of Tree.Error_List (Error_Node) loop
               Ada.Text_IO.Put_Line
                 (Tree.Error_Message
                    (Ref     => Error_Ref.Ref.Ref, -- For line, column
                     Message => Err.Image (Tree, Error_Node)));
            end loop;
         end;
      end loop;
   end Put_Errors;

end WisiToken.Parse;
