--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with WisiToken.In_Parse_Actions;
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
      use Syntax_Trees;
      use all type Ada.Containers.Count_Type;

      First_Term : constant Node_Access := Tree.First_Terminal (Error_Node);

      Item_Byte_Region : constant Buffer_Region :=
        (if First_Term = Invalid_Node_Access
         then Null_Buffer_Region
         else Tree.Byte_Region (First_Term, Trailing_Non_Grammar => False));

      Msg : constant String :=
        "syntax_error: expecting " & Image (Data.Expecting, Tree.Lexer.Descriptor.all) &
        ", found " &
        (if First_Term = Invalid_Node_Access
         then "empty nonterm " & Image (Tree.ID (Error_Node), Tree.Lexer.Descriptor.all)
         else "'" & Tree.Lexer.Buffer_Text (Item_Byte_Region) & "'");
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
            use all type WisiToken.Syntax_Trees.In_Parse_Actions.Status;
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
      Result := +"in_parse_action_error: " & WisiToken.In_Parse_Actions.Image (Data.Status, Tree, Error_Node);

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

   function Error_Pred_Lexer (Cur : in Syntax_Trees.Error_Data_Lists.Cursor) return Boolean
   is
      use Syntax_Trees.Error_Data_Lists;
   begin
      return
        (if Element (Cur) in Lexer_Error then True
         else False);
   end Error_Pred_Lexer;

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

         else False);
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

   function Get_In_Parse_Action
     (Parser : in Base_Parser;
      ID     : in Production_ID)
     return Syntax_Trees.In_Parse_Actions.In_Parse_Action
   is begin
      if Parser.Productions.Is_Empty then
         return null;
      elsif Parser.Productions (ID.LHS).RHSs.Is_Empty then
         return null;
      else
         return Parser.Productions (ID.LHS).RHSs (ID.RHS).In_Parse_Action;
      end if;
   end Get_In_Parse_Action;

   function Get_Post_Parse_Action
     (Productions : in Syntax_Trees.Production_Info_Trees.Vector;
      ID          : in Production_ID)
     return Syntax_Trees.Post_Parse_Action
   is begin
      if Productions.Is_Empty then
         return null;
      elsif Productions (ID.LHS).RHSs.Is_Empty then
         return null;
      else
         return Productions (ID.LHS).RHSs (ID.RHS).Post_Parse_Action;
      end if;
   end Get_Post_Parse_Action;

   function Get_Post_Parse_Action
     (Parser : in Base_Parser;
      ID     : in Production_ID)
     return Syntax_Trees.Post_Parse_Action
   is begin
      return Get_Post_Parse_Action (Parser.Productions, ID);
   end Get_Post_Parse_Action;

   function Next_Grammar_Token
     (Parser            : in out Base_Parser'Class;
      Last_Grammar_Node : in out WisiToken.Syntax_Trees.Node_Access)
     return Token_ID
   is
      use Syntax_Trees;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;
   begin
      loop
         declare
            Token        : WisiToken.Lexer.Token;
            Error_Count  : constant Natural := Lexer.Find_Next (Token);
            Lexer_Errors : Error_Data_Lists.List;
         begin

            if Trace_Lexer > Outline then
               Tree.Lexer.Trace.Put_Line (WisiToken.Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
            end if;

            if Error_Count > 0 then
               declare
                  Cur : WisiToken.Lexer.Error_Lists.Cursor := Lexer.Errors.Last;
               begin
                  for I in 1 .. Error_Count - 1 loop
                     WisiToken.Lexer.Error_Lists.Previous (Cur);
                  end loop;
                  for I in 1 .. Error_Count loop
                     Lexer_Errors.Append (Lexer_Error'(Error => Lexer.Errors (Cur)));
                     WisiToken.Lexer.Error_Lists.Next (Cur);
                  end loop;
               end;
            end if;

            if Token.ID >= Lexer.Descriptor.First_Terminal then
               declare
                  Ref : constant Terminal_Ref := Tree.Add_Terminal (Parser.Tree.Shared_Stream, Token, Lexer_Errors);
               begin
                  Process_Grammar_Token (Parser, Token, Ref.Node);
                  Last_Grammar_Node := Ref.Node;
               end;
            else
               if Trace_Lexer > Detail then
                  Tree.Lexer.Trace.Put_Line ("non-grammar in " & Parser.Tree.Image (Last_Grammar_Node));
               end if;
               if Error_Count > 0 then
                  --  test_incremental.adb Lexer_Errors_04, _05
                  Tree.Add_Errors (Tree.Shared_Stream, Last_Grammar_Node, Lexer_Errors);
               end if;

               Process_Non_Grammar_Token (Parser, Last_Grammar_Node, Token);
            end if;

            if Error_Count > 0 and Trace_Lexer > Detail then
               Tree.Lexer.Trace.Put_Line
                 ("lexer error" & (if Error_Count > 1 then "s" else "") &
                    " in " & Parser.Tree.Image (Last_Grammar_Node));
            end if;

            if Token.ID >= Lexer.Descriptor.First_Terminal then
               return Token.ID;
            end if;
         end;
      end loop;
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
         Parser.Tree.Lexer.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Last_Grammar_Node)'Image & " tokens lexed");
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

      Scanned_Byte_Pos : Base_Buffer_Pos  := 0;
      Scanned_Char_Pos : Base_Buffer_Pos  := 0;
      --  End of last token saved after being scanned by the lexer; this
      --  does not include trailing whitespace, so it is not actually the
      --  last position scanned by the lexer. Note that the lexer has
      --  effectively scanned the deleted bytes in the current KMN, so when
      --  comparing unshifted token positions to Scanned_Byte_Pos, we may
      --  need to add KMN.Deleted_Bytes.

      Shift_Bytes      : Base_Buffer_Pos  := 0;
      Shift_Chars      : Base_Buffer_Pos  := 0;

      Shift_Lines : Base_Line_Number_Type := 0;
      --  Whenever a non_grammar is deleted from Tree (either permanently,
      --  or moved to Floating_Non_Grammar), Shift_Lines is decremented by
      --  New_Line_Count (non_grammar_token). Then if a token is restored
      --  from Floating_Non_Grammar to the Tree, Shift_Lines is incremented;
      --  if a token is deleted from Floating_Non_Grammar, Shift_Lines is
      --  not changed. See Deleted_Shift_Lines in KMN_Loop for additional
      --  rules.

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
      --  When multiple edits occur in a token, the last one may insert or
      --  delete an end delimiter, so it is not possible to compute the
      --  correct scan end when handling the first edit. So the scan is
      --  delayed. If Delayed_Floating_Index /= 'Last, the token is a
      --  non_grammar.

      Scan_End : Base_Buffer_Pos := Invalid_Buffer_Pos;
      --  If Scan_End /= Invalid_Buffer_Pos, an edit exposed text as
      --  code; a comment or string end was inserted, or a comment or string
      --  start was deleted. Scan all exposed code thru Scan_End (which
      --  is shifted).
      --
      --  If the start and end delimiters of the block token are different,
      --  we don't need to check for start delimiter inserted or end
      --  delimiter deleted; the edited token is scanned, and all the tokens
      --  that the new token covers will be deleted because they are in the
      --  region scanned.
      --
      --  However, if the start and end delimiters are the same (as for
      --  strings), then deleting either delimiter requires all text thru
      --  new-line or EOI be scanned.

      type Lexer_Error_Data is record
         Node            : Node_Access;
         Scan_End        : Base_Buffer_Pos := Invalid_Buffer_Pos;
         Scan_Start_Node : Node_Access     := Invalid_Node_Access;
      end record;

      package Lexer_Error_Data_Lists is new Ada.Containers.Doubly_Linked_Lists (Lexer_Error_Data);

      Lexer_Errors : Lexer_Error_Data_Lists.List;
      --  This list records the scan region for lexer errors, depending on
      --  where they occur. Then if an edit might affect the lexer error,
      --  the scan for the edit covers that region. That lets an edit fix
      --  the lexer error. test_incremental.adb Edit_String_06,
      --  Lexer_Errors_04.

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;
      --  Node being considered for shift or delete. Terminals before
      --  Terminal are shifted; Terminal and terminals after it are
      --  unshifted.

      Terminal_Non_Grammar_Next : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
      --  Next non_grammar in Terminal to be shifted or deleted.

      procedure Maybe_Delete_Lexer_Errors (Node : in Valid_Node_Access)
      is begin
         if Invalid_Error_Ref /= Tree.Has_Error_Class (Node, Lexer_Error'(others => <>)) then
            --  Delete from Lexer_Errors. test_incremental.adb Edit_String_09
            declare
               use Lexer_Error_Data_Lists;
               Cur : Cursor := Lexer_Errors.First;
            begin
               loop
                  exit when Cur = No_Element;

                  if Lexer_Errors (Cur).Node = Node then
                     declare
                        To_Delete_1 : Cursor := Cur;
                     begin
                        Next (Cur);
                        Lexer_Errors.Delete (To_Delete_1);
                     end;
                  end if;
                  Next (Cur);
               end loop;
            end;
         end if;
      end Maybe_Delete_Lexer_Errors;

      procedure Breakdown (Terminal : in out Terminal_Ref; To_Single : in Boolean := False)
      with Pre => Terminal /= Invalid_Stream_Node_Ref
      is begin
         if Tree.Label (Terminal.Element) = Nonterm then
            if Trace_Incremental_Parse > Detail then
               Tree.Lexer.Trace.Put_Line
                 ("breakdown " & (if To_Single then "single " else "") & Tree.Image
                    (Tree.Get_Node (Terminal.Stream, Terminal.Element), Node_Numbers => True) &
                    " target " & Tree.Image (Terminal.Node, Node_Numbers => True));
               if Trace_Incremental_Parse > Extra + 1 then
                  Tree.Lexer.Trace.Put_Line ("... before:");
                  Tree.Lexer.Trace.Put_Line (Tree.Image (Stream, Children => True));
                  Tree.Lexer.Trace.New_Line;
               end if;
            end if;
            Tree.Breakdown (Terminal, Parser.Productions, Parser.User_Data, First_Terminal => True);

            if To_Single and then Tree.Label (Terminal.Element) = Nonterm then
               Tree.Left_Breakdown (Terminal, Parser.User_Data);
            end if;
            if Trace_Incremental_Parse > Extra then
               Tree.Lexer.Trace.Put_Line
                 ("... result " & Tree.Image (Stream, Children => Trace_Incremental_Parse > Extra + 1));
            end if;
         end if;
      end Breakdown;

   begin
      Tree.Start_Edit;

      if Edits.Length = 0 then
         return;
      end if;

      Stream := Tree.Shared_Stream;

      --  Breakdown Recover_Conflict nonterms. Nonterms are marked
      --  Recover_Conflict when the initial parse resolves a conflict during
      --  error recovery; a subseuent edit may require a different conflict
      --  resolution. We breakdown all the way to terminals, because some of
      --  the children may be nonterms that were created before the error
      --  was detected, but need to change. test_incremental.adb
      --  Undo_Conflict_01.
      --
      --  We must do this before Undo_Recover, because that might breakdown
      --  a Recover_Conflict node.
      declare
         Ref          : Stream_Node_Ref := Tree.First_Recover_Conflict;
         To_Breakdown : Stream_Node_Ref;
      begin
         Breakdown_Recover_Conflict :
         loop
            exit Breakdown_Recover_Conflict when Ref = Invalid_Stream_Node_Ref;

            if Tree.Label (Ref.Node) = Nonterm and then Tree.Is_Empty_Nonterm (Ref.Node) then
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("delete empty recover_conflict node " & Tree.Image (Ref.Node, Node_Numbers => True));
               end if;

               if Get_Node (Ref.Element) /= Ref.Node then
                  Tree.Breakdown (Ref, Parser.Productions, Parser.User_Data, First_Terminal => False);
               end if;

               declare
                  To_Delete : Stream_Index := Ref.Element;
               begin
                  Tree.Stream_Next (Ref, Rooted => True);
                  Tree.Stream_Delete (Stream, To_Delete);
               end;

            else
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("breakdown recover_conflict node " & Tree.Image (Ref, Node_Numbers => True));
               end if;

               Tree.Breakdown (Ref, Parser.Productions, Parser.User_Data, First_Terminal => False);
               To_Breakdown := Ref;
               Tree.First_Terminal (To_Breakdown);
               Tree.Stream_Next (Ref, Rooted => True);

               To_Terminals :
               loop
                  if Trace_Incremental_Parse > Extra then
                     Tree.Lexer.Trace.Put_Line
                       ("... to_breakdown " & Tree.Image (To_Breakdown, Node_Numbers => True));
                  end if;

                  if Tree.Label (To_Breakdown.Element) = Nonterm then
                     if Tree.Is_Empty_Nonterm (Tree.Get_Node (Stream, To_Breakdown.Element)) then
                        declare
                           To_Delete : Stream_Index := To_Breakdown.Element;
                        begin
                           Tree.Stream_Next (To_Breakdown, Rooted => False);
                           Tree.Stream_Delete (Stream, To_Delete);
                        end;
                     else
                        Tree.Left_Breakdown (To_Breakdown, Parser.User_Data);
                        Tree.Stream_Next (To_Breakdown, Rooted => False);
                     end if;
                     if Trace_Incremental_Parse > Extra then
                        Tree.Lexer.Trace.Put_Line
                          ("... stream " & Tree.Image (Stream, Node_Numbers => True));
                     end if;

                  else
                     Tree.Stream_Next (To_Breakdown, Rooted => False);
                  end if;
                  exit To_Terminals when To_Breakdown.Element = Ref.Element;
               end loop To_Terminals;

               To_Breakdown := Invalid_Stream_Node_Ref;
            end if;

            Tree.First_Recover_Conflict (Ref);
         end loop Breakdown_Recover_Conflict;
      end;

      --  Undo all error recover insert/delete, in case this is needed as
      --  part of an edit in another place; test_incremental.adb
      --  Preserve_Parse_Errors_2.
      --
      --  IMPROVEME incremental: This algorithm visits every terminal; not
      --  incremental. Cache Has_Following_Deleted, has_virtual in nonterms.

      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));
      Undo_Recover :
      loop
         Next_Recover :
         loop
            exit Undo_Recover when Terminal.Node = Invalid_Node_Access;
            exit Next_Recover when Tree.Label (Terminal.Node) in Virtual_Terminal;
            exit Next_Recover when Tree.Label (Terminal.Node) = Source_Terminal and then
              Tree.Has_Following_Deleted (Terminal.Node);
            Tree.Next_Terminal (Terminal);
         end loop Next_Recover;

         case Terminal_Label'(Tree.Label (Terminal.Node)) is
         when Source_Terminal =>
            declare
               Has_Deleted   : constant Valid_Node_Access := Terminal.Node;
               Insert_Before : Terminal_Ref;
            begin
               Tree.Next_Terminal (Terminal);

               Breakdown (Terminal);
               Insert_Before := Terminal;

               for Deleted_Node of reverse Tree.Following_Deleted (Has_Deleted) loop
                  if Tree.Label (Deleted_Node) in Virtual_Terminal_Label then
                     --  This would be deleted in the next step, so don't bother restoring
                     --  it.
                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("drop virtual deleted node " & Tree.Image (Deleted_Node, Node_Numbers => True));
                     end if;

                  else
                     declare
                        Deleted_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                          (Deleted_Node, Trailing_Non_Grammar => False);

                        Has_Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var
                          (Has_Deleted);
                        First_To_Move : Positive_Index_Type := Positive_Index_Type'Last;
                        Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Deleted_Node);
                        pragma Assert (Deleted_Non_Grammar.Length = 0);
                     begin
                        if Has_Deleted_Non_Grammar.Length > 0 and then
                          Deleted_Byte_Region.First < Has_Deleted_Non_Grammar
                            (Has_Deleted_Non_Grammar.Last_Index).Byte_Region.Last
                        then
                           --  Move some Non_Grammar to Deleted_Non_Grammar
                           --  test_incremental.adb Modify_Deleted_Element, Lexer_Errors_01,
                           --  Restore_Deleted_01
                           for I in Has_Deleted_Non_Grammar.First_Index .. Has_Deleted_Non_Grammar.Last_Index loop
                              if Deleted_Byte_Region.First < Has_Deleted_Non_Grammar (I).Byte_Region.Last then
                                 First_To_Move := I;
                                 exit;
                              end if;
                           end loop;
                           for I in First_To_Move .. Has_Deleted_Non_Grammar.Last_Index loop
                              Deleted_Non_Grammar.Append (Has_Deleted_Non_Grammar (I));
                           end loop;
                           if First_To_Move = Has_Deleted_Non_Grammar.First_Index then
                              Has_Deleted_Non_Grammar.Clear;
                           else
                              Has_Deleted_Non_Grammar.Set_First_Last
                                (Has_Deleted_Non_Grammar.First_Index, First_To_Move - 1);
                           end if;
                        end if;
                     end;

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("restore deleted node " & Tree.Image
                             (Deleted_Node, Node_Numbers => True, Non_Grammar => True) &
                             " before " & Tree.Image
                               (Insert_Before, Node_Numbers => True, Non_Grammar => True));
                     end if;

                     Tree.Set_Sequential_Index (Deleted_Node, Invalid_Sequential_Index);
                     Insert_Before := Tree.Stream_Insert (Stream, Deleted_Node, Insert_Before.Element);
                  end if;
               end loop;
               Tree.Following_Deleted (Has_Deleted).Clear;

               if Trace_Incremental_Parse > Extra then
                  Tree.Lexer.Trace.Put_Line
                    ("stream:" & Tree.Image
                       (Stream,
                        Children    => Trace_Incremental_Parse > Detail,
                        Non_Grammar => True,
                        Augmented   => True));
               end if;
            end;

         when Virtual_Terminal_Label =>
            --  Delete Terminal.
            Breakdown (Terminal, To_Single => True);

            declare
               Terminal_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
               To_Delete            : Stream_Index := Terminal.Element;
               Next_Element         : Stream_Index := Tree.Stream_Next (Terminal.Stream, Terminal.Element);
            begin
               if Terminal_Non_Grammar.Length > 0 then
                  declare
                     Term_Non_Gramm_Region : constant Buffer_Region :=
                       (First => Terminal_Non_Grammar (Terminal_Non_Grammar.First_Index).Byte_Region.First,
                        Last  => Terminal_Non_Grammar (Terminal_Non_Grammar.Last_Index).Byte_Region.Last);

                     Next_Terminal : Stream_Node_Ref := Tree.Next_Terminal (Terminal);
                  begin
                     --  Terminal_Non_Grammar is non-empty only if User_Data.Insert_Token
                     --  moved some non_grammar to it. If the terminal they were moved from
                     --  was subsequently deleted and restored, it may now be
                     --  Next_Terminal: ada_mode-interactive_09.adb new_line after 'for'.
                     --  Or it may be before a previous terminal; ada_mode-recover_09.adb.
                     --
                     --  Find a terminal to move Terminal_Non_Grammar to.
                     if Next_Terminal = Invalid_Stream_Node_Ref then
                        --  ada_mode-recover_partial_26.adb
                        Next_Terminal := Terminal;
                     end if;
                     loop
                        exit when Tree.Byte_Region (Next_Terminal.Node, Trailing_Non_Grammar => False).First <
                          Term_Non_Gramm_Region.First;
                        exit when Tree.ID (Next_Terminal.Node) = Tree.Lexer.Descriptor.SOI_ID;
                        Tree.Prev_Terminal (Next_Terminal);
                     end loop;
                     Tree.Non_Grammar_Var (Next_Terminal.Node).Append (Terminal_Non_Grammar);
                     Terminal_Non_Grammar.Clear;
                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("move non_grammar to " & Tree.Image
                             (Next_Terminal.Node, Node_Numbers => True, Non_Grammar => True));
                     end if;
                  end;
               end if;

               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("delete virtual " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
               end if;

               Tree.Next_Terminal (Terminal);
               Tree.Stream_Delete (Terminal.Stream, To_Delete);


               --  Delete immediately following empty nonterms. For example, in Ada,
               --  error recover often inserts 'end <name_opt> ;', where name_opt is
               --  empty; delete all three tokens. On the other hand, an empty
               --  nonterm could be a block name; it will be recreated by the parser,
               --  not treated as an error.
               loop
                  exit when Next_Element = Invalid_Stream_Index;
                  declare
                     Node : constant Valid_Node_Access := Tree.Get_Node (Terminal.Stream, Next_Element);
                     To_Delete : Stream_Index;
                  begin
                     if Tree.Label (Node) = Nonterm and then Tree.Child_Count (Node) = 0 then
                        To_Delete := Next_Element;

                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
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
      end loop Undo_Recover;

      --  Delete parse error nodes; any parse errors remaining after the
      --  edit is applied will be recreated during parse. We don't delete
      --  lexer errors, because they will not be recreated by parsing; they
      --  will be deleted if they are fixed by an edit, because the tokens
      --  containing them will be rescanned.
      --
      --  It is tempting to try to retain parse errors so the previously
      --  found solution can be reapplied, but then it is difficult to
      --  decide which errors to delete during parse; test_incremental.adb
      --  Edit_String_05.
      declare
         Err_Ref : Stream_Error_Ref := Tree.First_Error (Stream);
      begin
         loop
            exit when not Has_Error (Err_Ref);
            declare
               Err       : constant Error_Data'Class := Error (Err_Ref);
               Error_Ref : constant Stream_Node_Ref  := Tree.Error_Node (Err_Ref);
            begin
               if Err in Lexer_Error then
                  --  We don't know Shift_Bytes yet, so we can't find Scan_End.
                  Lexer_Errors.Append ((Error_Ref.Node, others => <>));
                  Tree.Next_Error (Err_Ref);
               else
                  if Trace_Incremental_Parse > Detail then
                     Tree.Lexer.Trace.Put_Line ("delete " & Err.Image (Tree, Error_Ref.Node));
                  end if;
                  Tree.Delete_Error (Err_Ref);
               end if;
            end;
         end loop;
      end;

      --  Now process source edits. We have to start with SOI to handle
      --  edits in leading non-grammar. test_incremental.adb Edit_Comment_12
      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream, Skip_SOI => False));

      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Edits (KMN_Node);

            Stable_Region : constant Buffer_Region := -- Not shifted
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Stable_Region_Chars : constant Buffer_Region :=
              (Old_Char_Pos + 1, Old_Char_Pos + KMN.Stable_Chars);

            Deleted_Region : constant Buffer_Region := -- Not shifted.
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);
            --  Inserted_Region.First is the first char after the stable region in
            --  the edited text (which means it is shifted).
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

            Deleted_Shift_Lines : Base_Line_Number_Type := 0;
            --  When a non_grammar is deleted by Delete_Deleted_Loop below, if
            --  Terminal remains before the deleted non_grammar
            --  Deleted_Shift_Lines is incremented instead of Shift_Lines. Then
            --  Shift_Lines is correct when computing a scan start point. After
            --  the scan, Deleted_Shift_Lines is added to Shift_Lines.

         begin
            --  Parser.Lexer contains the edited text, so we can't check that
            --  stable, deleted are inside the initial text. Caller should use
            --  Validate_KMN.

            if Trace_Incremental_Parse > Detail then
               Tree.Lexer.Trace.New_Line;
               Tree.Lexer.Trace.Put_Line
                 ("KMN: " & Image (Stable_Region) & Image (Inserted_Region) & Image (Deleted_Region));
               Tree.Lexer.Trace.Put_Line ("old  :" & Old_Byte_Pos'Image & Old_Char_Pos'Image);
               Tree.Lexer.Trace.Put_Line
                 ("shift:" & Shift_Bytes'Image & " " & Shift_Chars'Image & " " & Shift_Lines'Image);
               Tree.Lexer.Trace.Put_Line ("scanned_byte_pos:" & Scanned_Byte_Pos'Image);
               Tree.Lexer.Trace.Put_Line
                 ("stream:" & Tree.Image
                    (Stream,
                     Children    => True,
                     Non_Grammar => True,
                     Augmented   => True));

               Tree.Lexer.Trace.Put_Line
                 ("terminal: " & Tree.Image (Terminal, Non_Grammar => True, Node_Numbers => True));
               if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                  Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
               end if;

               if Floating_Non_Grammar.Length > 0 then
                  Tree.Lexer.Trace.Put_Line
                    ("floating_non_grammar: " & Lexer.Full_Image (Floating_Non_Grammar, Tree.Lexer.Descriptor.all));
                  if Delayed_Floating_Index /= Positive_Index_Type'Last then
                     Tree.Lexer.Trace.Put_Line ("delayed_floating_index:" & Delayed_Floating_Index'Image);
                  end if;
               end if;

               Tree.Lexer.Trace.New_Line;
            end if;

            if not Contains (Outer => Parser.Tree.Lexer.Buffer_Region_Byte, Inner => Inserted_Region) then
               raise User_Error with "KMN insert region " & Image (Inserted_Region) & " outside edited source text " &
                 Image (Parser.Tree.Lexer.Buffer_Region_Byte);
            end if;

            declare
               use Lexer_Error_Data_Lists;
               Cur : Cursor := Lexer_Errors.First;
            begin
               loop
                  exit when Cur = No_Element;

                  if Tree.Byte_Region (Lexer_Errors (Cur).Node, Trailing_Non_Grammar => False).First in
                    Stable_Region.First .. Next_KMN_Stable_First
                  then
                     --  Now we know Shift for this lexer error.
                     declare
                        Node    : constant Valid_Node_Access := Lexer_Errors (Cur).Node;
                        Node_ID : constant Token_ID          := Tree.ID (Node);
                     begin
                        --  Node has not yet been shifted.
                        if Tree.Lexer.Is_Block_Delimited (Node_ID) then
                           --  test_incremental.adb Edit_String_09, Lexer_Errors_03.
                           declare
                              Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                                (Node, Trailing_Non_Grammar => False);

                              Prev_Terminal : constant Stream_Node_Ref :=
                                Tree.Prev_Terminal (Tree.To_Stream_Node_Ref (Stream, Node));
                              Data : Lexer_Error_Data renames Lexer_Errors (Cur);
                           begin
                              Data.Scan_End := Tree.Lexer.Find_Scan_End
                                (Tree.ID (Node), Node_Byte_Region + Shift_Bytes +
                                   (if Node_Byte_Region.First > Stable_Region.Last
                                    then 0
                                    else KMN.Inserted_Bytes),
                                 Inserted  => True,
                                 Start     => True);

                              if Tree.ID (Prev_Terminal) = Tree.ID (Node) and then
                                Tree.Byte_Region (Prev_Terminal, Trailing_Non_Grammar => False).Last + 1 =
                                Node_Byte_Region.First and then
                                Tree.Lexer.Same_Block_Delimiters (Node_ID) and then
                                Tree.Lexer.Escape_Delimiter_Doubled (Node_ID)
                              then
                                 --  Prev, Node look like:
                                 --
                                 --   "foo""bar"
                                 --
                                 --  Need to scan both. test_incremental.adb Edit_String_12
                                 Data.Scan_Start_Node := Prev_Terminal.Node;
                              end if;
                           end;
                        else
                           --  The lexer error occurred while scanning the token or one of the
                           --  following non_grammars. test_incremental.adb Lexer_Errors_04.
                           declare
                              Node_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                                (Node, Trailing_Non_Grammar => True);
                           begin
                              Lexer_Errors (Cur).Scan_End := Node_Byte_Region.Last;
                           end;
                        end if;
                     end;

                     if Lexer_Errors (Cur).Scan_End <= Stable_Region.Last + Shift_Bytes then
                        --  This lexer error is not fixed by these edits.
                        declare
                           To_Delete : Cursor := Cur;
                        begin
                           Next (Cur);
                           Lexer_Errors.Delete (To_Delete);
                        end;
                     else
                        --  We must scan from this lexer error to find out if it is fixed.
                        if Trace_Lexer > Outline then
                           declare
                              Data : Lexer_Error_Data renames Lexer_Errors (Cur).Element.all;
                           begin
                              Tree.Lexer.Trace.Put_Line
                                ("lexer error on " & Tree.Image (Data.Node, Node_Numbers => True) &
                                   " possibly fixed by this KMN; scan end" & Data.Scan_End'Image);
                           end;
                        end if;
                        Next (Cur);
                     end if;
                  else
                     exit;
                  end if;
               end loop;
            end;

            if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
               --  We only shift EOI after all KMN are processed; it may need to be
               --  shifted for more than one edit point. test_incremental.adb
               --  Edit_Comment_3.
               if Trace_Incremental_Parse > Detail then
                  Tree.Lexer.Trace.Put_Line
                    ("nothing left to shift; terminal:" & Tree.Image
                       (Terminal, Non_Grammar => True, Augmented => True));
               end if;

            elsif not Delayed_Scan then
               --  If there was a Delayed_Scan, some changed tokens may be before
               --  Stable_Region, so we don't do Unchanged_Loop.
               --
               --  It is tempting to skip Unchanged_Loop if Shift_Bytes = 0 and
               --  Shift_Chars = 0 and Shift_Lines = 0. But we need to scan all
               --  Non_Grammar for Floating_Non_Grammar, which changes Shift_Lines.
               --  IMPROVEME: only need to scan trailing stable terminal?

               Unchanged_Loop :
               loop
                  exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
                  exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  --  All virtuals were deleted above by removing error corrections.
                  pragma Assert (Tree.Label (Terminal.Node) = Syntax_Trees.Source_Terminal);

                  if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                     --  Exit when Terminal may be changed by the current KMN edit; it is
                     --  partly past or adjacent to Stable_Region.Last. Also exit when last
                     --  KMN is done.
                     exit Unchanged_Loop when
                       Tree.ID (Terminal) /= Tree.Lexer.Descriptor.SOI_ID and then
                       (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                        then Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last >
                          Stable_Region.Last -- Last KMN
                        else Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last >=
                          Stable_Region.Last);

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("stable shift " & Tree.Image
                             (Terminal.Node,
                              Non_Grammar => True, Terminal_Node_Numbers => True, Augmented => True));
                     end if;

                     --  Tree.Shift sets Terminal_Non_Grammar_Next to the first non-grammar
                     --  that may be modified.
                     Tree.Shift
                       (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines,
                        Last_Stable_Byte =>
                          (if KMN.Inserted_Bytes = 0 and KMN.Deleted_Bytes = 0
                           then Buffer_Pos'Last --  ada_mode-interactive_02.adb
                           else Stable_Region.Last),
                        Non_Grammar_Next => Terminal_Non_Grammar_Next);

                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line
                          ("  => " & Tree.Image
                             (Terminal.Node, Non_Grammar => True, Terminal_Node_Numbers => True,
                              Augmented => True));
                     end if;

                     if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                        exit Unchanged_Loop;
                     else
                        Tree.Next_Terminal (Terminal);
                     end if;

                  else
                     --  The previous KMN left Terminal_Non_Grammar_Next /= No_Index
                     if Trace_Incremental_Parse > Detail then
                        Tree.Lexer.Trace.Put_Line ("Terminal_Non_Grammar_Next:" & Terminal_Non_Grammar_Next'Image);
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
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next cleared");
                        else
                           Tree.Lexer.Trace.Put_Line ("terminal_non_grammar_next:" & Terminal_Non_Grammar_Next'Image);
                        end if;
                     end if;
                  end if;
               end loop Unchanged_Loop;
            end if;

            --  Unchanged_Loop exited because Terminal or Terminal.Non_Grammar is
            --  at least partly out of Stable_Region or adjacent to the edit
            --  start, or because it reached Tree.EOI. Therefore the edit start is
            --  in Terminal, or in a non-grammar token or whitespace before
            --  Terminal (or after Tree.EOI), or after Terminal if Terminal is
            --  deleted.

            if KMN.Deleted_Bytes > 0 then
               --  Delete tokens deleted by this KMN, preserving Terminal if
               --  necessary. test_incremental.adb Edit_Code_03, _04.
               --
               --  If deleting a grammar token, delete start is before the token,
               --  delete end may be in its non_grammar; check for deleted comment
               --  start and float non-deleted non_grammar. If not deleting a grammar
               --  token, delete start is in its non_grammar, delete end may be in or
               --  after its non_grammar; check for deleted comment start and end
               --  (there can be one of each) and float non-deleted non_grammar. To
               --  simplify the code, we always check for comment start and comment
               --  end deleted.

               declare
                  --  If we do Breakdown on a copy of Terminal, Terminal may become
                  --  invalid because Terminal.Element is deleted. So before the first
                  --  delete, we save the previous terminal.
                  Saved_Prev_Terminal : Terminal_Ref := Invalid_Stream_Node_Ref;

                  Check_Deleted : Terminal_Ref :=
                    (if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index or
                       Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).Last < Deleted_Region.First
                     then Tree.Next_Terminal (Terminal)
                     else Terminal);

                  Terminal_Is_Check_Deleted : Boolean;

                  Keep_Terminal : constant Boolean :=
                    Tree.ID (Terminal.Node) in Tree.Lexer.Descriptor.SOI_ID | Tree.Lexer.Descriptor.EOI_ID or
                    Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index or -- comment is modified
                    Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First <
                    Deleted_Region.First; -- terminal is modified

                  procedure Check_Scan_End
                    (ID           : in Token_ID;
                     Start_Region : in Buffer_Region;
                     End_Region   : in Buffer_Region)
                  --  Check if the start or end delimiter is deleted or modified.
                  is
                     Start_Changed : constant Boolean := Deleted_Region.First <= Start_Region.Last and
                       Deleted_Region.Last >= Start_Region.First; -- start delimiter deleted or modified

                     End_Changed : constant Boolean := Deleted_Region.Last >= End_Region.Last and
                          Deleted_Region.First <= End_Region.First; -- end delimiter deleted or modified
                  begin
                     if Start_Changed and End_Changed then
                        return;

                     elsif Start_Changed or End_Changed then
                        --  test_incremental.adb Delete_Comment_Start_*, Edit_String_*, Edit_Comment_16,
                        --  not ada_mode-interactive_01.adb "-- ada_identifier"
                        Scan_End := Tree.Lexer.Find_Scan_End
                          (ID,
                           (Start_Region.First + Shift_Bytes +
                              --  If not Start_Changed, start delimiter is before the current KMN
                              (if Start_Changed then KMN.Inserted_Bytes - KMN.Deleted_Bytes else 0),
                            End_Region.Last    + Shift_Bytes + KMN.Inserted_Bytes - KMN.Deleted_Bytes),
                           Inserted => False,
                           Start    => Start_Changed);

                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
                             ("start or end delimiter deleted or modified:" &
                                Start_Region.First'Image & " .." & Scan_End'Image);
                        end if;
                     end if;
                  end Check_Scan_End;

                  procedure Check_Scan_End (Token : in Lexer.Token)
                  with Pre => Tree.Lexer.Is_Block_Delimited (Token.ID)
                  --  Token is modified; check if the start or end delimiter is deleted or modified.
                  is
                  begin
                     Check_Scan_End
                       (ID           => Token.ID,
                        Start_Region =>
                          (Token.Byte_Region.First,
                           Token.Byte_Region.First + Buffer_Pos (Tree.Lexer.Start_Delimiter_Length (Token.ID)) - 1),
                        End_Region   =>
                          (Token.Byte_Region.Last - Buffer_Pos (Tree.Lexer.End_Delimiter_Length (Token.ID)) + 1,
                           Token.Byte_Region.Last));
                  end Check_Scan_End;

                  procedure Check_Scan_End (Node : in Valid_Node_Access)
                  with Pre => Tree.Lexer.Is_Block_Delimited (Tree.ID (Node))
                  --  Check if the start delimiter is deleted.
                  is
                     ID          : constant Token_ID      := Tree.ID (Node);
                     Byte_Region : constant Buffer_Region := Tree.Byte_Region (Node, Trailing_Non_Grammar => False);
                  begin
                     Check_Scan_End
                       (ID,
                        Start_Region =>
                          (Byte_Region.First,
                           Byte_Region.First + Buffer_Pos (Tree.Lexer.Start_Delimiter_Length (ID)) - 1),
                        End_Region =>
                          (Byte_Region.Last - Buffer_Pos (Tree.Lexer.End_Delimiter_Length (ID)) + 1,
                           Byte_Region.Last));
                  end Check_Scan_End;

                  procedure Handle_Non_Grammar
                    (Non_Grammar    : in out Lexer.Token_Arrays.Vector;
                     Delete_Grammar : in     Boolean;
                     Floating       : in     Boolean)
                  --  Delete start and/or end is in Non_Grammar. Check if it has any
                  --  partly or not deleted tokens, and if it has a deleted comment end
                  --  with a remaining comment start and vice versa. If Delete_Grammar,
                  --  the grammar token that owns Non_Grammar is being deleted.
                  is
                     type Action_Type is (Keep, Delete, Float);

                     --  If the KMN deletes from the middle of Non_Grammar, and not
                     --  Delete_Grammar, we can have actions like:
                     --  Keep, Keep, Delete, Keep, Keep.
                     --
                     --  If Delete_Grammar, that could be:
                     --  Keep, Keep, Delete, Keep, Float
                     --
                     --  Where token 4 is modified. There are two ranges to delete.
                     --
                     --  For the cases we do handle, any deletes will always be contiguous,
                     --  and floats will immediately follow the deletes.

                     Delete_First : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Delete_Last  : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Float_First  : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                     Float_Last   : SAL.Base_Peek_Type := Non_Grammar.First_Index - 1;
                  begin
                     --  First decide what to keep and delete, and float the ones that need to be floated
                     for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                        declare
                           Token  : Lexer.Token renames Non_Grammar (I);
                           Action : Action_Type := Keep;
                        begin
                           if Token.ID in Tree.Lexer.Descriptor.SOI_ID | Tree.Lexer.Descriptor.EOI_ID then
                              null;

                           elsif Token.Byte_Region.First >= Deleted_Region.First and
                             Token.Byte_Region.Last <= Deleted_Region.Last
                           then
                              --  Token is deleted.
                              Action := Delete;

                           elsif (Deleted_Region.First <= Token.Byte_Region.First and
                                    Deleted_Region.Last >= Token.Byte_Region.First and
                                    Deleted_Region.Last < Token.Byte_Region.Last) or
                             (Token.Byte_Region.First < Deleted_Region.First and
                                Token.Byte_Region.Last >= Deleted_Region.First)
                           then
                              --  Token is modified; it needs to be scanned.
                              if Delete_Grammar then
                                 --  If this edit also modified a precending grammar token, the scan
                                 --  will start there, and include this modified non_grammar.
                                 --  test_incremental.adb Delete_Comment_Start_05.
                                 --
                                 --  Otherwise, the scan will start at the end of the deleted text, and
                                 --  include this non_grammar; test_incremental.adb
                                 --  Delete_Comment_Start_06.
                                 Action := Float;

                              else
                                 pragma Assert (if Floating then Delayed_Scan); -- test_incremental.adb Edit_Comment_02

                                 --  If Floating, Non_Grammar was edited by a previous change,
                                 --  Delayed_Scan is true, so it will be scanned. We leave it in
                                 --  Non_Grammar to be consistent. If not Floating, we leave it in
                                 --  Check_Deleted.Non_Grammar so it is included in the scan start
                                 --  compute below.
                                 Action := Keep;
                              end if;

                              if Tree.Lexer.Is_Block_Delimited (Token.ID) then
                                 Check_Scan_End (Token);
                              end if;

                           else
                              --  Token is neither deleted nor modified.
                              if Floating then
                                 Action := Keep;
                              elsif Delete_Grammar then
                                 Action := Float;
                              else
                                 Action := Keep;
                              end if;
                           end if;

                           case Action is
                           when Keep =>
                              null;

                           when Delete =>
                              if Delete_First = Non_Grammar.First_Index - 1 then
                                 Delete_First := I;
                              end if;
                              Delete_Last := I;

                              if not Floating then
                                 --  Floating_Non_Grammar lines are included in Shift_Lines when
                                 --  floated.

                                 if Keep_Terminal then
                                    --  test_incremental.adb Edit_Code_09
                                    Deleted_Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                                 else
                                    Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                                 end if;
                              end if;

                              if Trace_Incremental_Parse > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("delete deleted " & (if Floating then "floating " else "") &
                                      "non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                              end if;

                           when Float =>
                              pragma Assert (not Floating);
                              if Keep_Terminal then
                                 --  test_incremental.adb Edit_Code_09
                                 Deleted_Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                              else
                                 Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                              end if;

                              Floating_Non_Grammar.Append (Token);
                              if Float_First = Non_Grammar.First_Index - 1 then
                                 Float_First := I;
                              end if;
                              Float_Last := I;

                              if Trace_Incremental_Parse > Detail then
                                 Tree.Lexer.Trace.Put_Line
                                   ("float non_grammar " & Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                              end if;
                           end case;
                        end;
                     end loop;

                     --  Delete the deleted and floated.
                     declare
                        procedure Delete_Range (First, Last : in SAL.Base_Peek_Type)
                        is begin
                           if First < Non_Grammar.First_Index then
                              null;

                           elsif First = Non_Grammar.First_Index then
                              if Last = Non_Grammar.Last_Index then
                                 Non_Grammar.Clear;
                              else
                                 Non_Grammar.Set_First_Last (Last + 1, Non_Grammar.Last_Index);
                              end if;

                           elsif Last = Non_Grammar.Last_Index then
                              Non_Grammar.Set_First_Last (Non_Grammar.First_Index, First - 1);

                           else
                              --  Delete slice from middle
                              declare
                                 New_Non_Grammar : Lexer.Token_Arrays.Vector;
                              begin
                                 for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                    if I < First then
                                       New_Non_Grammar.Append (Non_Grammar (I));
                                    elsif I > Last then
                                       New_Non_Grammar.Append (Non_Grammar (I));
                                    end if;
                                 end loop;
                                 Non_Grammar := New_Non_Grammar;
                              end;
                           end if;
                        end Delete_Range;
                     begin
                        Delete_Range (Delete_First, Delete_Last);
                        Delete_Range (Float_First, Float_Last);
                     end;
                  end Handle_Non_Grammar;

                  procedure Do_Delete
                  is
                  begin
                     if Terminal_Is_Check_Deleted then
                        Terminal := Invalid_Stream_Node_Ref; -- allow deleting Terminal.Element via Check_Deleted
                     end if;

                     if Saved_Prev_Terminal = Invalid_Stream_Node_Ref and not Terminal_Is_Check_Deleted then
                        if Terminal.Element = Check_Deleted.Element then
                           Check_Deleted.Element := Invalid_Stream_Index;

                           Breakdown (Terminal, To_Single => False);

                           --  Find the stream element that contains Check_Deleted_Node.
                           Check_Deleted.Element := Terminal.Element;
                           loop
                              pragma Assert
                                (Tree.ID (Tree.Get_Node (Check_Deleted.Stream, Check_Deleted.Element)) /=
                                   Tree.Lexer.Descriptor.EOI_ID);

                              if Tree.Is_Descendant_Of
                                (Root => Tree.Get_Node (Check_Deleted.Stream, Check_Deleted.Element),
                                 Descendant => Check_Deleted.Node)
                              then
                                 exit;
                              end if;
                              Check_Deleted.Element := Tree.Stream_Next (Check_Deleted.Stream, Check_Deleted.Element);
                           end loop;
                           if Terminal.Element = Check_Deleted.Element then
                              --  Check_Deleted.Element was not deleted.
                              Saved_Prev_Terminal := Tree.Prev_Terminal (Terminal);
                              Terminal := Invalid_Stream_Node_Ref;
                           end if;
                        end if;
                     end if;

                     pragma Assert
                       (Terminal_Is_Check_Deleted or else
                        (if Saved_Prev_Terminal = Invalid_Stream_Node_Ref
                         then Terminal.Element /= Check_Deleted.Element
                         else Saved_Prev_Terminal.Element /= Check_Deleted.Element));

                     Breakdown (Check_Deleted, To_Single => True);

                     declare
                        To_Delete : Stream_Node_Ref := Check_Deleted;
                     begin
                        Tree.Next_Terminal (Check_Deleted);
                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line
                             ("delete deleted " &
                                Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => False));
                        end if;

                        Maybe_Delete_Lexer_Errors (To_Delete.Node);

                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (To_Delete.Node)) then
                           Check_Scan_End (To_Delete.Node);
                        end if;

                        --  FIXME: if terminal_is_check_deleted, we already did this
                        Handle_Non_Grammar
                          (Tree.Non_Grammar_Var (To_Delete.Node), Delete_Grammar => True, Floating => False);

                        pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                        Tree.Stream_Delete (Stream, To_Delete.Element);
                     end;

                     if Terminal_Is_Check_Deleted then
                        Terminal := Check_Deleted;
                     end if;
                  end Do_Delete;

               begin
                  if Floating_Non_Grammar.Length > 0 then
                     --  test_incremental.adb Edit_Comment_3
                     Handle_Non_Grammar (Floating_Non_Grammar, Delete_Grammar => False, Floating => True);
                  end if;

                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     Handle_Non_Grammar
                       (Tree.Non_Grammar_Var (Terminal.Node), Delete_Grammar => False, Floating => False);
                  end if;

                  Delete_Deleted_Loop :
                  loop
                     Terminal_Is_Check_Deleted := Terminal = Check_Deleted;

                     exit Delete_Deleted_Loop when Check_Deleted = Invalid_Stream_Node_Ref;
                     --  Happens when Terminal is EOI. test_incremental.adb Edit_Comment_3

                     exit Delete_Deleted_Loop when Tree.ID (Check_Deleted.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;
                     --  FIXME: exit when check_deleted outside KMN?

                     if Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).First >
                       Deleted_Region.Last + 1
                     then
                        --  Check_Deleted is not deleted or modified
                        exit Delete_Deleted_Loop;

                     elsif Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).First <
                       Deleted_Region.First or
                       Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).Last > Deleted_Region.Last
                     then
                        --  Check_Deleted is not deleted, but potentially modified.
                        --  test_incremental.adb Edit_Code_04, Edit_Code_05.
                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Check_Deleted.Node)) then
                           --  test_incremental.adb Edit_String_01
                           Check_Scan_End (Check_Deleted.Node);
                        end if;

                        Handle_Non_Grammar
                          (Tree.Non_Grammar_Var (Check_Deleted.Node), Delete_Grammar => False, Floating => False);

                        if Tree.Byte_Region (Check_Deleted.Node, Trailing_Non_Grammar => False).Last >
                          Deleted_Region.Last
                        then
                           --  test_incremental.adb Edit_Comment_10
                           exit Delete_Deleted_Loop;
                        else
                           --  test_incremental.adb Delete_Comment_Start_05
                           Tree.Next_Terminal (Check_Deleted);
                        end if;

                     else
                        Do_Delete;
                     end if;
                  end loop Delete_Deleted_Loop;

                  if Keep_Terminal then
                     if Saved_Prev_Terminal /= Invalid_Stream_Node_Ref then
                        Terminal := Tree.Next_Terminal (Saved_Prev_Terminal);
                        --  Terminal_Non_Grammar_Next is unchanged.
                     end if;
                  else
                     Terminal := Check_Deleted;
                  end if;
                  if Trace_Incremental_Parse > Extra then
                     Tree.Lexer.Trace.Put_Line
                       ("terminal: " & Tree.Image (Terminal, Non_Grammar => True, Node_Numbers => True));
                     Tree.Lexer.Trace.Put_Line ("deleted_shift_lines:" & Deleted_Shift_Lines'Image);
                  end if;
               end;
            end if;

            --  Now decide what to scan.
            --
            --  If two edit regions affect the same token, scanning the first will
            --  also scan the second.
            declare
               Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                 (Terminal.Node, Trailing_Non_Grammar => Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index);

               Do_Scan : Boolean := False;

               Lex_Start_Byte : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Char : Buffer_Pos       := Buffer_Pos'Last;
               Lex_Start_Line : Line_Number_Type := Line_Number_Type'Last;

               Last_Grammar       : Stream_Node_Ref := Invalid_Stream_Node_Ref;
               Last_Scanned_Token : Lexer.Token;

               procedure Check_Scan_End
                 (ID     : in Token_ID;
                  Region : in Buffer_Region)
               --  Check if Inserted_Region inserts an end delimiter for ID in
               --  Region.
               is
                  Shift : constant Base_Buffer_Pos := KMN.Inserted_Bytes - KMN.Deleted_Bytes +
                    (if Delayed_Scan then Shift_Bytes else 0);
               begin
                  if KMN.Inserted_Bytes > 0 and then
                    Inserted_Region.First <= Region.Last
                  then
                     declare
                        Delimiter_Pos : constant Base_Buffer_Pos := Tree.Lexer.Contains_End_Delimiter
                          (ID, Inserted_Region);
                     begin
                        if Delimiter_Pos /= Invalid_Buffer_Pos then
                           --  test_incremental.adb Edit_Comment_5, _12, Edit_String_*, ada_mode-interactive_02.adb
                           Scan_End := Tree.Lexer.Find_Scan_End
                             (ID, (Delimiter_Pos, Region.Last + Shift), Inserted => True, Start => False);

                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("end delimiter inserted:" &
                                   Region.First'Image & " .." &
                                   Scan_End'Image);
                           end if;
                        end if;
                     end;
                  end if;
               end Check_Scan_End;

               procedure Check_Scan_End (Token : in Lexer.Token)
               with Pre => Tree.Lexer.Is_Block_Delimited (Token.ID)
               --  Check if Inserted_Region inserts an end delimiter in Token,
               --  exposing the rest of Token as code.
               is begin
                  Check_Scan_End (Token.ID, Token.Byte_Region);
               end Check_Scan_End;

               procedure Check_Scan_End (Node : in Valid_Node_Access)
               with Pre => Tree.Lexer.Is_Block_Delimited (Tree.ID (Node))
               --  Check if Inserted_Region inserts an end delimiter in Node,
               --  exposing the rest of Node as code.
               is begin
                  Check_Scan_End (Tree.ID (Node), Tree.Byte_Region (Node, Trailing_Non_Grammar => False));
               end Check_Scan_End;

            begin
               if Lexer_Errors.Length > 0 and then
                 Lexer_Errors (Lexer_Errors.First).Scan_End /= Invalid_Buffer_Pos
               then
                  --  Lexer_Errors is set above to contain lexer errors that may be
                  --  affected by this KMN. test_incremental.adb Edit_String_06,
                  --  Lexer_Errors_nn.
                  declare
                     Data : Lexer_Error_Data renames Lexer_Errors (Lexer_Errors.First);
                     Ref : Stream_Node_Ref;

                     procedure Delete_Node_To_Terminal
                     --  Delete terminals Ref thru prev (Terminal); normally scanned
                     --  tokens get deleted in Delete_Scanned_Loop below, but that only
                     --  deletes tokens Terminal and after.
                     is begin
                        loop
                           Breakdown (Ref, To_Single => True);
                           declare
                              To_Delete : Stream_Node_Ref := Ref;
                           begin
                              Tree.Next_Terminal (Ref);
                              Tree.Stream_Delete (Stream, To_Delete.Element);
                           end;
                           exit when Ref.Node = Terminal.Node;
                        end loop;
                     end Delete_Node_To_Terminal;

                  begin
                     if Data.Scan_Start_Node /= Invalid_Node_Access then
                        --  Breakdown Terminal so we can delete terminals before Terminal.
                        Breakdown (Terminal);
                        Ref := Tree.To_Stream_Node_Ref (Stream, Data.Scan_Start_Node);
                        Delete_Node_To_Terminal;

                        Do_Scan        := True;
                        Lex_Start_Byte := Tree.Byte_Region (Data.Scan_Start_Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Char := Tree.Char_Region (Data.Scan_Start_Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Line := Tree.Line_Region (Ref, Trailing_Non_Grammar => False).First;
                        Scan_End       := Data.Scan_End;

                     elsif Data.Node = Terminal.Node then
                        --  Data.Node is not shifted, and Err may be before or after
                        --  Terminal.Byte_Region.
                        declare
                           Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Data.Node, Trailing_Non_Grammar => False);
                        begin
                           if Inserted_Region.First < Terminal_Byte_Region.First then
                              --  test_incremental.adb Lexer_Errors_07
                              Do_Scan        := True;
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Tree.Line_Region (Terminal, Trailing_Non_Grammar => False).First;
                              Scan_End       := Data.Scan_End;
                           else
                              Do_Scan        := True;
                              Lex_Start_Byte := Terminal_Byte_Region.First;
                              Lex_Start_Char := Tree.Char_Region (Data.Node, Trailing_Non_Grammar => False).First;
                              Lex_Start_Line := Tree.Line_Region (Terminal, Trailing_Non_Grammar => False).First;
                              Scan_End       := Data.Scan_End;
                           end if;
                        end;

                        if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                           Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                        end if;

                     elsif Tree.Byte_Region (Data.Node, Trailing_Non_Grammar => False).First <
                       Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First
                     then
                        --  Data.Node is shifted.

                        --  Breakdown Terminal so it does not share a stream element with
                        --  elements being deleted, and we can delete terminals before
                        --  Terminal.
                        Breakdown (Terminal);
                        Ref := Tree.To_Stream_Node_Ref (Stream, Data.Node);
                        Delete_Node_To_Terminal;

                        Do_Scan        := True;
                        Lex_Start_Byte := Tree.Byte_Region (Data.Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Char := Tree.Char_Region (Data.Node, Trailing_Non_Grammar => False).First;
                        Lex_Start_Line := Tree.Line_Region (Ref, Trailing_Non_Grammar => False).First;
                        Scan_End       := Data.Scan_End;

                        if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                           Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;
                        end if;

                     else
                        --  Scan start determined by Terminal, Terminal_Non_Grammar_Next below.
                        --  test_incremental.adb Lexer_Errors_03
                        Scan_End := Data.Scan_End;
                     end if;
                  end;

                  Lexer_Errors.Delete_First;
               end if;

               if Do_Scan then
                  --  Set from a Lexer_Error
                  null;

               elsif Delayed_Scan then
                  --  A previous edit start affected Terminal or Floating_Non_Grammar
                  --  (Delayed_Floating_Index). test_incremental.adb Edit_String_07
                  declare
                     Token_ID : constant WisiToken.Token_ID :=
                       (if Delayed_Floating_Index = Positive_Index_Type'Last
                        then Tree.ID (Terminal.Node)
                        else Floating_Non_Grammar (Delayed_Floating_Index).ID);

                     Token_Byte_Region : constant Buffer_Region :=
                       (if Delayed_Floating_Index = Positive_Index_Type'Last
                        then Tree.Byte_Region (Terminal)
                        else Floating_Non_Grammar (Delayed_Floating_Index).Byte_Region);
                  begin
                     if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                       Next_KMN_Stable_First < Token_Byte_Region.Last
                     then
                        --  Next change also edits the token; more delay.
                        null;
                     else
                        Do_Scan        := True;
                        Lex_Start_Byte := Delayed_Lex_Start_Byte;
                        Lex_Start_Char := Delayed_Lex_Start_Char;
                        Lex_Start_Line := Delayed_Lex_Start_Line;

                        if Tree.Lexer.Is_Block_Delimited (Token_ID) then
                           if Delayed_Floating_Index = Positive_Index_Type'Last then
                              Check_Scan_End (Terminal.Node);
                           else
                              Check_Scan_End (Floating_Non_Grammar (Delayed_Floating_Index));
                           end if;
                        end if;

                        Delayed_Scan := False;
                     end if;
                  end;

               elsif KMN.Inserted_Bytes = 0 and KMN.Deleted_Bytes = 0 then
                  --  Nothing to scan; last KMN
                  null;

               elsif Next_KMN_Stable_First + Shift_Bytes < Scanned_Byte_Pos + Base_Buffer_Pos (KMN.Deleted_Bytes) then
                  --  All of current edit has been scanned, and scan end is
                  --  not adjacent to KMN end. test_incremental.adb Edit_Comment_2
                  null;

               elsif Terminal_Byte_Region.Last + Shift_Bytes +
                 (if Inserted_Region.First <= Terminal_Byte_Region.Last + Shift_Bytes
                  then Base_Buffer_Pos (KMN.Inserted_Bytes)
                  else 0) > Scanned_Byte_Pos
                  --  Else Terminal was scanned by a previous KMN. test_incremental.adb
                  --  Edit_Code_12, _14, _16, ada_mode-recover_align_1.adb,
                  --  ada_mode-interactive_02.adb

               then
                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     --  Edit start is in Terminal_Non_Grammar_Next.
                     --  test_incremental.adb Edit_Comment*

                     declare
                        Non_Grammar  : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        Last_Floated : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
                     begin
                        declare
                           Token : Lexer.Token renames Non_Grammar (Terminal_Non_Grammar_Next);
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
                                   (Tree.Prev_Source_Terminal (Terminal, Trailing_Non_Grammar => True),
                                    Trailing_Non_Grammar => True).Last);
                              Do_Scan := True;
                           else
                              --  Edit start is in or just after Token
                              Lex_Start_Byte := Token.Byte_Region.First + Shift_Bytes;
                              Lex_Start_Char := Token.Char_Region.First + Shift_Chars;
                              Lex_Start_Line := Token.Line_Region.First + Shift_Lines;
                              Do_Scan := True;

                              if Tree.Lexer.Is_Block_Delimited (Token.ID) then
                                 Check_Scan_End (Token);
                              end if;
                           end if;
                        end;

                        --  Remaining Non_Grammar will either be scanned, or moved to a new
                        --  grammar token, so delete or move to floating now.
                        for I in Terminal_Non_Grammar_Next .. Non_Grammar.Last_Index loop
                           declare
                              Token       : Lexer.Token renames Non_Grammar (I);
                              Byte_Region : Buffer_Region renames Token.Byte_Region;
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
                                    --  test_incremental.adb Edit_Comment_03, _07, ada_mode-partial_parse.adb.
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
                                       Tree.Lexer.Trace.Put_Line
                                         ("scan delayed" & Lex_Start_Byte'Image &
                                            (if Scan_End /= Invalid_Buffer_Pos
                                             then " .." & Scan_End'Image
                                             else ""));
                                       if Trace_Incremental_Parse > Extra then
                                          Tree.Lexer.Trace.Put_Line
                                            ("float non_grammar" & I'Image & ":" &
                                               Lexer.Full_Image (Non_Grammar (I), Tree.Lexer.Descriptor.all));
                                       end if;
                                    end if;
                                 else
                                    --  Token overlaps or is adjacent to the change region; it will be
                                    --  rescanned. Delete it here (ie don't copy to floating). It may
                                    --  contain New_Lines. test_incremental.adb Delete_Comment_End.
                                    if Trace_Incremental_Parse > Extra then
                                       Tree.Lexer.Trace.Put_Line
                                         ("delete non_grammar" & I'Image & ":" &
                                            Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                                    end if;
                                    declare
                                       New_Line_Count : constant Base_Line_Number_Type := WisiToken.New_Line_Count
                                         (Non_Grammar (I).Line_Region);
                                    begin
                                       Shift_Lines := @ - New_Line_Count;
                                    end;
                                 end if;
                              else
                                 --  Token does not overlap the edit region; handle it later.
                                 Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                 Floating_Non_Grammar.Append (Non_Grammar (I));
                                 if Trace_Incremental_Parse > Extra then
                                    Tree.Lexer.Trace.Put_Line
                                      ("float non_grammar" & I'Image & ":" &
                                         Lexer.Full_Image (Non_Grammar (I), Tree.Lexer.Descriptor.all));
                                 end if;
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
                              Tree.Lexer.Trace.Put_Line
                                ("float non_grammar" & Terminal_Non_Grammar_Next'Image & " .." &
                                   Last_Floated'Image);
                           end if;
                        end if;

                        Terminal_Non_Grammar_Next := Lexer.Token_Arrays.No_Index;

                        Tree.Next_Terminal (Terminal);
                     end;

                  elsif Terminal_Byte_Region.First + Shift_Bytes < Inserted_Region.First then
                     --  Edit start is in Terminal, not at first byte. test_incremental.adb
                     --  Edit_Code_10, _11.

                     if Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID then
                        if Length (Inserted_Region) > 0 then
                           --  Scan new text inserted at EOI.
                           Do_Scan        := True;
                           Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                           Lex_Start_Char := Tree.Char_Region (Terminal.Node, Trailing_Non_Grammar => False).First +
                             Shift_Chars;

                           --  Line_Region.First is from prev_terminal.non_grammar, which is shifted
                           Lex_Start_Line := Tree.Line_Region (Terminal, Trailing_Non_Grammar => False).First;
                        else
                           --  We never re-scan eoi; we just shift it.
                           null;
                        end if;
                     else
                        Do_Scan        := True;
                        Lex_Start_Byte := Terminal_Byte_Region.First + Shift_Bytes;
                        Lex_Start_Char := Tree.Char_Region (Terminal.Node, Trailing_Non_Grammar => False).First +
                          Shift_Chars;

                        --  Line_Region.First is from prev_terminal.non_grammar, which is shifted
                        Lex_Start_Line := Tree.Line_Region (Terminal, Trailing_Non_Grammar => True).First;

                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Terminal.Node)) then
                           Check_Scan_End (Terminal.Node);
                        end if;
                     end if;

                  else
                     --  Edit start is in or adjacent to some non_grammar token or
                     --  whitespace preceding Terminal (including at Terminal first byte);
                     --  delete non_grammar tokens adjacent to, containing or after the
                     --  edit start; they will be rescanned (the scan loop exits on
                     --  terminals, not non_grammars). Deleted New_Lines decrement
                     --  Shift_Lines.

                     declare
                        procedure In_Whitespace
                        is begin
                           --  Edit start is in whitespace before Terminal.
                           --  test_incremental.adb Edit_Code_01, Edit_Whitespace_1, _2
                           --  ada_mode-incremental_04.adb
                           Lex_Start_Byte := Buffer_Pos'Max (Scanned_Byte_Pos + 1, Inserted_Region.First);
                           Lex_Start_Char := Buffer_Pos'Max (Scanned_Char_Pos + 1, Inserted_Region_Chars.First);

                           declare
                              Prev_Non_Grammar : Terminal_Ref := Tree.Prev_Terminal (Terminal);
                           begin
                              --  We can't use Tree.Line_Region (Prev) here, because if Prev has no
                              --  non_grammar, it uses the following non_grammar for result.last,
                              --  and that's not shifted yet. ada_mode-incremental_02.adb
                              if Tree.Non_Grammar_Const (Prev_Non_Grammar.Node).Length = 0 then
                                 Tree.Prev_Non_Grammar (Prev_Non_Grammar);
                              end if;
                              declare
                                 Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const
                                   (Prev_Non_Grammar.Node);
                              begin
                                 Lex_Start_Line := Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
                              end;
                           end;
                           Do_Scan := True;
                        end In_Whitespace;

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
                           if Non_Grammar.Length = 0 or else
                             Non_Grammar (Non_Grammar.Last_Index).ID = Tree.Lexer.Descriptor.SOI_ID
                           then
                              In_Whitespace;

                           elsif Last_Byte <= Scanned_Byte_Pos +
                             (if Deleted_Region.First <= Last_Byte
                              then Base_Buffer_Pos (KMN.Deleted_Bytes)
                              else 0)
                           then
                              --  All of Non_Grammar has been scanned already.
                              --  test_incremental.adb Edit_Comment_10, _17.

                              if (KMN.Inserted_Bytes = 0 or else
                                    (Inserted_Region.Last <= Scanned_Byte_Pos and
                                       Inserted_Region.Last < Terminal_Byte_Region.First + Shift_Bytes - 1)) and
                                (KMN.Deleted_Bytes = 0 or else
                                   (Deleted_Region.Last + Shift_Bytes <=
                                      Scanned_Byte_Pos + Base_Buffer_Pos (KMN.Deleted_Bytes) and
                                      Deleted_Region.Last < Terminal_Byte_Region.First - 1))
                              then
                                 --  Inserted and Deleted have been scanned already, and are not
                                 --  adjacent to Terminal. test_incremental.adb Edit_Code_14,
                                 --  Edit_Comment_10 ada_mode-interactive_02.adb
                                 null;

                              else
                                 In_Whitespace;
                              end if;
                           else
                              for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                 declare
                                    Byte_Last : constant Buffer_Pos := Non_Grammar (I).Byte_Region.Last +
                                      (if Floating then Shift_Bytes else 0);
                                 begin
                                    if Byte_Last + 1 >= Inserted_Region.First then
                                       --  We don't need to check Scanned_Byte_Pos here; we always scan all
                                       --  consecutive non_grammars, and we checked Scanned_Byte_Pos above.
                                       --  ada_mode-recover_align_1.adb, test_incremental.adb Edit_Comment_2
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
                                    if Tree.Lexer.Is_Block_Delimited (Token.ID) and
                                      Inserted_Region.First < Token.Byte_Region.Last
                                      --  Inserting in middle of Token, not adding to end.
                                    then
                                       declare
                                          Delimiter_Pos : constant Base_Buffer_Pos := Tree.Lexer.Contains_End_Delimiter
                                            (Token.ID, Inserted_Region);
                                       begin
                                          if Delimiter_Pos /= Invalid_Buffer_Pos then
                                             --  A new end delimiter is inserted in Token, exposing the rest of
                                             --  Token as code. test_incremental.adb Edit_Comment_4, Edit_Comment_7
                                             Scan_End := Tree.Lexer.Find_Scan_End
                                               (Token.ID,
                                                (Delimiter_Pos, Invalid_Buffer_Pos),
                                                Inserted => True,
                                                Start    => False);

                                             if Trace_Incremental_Parse > Detail then
                                                Tree.Lexer.Trace.Put_Line
                                                  ("end delimiter inserted:" &
                                                     Token.Byte_Region.First'Image & " .." &
                                                     Scan_End'Image);
                                             end if;
                                          end if;
                                       end;
                                    end if;

                                    Lex_Start_Byte := Buffer_Pos'Min
                                      (Token.Byte_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region.First);

                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region_Chars.First);

                                    if Floating then
                                       --  Tokens Delete .. Non_Grammar.Last contributed to Shift_Lines;
                                       --  ignore that contribution because they are after the lex start.
                                       --  test_incremental.adb Edit_Code_10
                                       --  ada_mode-interactive_10.adb
                                       --  ada_mode-recover_14 comment after extra 'begin'.
                                       declare
                                          Temp_Shift_Lines : Base_Line_Number_Type := Shift_Lines;
                                       begin
                                          for I in Delete .. Non_Grammar.Last_Index loop
                                             Temp_Shift_Lines := @ + New_Line_Count (Non_Grammar (I).Line_Region);
                                          end loop;
                                          Lex_Start_Line := Token.Line_Region.First + Temp_Shift_Lines;
                                       end;
                                    else
                                       Lex_Start_Line := Token.Line_Region.First;
                                    end if;
                                 end;

                                 if Trace_Incremental_Parse > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ((if Floating
                                        then "delete floating_non_grammar"
                                        else "delete non_grammar") &
                                         Delete'Image & " .." & Non_Grammar.Last_Index'Image);
                                 end if;

                                 if not Floating then
                                    for I in Delete .. Non_Grammar.Last_Index loop
                                       Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                    end loop;
                                 end if;

                                 Non_Grammar.Set_First_Last (Non_Grammar.First_Index, Delete - 1);
                              else
                                 In_Whitespace;
                              end if;
                           end if;
                        end Handle_Non_Grammar;
                     begin
                        if Tree.Lexer.Is_Block_Delimited (Tree.ID (Terminal.Node)) then
                           Check_Scan_End (Terminal.Node);
                        end if;

                        if Floating_Non_Grammar.Length > 0 and then
                          Floating_Non_Grammar (Floating_Non_Grammar.First_Index).Byte_Region.First + Shift_Bytes <=
                          Inserted_Region.First
                        then
                           --  The edit start is in a floated non_grammar.
                           --  test_incremental.adb Edit_Comment_7, Edit_Code_10, _17
                           --  ada_mode-recover_14.adb comment after deleted "begin".
                           Handle_Non_Grammar (Floating_Non_Grammar, Floating => True);
                        elsif Tree.ID (Terminal.Node) /= Tree.Lexer.Descriptor.SOI_ID then
                           Handle_Non_Grammar
                             (Tree.Non_Grammar_Var (Tree.Prev_Terminal (Terminal).Node), Floating => False);
                        end if;
                     end;
                  end if;

                  if Do_Scan and not Delayed_Scan then
                     if (Next_KMN.Deleted_Bytes > 0 or Next_KMN.Inserted_Bytes > 0) and then
                       Next_KMN_Stable_Last < Terminal_Byte_Region.Last
                     then
                        --  Next change is an actual change (not just last placeholder KMN),
                        --  and it also overlaps this token. It may insert or delete a delimiter
                        --  end, so we don't know when to end a scan; handle it then.
                        --  test_incremental.adb Edit_String_07.
                        Do_Scan                := False;
                        Delayed_Scan           := True;
                        Delayed_Floating_Index := Positive_Index_Type'Last;
                        Delayed_Lex_Start_Byte := Lex_Start_Byte;
                        Delayed_Lex_Start_Char := Lex_Start_Char;
                        Delayed_Lex_Start_Line := Lex_Start_Line;

                        if Trace_Incremental_Parse > Detail then
                           Tree.Lexer.Trace.Put_Line ("scan delayed");
                        end if;
                     end if;
                  end if;
               end if;

               if Do_Scan then
                  Last_Scanned_Token := (others => <>);

                  if Trace_Incremental_Parse > Detail then
                     Tree.Lexer.Trace.Put_Line
                       ("lexer.set_position" & Lex_Start_Byte'Image & Lex_Start_Char'Image & Lex_Start_Line'Image);
                     if Scan_End /= Invalid_Buffer_Pos then
                        Tree.Lexer.Trace.Put_Line ("scan_end " & Scan_End'Image);
                     end if;
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
                        Token       : Lexer.Token;
                        Error_Count : constant Natural := Tree.Lexer.Find_Next (Token);
                        Ref         : Terminal_Ref;
                        Scan_Errors : Error_Data_Lists.List;
                     begin
                        if Trace_Lexer > Outline then
                           Tree.Lexer.Trace.Put_Line ("lex: " & Lexer.Image (Token, Parser.Tree.Lexer.Descriptor.all));
                        end if;

                        if Error_Count > 0 then
                           declare
                              Cur : WisiToken.Lexer.Error_Lists.Cursor := Tree.Lexer.Errors.Last;
                           begin
                              for I in 1 .. Error_Count - 1 loop
                                 WisiToken.Lexer.Error_Lists.Previous (Cur);
                              end loop;
                              for I in 1 .. Error_Count loop
                                 declare
                                    Error : Lexer.Error renames Tree.Lexer.Errors (Cur);
                                 begin
                                    Scan_Errors.Append (Lexer_Error'(Error => Error));

                                    if Trace_Lexer > Outline then
                                       Tree.Lexer.Trace.Put_Line
                                         (" ... error: " & Error.Char_Pos'Image &
                                            (if Error.Recover_Char (1) /= ASCII.NUL
                                             then "'" & Error.Recover_Char (1) & "'"
                                             else ""));
                                    end if;
                                 end;
                                 WisiToken.Lexer.Error_Lists.Next (Cur);
                              end loop;
                           end;
                        end if;

                        exit Scan_Changed_Loop when Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal and
                          Scan_End /= Invalid_Buffer_Pos
                        then
                           if Token.Byte_Region.First > Scan_End then
                              --  test_incremental.adb Edit_Comment_4, _5, _7, Delete_Comment_Start,
                              --  edit_String_*
                              Scan_End := Invalid_Buffer_Pos;
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
                                       --  Previously existing token starts immediately after deleted region;
                                       --  it may have been truncated (test_incremental.adb Edit_Code_4 'Cc')
                                    ));
                        end if;

                        Scanned_Byte_Pos   := Token.Byte_Region.Last;
                        Scanned_Char_Pos   := Token.Char_Region.Last;
                        Last_Scanned_Token := Token;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal then
                           --  grammar token
                           Ref := Tree.Insert_Source_Terminal
                             (Stream, Token,
                              Before => Terminal.Element,
                              Errors => Scan_Errors);

                           Process_Grammar_Token (Parser, Token, Ref.Node);
                           Last_Grammar := Ref;

                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line ("scan new " & Tree.Image (Ref));
                           end if;

                        else
                           --  non_grammar token
                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("scan new " & Lexer.Full_Image (Token, Parser.Tree.Lexer.Descriptor.all));
                           end if;

                           if Error_Count > 0 then
                              --  test_incremental.adb Lexer_Errors_04
                              Tree.Add_Errors (Tree.Shared_Stream, Last_Grammar.Node, Scan_Errors);
                           end if;
                           Process_Non_Grammar_Token (Parser, Last_Grammar.Node, Token);
                           Shift_Lines := @ + New_Line_Count (Token.Line_Region);
                        end if;

                     end;
                  end loop Scan_Changed_Loop;
               end if;

               --  Do this here so Shift_Bytes is correct in Delete_Scanned_Loop.
               --
               --  However, if Terminal is before the edit region, the previous
               --  shift applies. test_incremental.adb Edit_Whitespace
               Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
               Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

               Shift_Lines := @ + Deleted_Shift_Lines;

               Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
               Old_Char_Pos := Stable_Region_Chars.Last + KMN.Deleted_Chars;
               New_Byte_Pos := Inserted_Region.Last;
               New_Char_Pos := Inserted_Region_Chars.Last;
               pragma Assert (New_Byte_Pos - Old_Byte_Pos = Shift_Bytes);

               if Last_Scanned_Token.ID /= Invalid_Token_ID then
                  --  If Last_Scanned_Token.ID = Invalid_Token_ID, only whitespace was
                  --  scanned; Edit_Comment_8
                  --
                  --  We don't check Do_Scan, because we may be processing the next KMN
                  --  covered by a previous scan.
                  --
                  --  Delete tokens in the current KMN that were replaced by the scan.
                  --
                  --  If a scan covers more than one KMN, we can't process more than the
                  --  first because Shift_* is not known. test_incremental.adb
                  --  Edit_Code_4, Edit_Comment_13, ada_skel.adb ada-skel-return.
                  --
                  --  If the last token scanned was a comment created by an inserted
                  --  comment start or extended by a deleted comment end then we must
                  --  delete all tokens are now part of by the comment.
                  --  test_incremental.adb Delete_Comment_End, Edit_Comment_9,
                  --  Insert_Comment_Start
                  Delete_Scanned_Loop :
                  loop
                     exit Delete_Scanned_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

                     exit Delete_Scanned_Loop when Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First > Next_KMN_Stable_Last;
                     --  Terminal is in next KMN; we don't know Shift_Bytes to compare to
                     --  Scanned_Byte_Pos. test_incremental.adb Edit_Comment_13

                     exit Delete_Scanned_Loop when Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First + Shift_Bytes -
                       (if Tree.Byte_Region (Terminal.Node, Trailing_Non_Grammar => False).First <= Stable_Region.Last
                        then -KMN.Deleted_Bytes + KMN.Inserted_Bytes else 0) > Scanned_Byte_Pos;

                     if Tree.ID (Terminal) = Tree.Lexer.Descriptor.SOI_ID then
                        Tree.Next_Terminal (Terminal);

                     else
                        --  Ensure Terminal is Single, so we can delete it.
                        Breakdown (Terminal, To_Single => True);

                        declare
                           To_Delete : Stream_Node_Ref := Terminal;
                        begin
                           Tree.Next_Terminal (Terminal);
                           if Trace_Incremental_Parse > Detail then
                              Tree.Lexer.Trace.Put_Line
                                ("delete modified " &
                                   Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => True));
                           end if;

                           --  We always scan all non_grammar immediately following a scanned
                           --  terminal.
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
                                    Tree.Lexer.Trace.Put_Line
                                      ("delete non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                              else
                                 if Trace_Incremental_Parse > Detail then
                                    Tree.Lexer.Trace.Put_Line
                                      ("float non_grammar " & Lexer.Full_Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                                 Floating_Non_Grammar.Append (Token);
                              end if;
                           end loop;

                           pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                           Tree.Stream_Delete (Stream, To_Delete.Element);
                           Maybe_Delete_Lexer_Errors (To_Delete.Node);
                        end;
                     end if;
                  end loop Delete_Scanned_Loop;
               end if;

               --  If any Floating_Non_Grammar are in this KMN's change region or next
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
                     Terminal_First : constant Buffer_Pos := Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First;
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
                           if Tree.ID (Terminal) = Tree.Lexer.Descriptor.SOI_ID then
                              return Terminal;
                           end if;

                           declare
                              --  Target_Bytes is unshifted. If searching forward, all nodes are
                              --  also unshifted. If searching back, all nodes except Terminal are
                              --  shifted. Compare Target_Bytes to unshifted region.
                              --
                              --  region bounds test case: ada_mode-recover_partial_14.adb
                              --  contains; ada_mode-recover_42.adb lexer_error string_literal
                              Shift_First : constant Base_Buffer_Pos := -Shift_Bytes;
                              Shift_Last : constant Base_Buffer_Pos := (if Before = Terminal then 0 else -Shift_Bytes);

                              First : constant Buffer_Pos := Tree.Byte_Region
                                (Before_1.Node, Trailing_Non_Grammar => False).Last + 1 + Shift_First;
                              Last  : constant Buffer_Pos :=
                                (if After
                                 then Tree.Byte_Region (Before).Last
                                 else Tree.Byte_Region (Before).First - 1) + Shift_Last;
                           begin
                              exit when Target_Bytes in First .. Last;
                           end;
                        else
                           declare
                              First : constant Buffer_Pos := Tree.Byte_Region
                                (Before_1.Node, Trailing_Non_Grammar => False).Last + 1;
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
                     if Token.Byte_Region.First < Tree.Byte_Region
                       (Terminal.Node, Trailing_Non_Grammar => False).First
                     then
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
                        Tree.Lexer.Trace.Put_Line
                          ("restore floating_non_grammar " & Lexer.Image (Old_Token, Tree.Lexer.Descriptor.all));
                        Tree.Lexer.Trace.Put_Line
                          (" ... to " & Tree.Image (Containing_Terminal, Non_Grammar => True));
                     end if;
                  end Restore;

               begin
                  if Delayed_Scan and then
                    (Delayed_Floating_Index = Positive_Index_Type'Last or
                       Floating_Non_Grammar.First_Index = Delayed_Floating_Index)
                  then
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
                              Tree.Lexer.Trace.Put_Line
                                ("delete floating_non_grammar " & Lexer.Full_Image
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
                  Tree.Lexer.Trace.Put_Line ("final shift " & Tree.Image (Terminal, Non_Grammar => True));
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
               if Trace_Incremental_Parse > Outline then
                  Tree.Lexer.Trace.Put_Line ("edit_tree: validate_tree failed");
                  Tree.Print_Streams (Children => True, Non_Grammar => True);
               end if;
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
            Error_Ref  : constant Stream_Error_Ref  := Error (Cur);
            Error_Node : constant Valid_Node_Access := Tree.Error_Node (Error_Ref).Node;
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
