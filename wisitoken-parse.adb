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

   function Image (Item : in Wrapped_Lexer_Error; Tree : in WisiToken.Syntax_Trees.Tree) return String
   is
      use Ada.Strings.Unbounded;
      use WisiToken.Syntax_Trees;
      Result : Unbounded_String;
   begin
      Append (Result, "(");
      if Item.Recover_Token_Ref /= Invalid_Stream_Node_Ref then
         Append (Result, Tree.Image (Item.Recover_Token_Ref.Node, Node_Numbers => True));
         Append (Result, ", ");
      end if;
      Append (Result, "(" & Item.Error.Char_Pos'Image & ", '");
      for C of Item.Error.Recover_Char loop
         if C /= ASCII.NUL then
            Append (Result, C);
         end if;
      end loop;
      Append (Result, "'");
      return To_String (Result);
   end Image;

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
            Ref := Tree.Add_Terminal (Parser.Tree.Shared_Stream, Token);
            Process_Grammar_Token (Parser, Token, Ref.Node);
            Last_Grammar_Node := Ref.Node;
         else
            if Trace_Lexer > Detail then
               Parser.Trace.Put_Line
                 (if Last_Grammar_Node = Invalid_Node_Access
                  then "leading non-grammar"
                  else "non-grammar in " & Parser.Tree.Image (Last_Grammar_Node));
            end if;
            Process_Non_Grammar_Token (Parser, Last_Grammar_Node, Token);
            Ref := Invalid_Stream_Node_Ref;
         end if;

         if Error then
            Parser.Wrapped_Lexer_Errors.Append
              ((Recover_Token_Ref => Ref,
                Error             => Lexer.Errors (Lexer.Errors.Last)));
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

   overriding function Copy (Data : in Parse_Error) return Syntax_Trees.Error_Data_Access
   is
      New_Data : constant Parse_Error_Access := new Parse_Error'(Data);
   begin
      return Syntax_Trees.Error_Data_Access (New_Data);
   end Copy;

   overriding function Image
     (Data       : in Parse_Error;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is
      use Ada.Strings.Unbounded;
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.Valid_Node_Access;

      Result : Unbounded_String;

      Item_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Error_Node);
      Msg : constant String := "syntax error: expecting " & Image (Data.Expecting, Tree.Lexer.Descriptor.all) &
        ", found '" & Tree.Lexer.Buffer_Text (Item_Byte_Region) &
        "'";
   begin
      --  If we get here because Parse raised Syntax_Error or other
      --  exception, Finish_Parse has not been called, so tree is not
      --  editable.

      if Tree.Editable then
         Result := +Tree.Error_Message (Error_Node, Msg);
      else
         raise SAL.Not_Implemented;
         --  FIXME: Need test case. set parents in Stream, so can find line from
         --  Error_Node. Or pass stream_node_Parents?
      end if;

      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         Append (Result, "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all));
      end if;

      return -Result;
   end Image;

   overriding function Copy (Data : in In_Parse_Action_Error) return Syntax_Trees.Error_Data_Access
   is
      New_Data : constant In_Parse_Action_Error_Access := new In_Parse_Action_Error'(Data);
   begin
      return Syntax_Trees.Error_Data_Access (New_Data);
   end Copy;

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
      Result := +Tree.Error_Message
        (Error_Node, "in parse action error: (" & In_Parse_Actions.Image (Data.Status, Tree, Error_Node));

      if Recover_Op_Arrays.Length (Data.Recover_Ops) /= 0 then
         Append (Result, ASCII.LF & "   recovered: " & Image (Data.Recover_Ops, Tree.Lexer.Descriptor.all));
      end if;

      return -Result;
   end Image;

   overriding function Copy (Data : in Error_Message) return Syntax_Trees.Error_Data_Access
   is
      New_Data : constant Error_Message_Access := new Error_Message'(Data);
   begin
      return Syntax_Trees.Error_Data_Access (New_Data);
   end Copy;

   overriding function Image
     (Data       : in Error_Message;
      Tree       : in Syntax_Trees.Tree'Class;
      Error_Node : in Syntax_Trees.Valid_Node_Access)
     return String
   is begin
      return -Data.Msg;
   end Image;

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
      --  These are _not_ shifted, because the correct shift is unknown at
      --  the time they are detached. ada_mode-recover_42.adb
      --
      --  If a non_grammar is floated from a scanned node, it is unshifted
      --  to be consistent.

      Comment_End_Deleted : Boolean         := False;
      New_Comment_End     : Base_Buffer_Pos := Invalid_Buffer_Pos;
      --  If Comment_End_Deleted, a comment end was deleted; delete all tree
      --  tokens thru New_Comment_End (which is not shifted).

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
            Tree.Breakdown (Terminal);

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

      Stream := Tree.Shared_Stream;

      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream));

      if Edits.Length = 0 then
         return;
      end if;

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

            if Trace_Incremental_Parse > Outline then
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
                     Children    => Trace_Incremental_Parse > Detail,
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

               if WisiToken.Trace_Incremental_Parse > Detail then
                  Parser.Trace.New_Line;
               end if;
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

            else
               --  Restore Parser.Deleted_Nodes that overlap or are adjacent to the
               --  edit region; they may be modified or deleted. We do this before
               --  Unchanged_Loop, because a restored node may be the Terminal that
               --  exits that loop. ada_mode-interactive_2.adb slowy insert comment.
               declare
                  use Syntax_Trees.Valid_Node_Access_Lists;
                  Cur : Syntax_Trees.Valid_Node_Access_Lists.Cursor := Parser.Deleted_Nodes.First;

                  procedure Delete (Label : in String)
                  is
                     To_Delete : Cursor := Cur;
                  begin
                     Cur := Next (Cur);

                     if Label'Length > 0 and Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("delete " & Label & " Deleted_Node " & Tree.Image
                             (Parser.Deleted_Nodes (To_Delete), Node_Numbers => True));
                     end if;
                     Parser.Deleted_Nodes.Delete (To_Delete);
                  end Delete;

               begin
                  loop
                     exit when not Has_Element (Cur);

                     if Tree.Label (Parser.Deleted_Nodes (Cur)) in Virtual_Terminal_Label then
                        --  It has no byte_region, and can just be ignored. We delete it here
                        --  so we don't consider it again.
                        Delete ("virtual");

                     else
                        declare
                           Byte_Region : constant Buffer_Region := Tree.Byte_Region (Parser.Deleted_Nodes (Cur));
                        begin
                           if Byte_Region.Last < Stable_Region.Last then
                              Delete ("stable");

                           elsif (KMN.Deleted_Bytes > 0 and Byte_Region.First <= Deleted_Region.Last + 1) or
                             (KMN.Inserted_Bytes > 0 and Byte_Region.First + Shift_Bytes <= Inserted_Region.Last + 1)
                           then
                              declare
                                 function Find_Insert return Terminal_Ref
                                 is begin
                                    --  FIXME: can this return SOI? - need test case.
                                    return Tree.Find_Byte_Pos
                                      (Stream, Byte_Region.First,
                                       Trailing_Non_Grammar => False,
                                       Start_At             => Terminal);
                                 end Find_Insert;

                                 Insert_Before : Terminal_Ref := Find_Insert;
                              begin
                                 if Insert_Before = Invalid_Stream_Node_Ref then
                                    --  Deleted node is after Tree.EOI
                                    Insert_Before := (Stream, Tree.Stream_Last (Stream), Tree.EOI);

                                 elsif Terminal.Element = Insert_Before.Element then
                                    pragma Assert (Terminal.Node /= Insert_Before.Node);
                                    if Terminal.Node = Tree.First_Terminal (Get_Node (Terminal.Element)) then
                                       --  test_incremental.adb Modify_Deleted_Element
                                       Tree.Prev_Terminal (Terminal);
                                       Breakdown (Insert_Before);
                                       Tree.Next_Terminal (Terminal);
                                    else
                                       Breakdown (Terminal);
                                       Insert_Before := Find_Insert;
                                    end if;

                                 else
                                    Breakdown (Insert_Before);
                                 end if;

                                 declare
                                    Prev_Insert_Before : constant Terminal_Ref := Tree.Prev_Terminal (Insert_Before);
                                    Prev_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var
                                      (Prev_Insert_Before.Node);
                                    First_To_Move : Positive_Index_Type := Positive_Index_Type'Last;
                                    Deleted_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var
                                      (Parser.Deleted_Nodes (Cur));
                                 begin
                                    if Prev_Non_Grammar.Length > 0 and then
                                      Byte_Region.First < Prev_Non_Grammar (Prev_Non_Grammar.Last_Index)
                                      .Byte_Region.Last
                                    then
                                       --  Move some Prev_Non_Grammar to deleted_node
                                       --  test_incremental.adb Modify_Deleted_Element
                                       for I in Prev_Non_Grammar.First_Index .. Prev_Non_Grammar.Last_Index loop
                                          if Byte_Region.First < Prev_Non_Grammar (I).Byte_Region.Last then
                                             First_To_Move := I;
                                             exit;
                                          end if;
                                       end loop;
                                       for I in First_To_Move .. Prev_Non_Grammar.Last_Index loop
                                          Deleted_Non_Grammar.Append (Prev_Non_Grammar (I));
                                       end loop;
                                       if First_To_Move = Prev_Non_Grammar.First_Index then
                                          Prev_Non_Grammar.Clear;
                                       else
                                          Prev_Non_Grammar.Set_First_Last
                                            (Prev_Non_Grammar.First_Index, First_To_Move - 1);
                                       end if;
                                    end if;
                                 end;

                                 Tree.Stream_Insert (Stream, Parser.Deleted_Nodes (Cur), Insert_Before.Element);

                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("restore Deleted_Node " & Tree.Image
                                         (Parser.Deleted_Nodes (Cur), Node_Numbers => True) &
                                         " before " & Tree.Image (Insert_Before.Node, Node_Numbers => True));
                                    Parser.Trace.Put_Line
                                      ("stream:" & Tree.Image
                                         (Stream,
                                          Children    => Trace_Incremental_Parse > Detail,
                                          Non_Grammar => True,
                                          Augmented   => True));
                                 end if;
                                 Delete ("");

                              end;
                           else
                              --  Remaining nodes are in following KMN.
                              exit;
                           end if;
                        end;
                     end if;
                  end loop;
               end;

               --  It is tempting to skip Unchanged_Loop if Shift_Bytes = 0 and
               --  Shift_Chars = 0 and Shift_Lines = 0. But we need to scan all
               --  Non_Grammar for Floating_Non_Grammar, which changes Shift_Lines.
               --  FIXME: only need to scan trailing stable terminal?

               Unchanged_Loop :
               loop
                  exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
                  exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  if Terminal_Non_Grammar_Next = Lexer.Token_Arrays.No_Index then
                     exit Unchanged_Loop when not Contains
                       (Inner          => Tree.Byte_Region (Terminal.Node),
                        Outer          => Stable_Region,
                        First_Boundary => Inclusive,
                        Last_Boundary  => (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                                           then Inclusive
                                           else Exclusive));

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
            begin
               if Terminal_Byte_Region.Last + Shift_Bytes > Scanned_Byte_Pos
                 --  else Terminal was scanned by a previous KMN.
                 --  ada_mode-recover_align_1.adb, ada_mode-interactive_02.adb
                 and Next_KMN_Stable_First + Shift_Bytes > Scanned_Byte_Pos
                 --  else all of current edit has been scanned. test_incremental.adb Edit_Comment_2
               then
                  if Terminal_Non_Grammar_Next /= Lexer.Token_Arrays.No_Index then
                     --  Edit start is in Terminal.Non_Grammar.
                     --  test_incremental.adb Edit_Comment
                     declare
                        Non_Grammar  : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (Terminal.Node);
                        Last_Floated : Lexer.Token_Arrays.Extended_Index := Lexer.Token_Arrays.No_Index;
                     begin
                        if Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.First + Shift_Bytes >
                          Inserted_Region.First
                        then
                           --  Edit start is in whitespace
                           Lex_Start_Byte := Inserted_Region.First;
                           Lex_Start_Char := Inserted_Region_Chars.First;
                           Lex_Start_Line :=
                             (if Terminal_Non_Grammar_Next > Non_Grammar.First_Index
                              then Non_Grammar (Terminal_Non_Grammar_Next - 1).Line_Region.Last
                              else Tree.Line_Region
                                (Tree.Prev_Source_Terminal (Terminal, Trailing_Non_Grammar => True)).Last);
                           Do_Scan := True;
                        else
                           Lex_Start_Byte := Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.First + Shift_Bytes;
                           Lex_Start_Char := Non_Grammar (Terminal_Non_Grammar_Next).Char_Region.First + Shift_Chars;
                           Lex_Start_Line := Non_Grammar (Terminal_Non_Grammar_Next).Line_Region.First + Shift_Lines;
                           Do_Scan := True;

                           if KMN.Deleted_Bytes > 0 and
                             Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.First < Deleted_Region.First and
                             Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.Last <= Deleted_Region.Last
                           then
                              --  test_incremental.adb Delete_Comment_End,
                              --  ada_mode-interactive_05.adb Ada_Identifier in comment.
                              Comment_End_Deleted := True;
                              declare
                                 Ref   : Terminal_Ref;
                                 Index : Positive_Index_Type;
                              begin
                                 Tree.Next_New_Line
                                   (Start_Ref          => Terminal,
                                    After_Non_Grammar  => Terminal_Non_Grammar_Next,
                                    Result_Ref         => Ref,
                                    Result_Non_Grammar => Index);

                                 New_Comment_End := Tree.Non_Grammar_Const (Ref.Node)(Index).Byte_Region.Last;
                              end;
                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("comment_end_deleted:" &
                                      Non_Grammar (Terminal_Non_Grammar_Next).Byte_Region.First'Image & " .." &
                                      New_Comment_End'Image);
                              end if;
                           end if;
                        end if;

                        --  Remaining Non_Grammar will either be scanned, or moved to
                        --  a new grammar token, so delete or move to floating now.
                        for I in Terminal_Non_Grammar_Next .. Non_Grammar.Last_Index loop
                           declare
                              Byte_Region : Buffer_Region renames Non_Grammar (I).Byte_Region;
                           begin
                              if (Deleted_Region /= Null_Buffer_Region or else
                                    Byte_Region.First > Deleted_Region.Last)
                                and
                                (Inserted_Region /= Null_Buffer_Region or else
                                   Byte_Region.First + Shift_Bytes > Inserted_Region.Last)
                              then
                                 --  Token is not in the edit region; handle it later.
                                 Shift_Lines := @ - New_Line_Count (Non_Grammar (I).Line_Region);
                                 Floating_Non_Grammar.Append (Non_Grammar (I));
                                 Last_Floated := I;

                              else
                                 --  Token is in the change region. Delete it here (ie don't copy to
                                 --  floating); it will be rescanned.
                                 null;
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
                                ("float non_grammar" & Terminal_Non_Grammar_Next'Image & " .." &
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
                     --  to, containing or after the edit start. Deleted New_Lines
                     --  decrement Shift_Lines.
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
                                 --
                                 --  If not Floating, Non_Grammar (Delete) is before Terminal and has
                                 --  already been shifted. If Floating, Non_Grammar has not been
                                 --  shifted.

                                 declare
                                    Token : WisiToken.Lexer.Token renames Non_Grammar (Delete);
                                 begin
                                    Lex_Start_Byte := Buffer_Pos'Min
                                      (Token.Byte_Region.First + (if Floating then Shift_Bytes else 0),
                                       Inserted_Region.First);

                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First + (if Floating then Shift_Chars else 0),
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
                                        then "delete floating non_grammar"
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
                              --  This token, and any preceding virtuals, will be deleted below.
                              --  Ensure we scan all of this non_grammar; this may be overridden by
                              --  a preceding virtual token.
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
                           then
                              --  The edit start is in a floated non_grammar;
                              --  ada_mode-recover_14.adb comment after deleted "begin".
                              Handle_Non_Grammar (Floating_Non_Grammar, Floating => True);
                           else
                              Handle_Non_Grammar (Tree.Non_Grammar_Var (Last_Grammar.Node), Floating => False);
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               if Do_Scan then

                  if Trace_Incremental_Parse > Outline then
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

                  --  Delete trailing Virtual_Terminals that are immediately before the
                  --  edit region; they were inserted by error recover, so they may
                  --  be wrong now. Virtual_Terminals immediately after are
                  --  deleted in Delete_Loop below.
                  declare
                     Term      : Stream_Node_Ref := Tree.Stream_Prev (Terminal);
                     To_Delete : Stream_Node_Ref;

                     procedure Delete_Empty
                     with Pre => Tree.Is_Empty_Nonterm (Tree.Get_Node (To_Delete.Stream, To_Delete.Element))
                     is begin
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line ("delete " & Tree.Image (To_Delete));
                        end if;

                        Tree.Stream_Delete (Term.Stream, To_Delete.Element);
                     end Delete_Empty;

                     procedure Delete
                     with Pre => Tree.Label (Tree.Get_Node (To_Delete.Stream, To_Delete.Element)) in Terminal_Label
                                 and To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI
                     is
                        Non_Grammar   : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Var (To_Delete.Node);
                        Prev_Terminal : constant Node_Access :=
                          (if Non_Grammar.Length = 0
                           then Invalid_Node_Access
                           else Tree.Prev_Terminal (To_Delete).Node);
                     begin
                        if Non_Grammar.Length > 0 then
                           --  Virtual terminals may have non_grammar from Insert_Token; see
                           --  ada_mode-interactive_05.adb Proc_2, ada_mode-recover_18.adb.
                           for Token of Non_Grammar loop
                              if Token.Byte_Region.First >= Lex_Start_Byte then
                                 --  This token will be scanned, so we don't copy it here (equivalent
                                 --  to deleting it). ada_mode-interactive_1.adb Proc_2, Func_2
                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("delete non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                                 Shift_Lines := @ - New_Line_Count (Token.Line_Region);
                              else
                                 --  We don't float this non_grammar, because it's shifted and we know
                                 --  where it goes.
                                 Tree.Non_Grammar_Var (Prev_Terminal).Append (Token);
                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("move non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all) & " to " &
                                         Tree.Image (Prev_Terminal));
                                 end if;
                              end if;
                           end loop;
                           Non_Grammar.Clear;
                        end if;

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete trailing virtual" & Tree.Image (To_Delete.Node, Terminal_Node_Numbers => True));
                        end if;

                        Tree.Stream_Delete (Term.Stream, To_Delete.Element);
                     end Delete;

                  begin
                     Delete_Virtuals_Loop :
                     loop
                        exit Delete_Virtuals_Loop when Term.Node = Invalid_Node_Access;

                        case Tree.Label (Term.Element) is
                        when Source_Terminal =>
                           exit Delete_Virtuals_Loop;

                        when Nonterm =>
                           --  Check if trailing terminals are virtual or empty
                           declare
                              Last_Term : constant Node_Access := Tree.Last_Terminal
                                (Tree.Get_Node (Term.Stream, Term.Element));
                           begin
                              if Last_Term = Invalid_Node_Access then
                                 --  empty nonterm; check for preceding virtuals
                                 To_Delete := Term;
                                 Tree.Stream_Prev (Term);
                                 Delete_Empty;

                              elsif Tree.Label (Last_Term) = Source_Terminal then
                                 exit Delete_Virtuals_Loop;

                              else
                                 --  Delete trailing virtual terminal.
                                 declare
                                    Temp : Terminal_Ref := (Term.Stream, Term.Element, Last_Term);
                                 begin
                                    Tree.Right_Breakdown (Temp);
                                    To_Delete := Temp;
                                    Term := Tree.Stream_Prev (Temp);
                                    Delete;
                                 end;
                              end if;
                           end;

                        when Virtual_Terminal =>
                           To_Delete := Term;
                           Tree.Stream_Prev (Term);
                           Delete;

                        when Virtual_Identifier =>
                           raise SAL.Programmer_Error with "support virtual_identifier in edit_tree";
                        end case;
                     end loop Delete_Virtuals_Loop;
                  end;

                  Last_Grammar := Tree.Prev_Terminal (Terminal);

                  Scan_Changed_Loop :
                  loop
                     declare
                        Token : Lexer.Token; -- Newly scanned, and thus shifted
                        Error : constant Boolean := Tree.Lexer.Find_Next (Token);
                        Ref   : Terminal_Ref;

                        function Deleted_Comment_Start return Boolean
                        --  True if Deleted_Bytes deletes a comment start; Token was in the
                        --  comment. test_incremental.adb Delete_Comment_Start.
                        is begin
                           for Tok of Floating_Non_Grammar loop
                              --  Tok is not shifted
                              if Contains (Deleted_Region, Tok.Byte_Region.First) and
                                Tree.Lexer.Is_Comment (Tok) and
                                Contains (Tok.Byte_Region, Token.Byte_Region.First - (Shift_Bytes - KMN.Deleted_Bytes))
                              then
                                 return True;
                              end if;
                           end loop;
                           return False;
                        end Deleted_Comment_Start;
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
                                 ) or Deleted_Comment_Start);

                        Scanned_Byte_Pos := Token.Byte_Region.Last;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal then
                           --  grammar token
                           Ref := Tree.Insert_Source_Terminal (Stream, Token, Before => Terminal.Element);

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

                        if Error then
                           Parser.Wrapped_Lexer_Errors.Append
                             ((Recover_Token_Ref =>
                                 (if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal
                                  then Ref
                                  else Invalid_Stream_Node_Ref),
                               Error             => Parser.Tree.Lexer.Errors (Parser.Tree.Lexer.Errors.Last)));
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

               --  Delete tokens that were deleted or modified, and Virtual_Terminals
               --  that are adjacent to the edit region. Deleted non_grammar
               --  New_Lines decrement Shift_Lines. Check for deleted comment end
               --  (deleted comment start handled in Scan_Changed_Loop).
               --
               --  Need delete even if not Do_Scan, to handle KMN.Deleted_Bytes > 0;
               --  test_incremental.adb Edit_Code_4, ada_skel.adb ada-skel-return

               --  First delete trailing virtual_terminals in preceding stream
               --  elements, if there is an actual edit in this KMN.
               if KMN.Inserted_Bytes + KMN.Deleted_Bytes > 0 then
                  Delete_Trailing :
                  declare
                     Ref : Stream_Node_Ref := Tree.Stream_Prev (Terminal);

                     procedure Delete_Empty
                     is
                        To_Delete : Stream_Index := Ref.Element;
                     begin
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete empty nonterm " &
                                Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
                        end if;

                        Tree.Stream_Prev (Ref, Rooted => True);
                        Tree.Stream_Delete (Ref.Stream, To_Delete);
                     end Delete_Empty;

                     procedure Delete
                     is
                        Ref_Non_Grammar : Lexer.Token_Arrays.Vector renames Tree.Non_Grammar_Const (Ref.Node);
                        Prev_Non_Grammar : Stream_Node_Ref := Ref;

                        To_Delete : Stream_Index := Ref.Element;
                     begin
                        if Ref_Non_Grammar.Length > 0 then
                           loop
                              exit when Tree.Label (Prev_Non_Grammar.Node) = Source_Terminal;
                              Tree.Prev_Terminal (Prev_Non_Grammar);
                           end loop;
                           Tree.Non_Grammar_Var (Prev_Non_Grammar.Node).Append (Ref_Non_Grammar);
                        end if;

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete virtual " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
                        end if;

                        Tree.Stream_Prev (Ref, Rooted => True);
                        Tree.Stream_Delete (Ref.Stream, To_Delete);
                     end Delete;

                  begin
                     loop
                        exit when Ref.Node = Tree.SOI;

                        case Tree.Label (Ref.Node) is
                        when Source_Terminal =>
                           exit;

                        when Virtual_Terminal | Virtual_Identifier =>
                           Delete;

                        when Nonterm =>
                           declare
                              First : constant Node_Access := Tree.First_Terminal (Ref.Node);
                              Last  : constant Node_Access := Tree.Last_Terminal (Ref.Node);
                           begin
                              if First = Invalid_Node_Access then
                                 Delete_Empty;
                              else
                                 if Tree.Label (First) in Virtual_Terminal | Virtual_Identifier then
                                    Ref.Node := First;
                                    Breakdown (Ref, To_Single => True);
                                    --  Modifies Ref, so we can't also check Last here.

                                 elsif Tree.Label (Last) in Virtual_Terminal | Virtual_Identifier then
                                    declare
                                       Temp : Rooted_Ref := Ref;
                                    begin
                                       --  Leave Ref at end of affected nodes, since we may uncover more that
                                       --  need deleting.
                                       Ref := Tree.Stream_Next (Ref);
                                       if Trace_Incremental_Parse > Detail then
                                          Parser.Trace.Put_Line
                                            ("right breakdown " & Tree.Image
                                               (Tree.Get_Node (Ref.Stream, Ref.Element), Node_Numbers => True) &
                                               " target " & Tree.Image (Last, Node_Numbers => True));
                                       end if;
                                       Tree.Right_Breakdown (Temp);
                                       Tree.Stream_Prev (Ref, Rooted => True);
                                    end;

                                 else
                                    exit;
                                 end if;
                              end if;
                           end;
                        end case;
                     end loop;
                  end Delete_Trailing;
               end if;

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
                     Tree.Next_Terminal (Terminal);
                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("delete deleted or modified " &
                             Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => True));
                     end if;

                     if Comment_End_Deleted and then
                       Tree.Byte_Region (To_Delete, Trailing_Non_Grammar => True).Last >= New_Comment_End
                     then
                        Comment_End_Deleted := False;
                        New_Comment_End     := Invalid_Buffer_Pos;
                     end if;

                     --  If Terminal is Virtual, it is tempting to just move non_grammar to
                     --  previous token. But there may be intervening deleted tokens;
                     --  ada_mode-recover_33.adb.
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
                  --  If Target_Bytes < Terminal.byte_region.first: If After
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
                        --  Shift_Lines; we are inserting it before those.
                        --  ada_mode-recover_33.adb, ada_mode-recover_extra_end_loop.adb
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
                  for I in Floating_Non_Grammar.First_Index .. Floating_Non_Grammar.Last_Index loop
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
   end Edit_Tree;

   procedure Put_Error
     (Error : in Syntax_Trees.Error_Ref;
      Tree  : in Syntax_Trees.Tree)
   is
      use Ada.Text_IO; --  FIXME: use Trace?
   begin
      Put_Line (Tree.Error (Tree.Error_Node (Error)).Image (Tree, Tree.Error_Node (Error)));
   end Put_Error;

   procedure Put (Errors : in Wrapped_Lexer_Error_Lists.List; Tree : in Syntax_Trees.Tree)
   is begin
      for Item of Errors loop
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error,
            Tree.Lexer.File_Name & ":0:0: lexer unrecognized character at" & Buffer_Pos'Image
              (Item.Error.Char_Pos));
      end loop;
   end Put;

   procedure Put_Errors (Parser : in Base_Parser'Class)
   is
      use WisiToken.Syntax_Trees;
   begin
      Put (Parser.Wrapped_Lexer_Errors, Parser.Tree);

      for Error_Cur in Parser.Tree.Error_Iterate loop
         Ada.Text_IO.Put_Line (Parser.Tree.Image (Element (Error_Cur)));
      end loop;
   end Put_Errors;

end WisiToken.Parse;
