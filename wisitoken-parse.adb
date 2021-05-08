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
      use KMN_Lists;
      use WisiToken.Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;

      Tree : Syntax_Trees.Tree renames Parser.Tree;

      KMN_Node         : Cursor                := Edits.First;
      Old_Byte_Pos     : Base_Buffer_Pos       := 0;
      Old_Char_Pos     : Base_Buffer_Pos       := 0;
      New_Byte_Pos     : Base_Buffer_Pos       := 0;
      New_Char_Pos     : Base_Buffer_Pos       := 0;
      Scanned_Byte_Pos : Base_Buffer_Pos       := 0; -- Last pos scanned by lexer
      Shift_Bytes      : Base_Buffer_Pos       := 0;
      Shift_Chars      : Base_Buffer_Pos       := 0;
      Shift_Lines      : Base_Line_Number_Type := 0;

      Floating_Non_Grammar : Lexer.Token_Arrays.Vector;
      --  Non_grammar that are detached from a shifted node because they are
      --  outside the stable region (they were moved to that node by error
      --  recover, not necessarily from a deleted node). These are _not_
      --  shifted, because the correct shift is unknown at the time they are
      --  detached. test case ada_mode-recover_42.adb

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;
   begin
      if Trace_Incremental_Parse > Detail then
         --  We can't get line_numbers in a node image after Start_Edit (until
         --  parse is done, and we have parents set again). So output a tree
         --  with line numbers now.
         Parser.Trace.Put_Line (Tree.Image (Line_Numbers => True, Non_Grammar => True));
         Parser.Trace.Put ("deleted_nodes: ");
         for Node of Parser.Deleted_Nodes loop
            Parser.Trace.Put (Tree.Image (Node, Node_Numbers => True, Non_Grammar => True), Prefix => False);
         end loop;
         Parser.Trace.New_Line;
      end if;

      Tree.Start_Edit;

      Stream := Tree.Shared_Stream;

      --  It is tempting to restore all Parser.Deleted_Nodes to Stream, so
      --  they will be considered in the processing below. However, that
      --  would force redoing error recover that is not caused by an
      --  incremental edit, so it is not incremental. We restore
      --  Parser.Deleted_Nodes that are in Inserted_Region below.

      Terminal := Tree.First_Terminal (Tree.Stream_First (Stream));

      if Edits.Length = 0 then
         return;
      end if;

      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Element (KMN_Node);

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
         begin
            --  Parser.Lexer contains the edited text, so we can't check that
            --  stable, deleted are inside the initial text. Caller should use
            --  Validate_KMN.

            if not Contains (Outer => Parser.Tree.Lexer.Buffer_Region_Byte, Inner => Inserted_Region) then
               raise User_Error with "KMN insert region outside edited source text";
            end if;

            if Trace_Incremental_Parse > Outline then
               Parser.Trace.New_Line;
               Parser.Trace.Put_Line
                 ("KMN: " & Image (Stable_Region) & Image (Inserted_Region) & Image (Deleted_Region));
               Parser.Trace.Put_Line ("old  :" & Old_Byte_Pos'Image & Old_Char_Pos'Image);
               Parser.Trace.Put_Line
                 ("shift: " & Shift_Bytes'Image & " " & Shift_Chars'Image & " " & Shift_Lines'Image);
               Parser.Trace.Put_Line ("scanned_byte_pos:" & Scanned_Byte_Pos'Image);
               Parser.Trace.Put_Line ("terminal:" & Tree.Image (Terminal, Non_Grammar => True, Augmented => True));

               Parser.Trace.Put_Line
                 ("stream:" & Tree.Image
                    (Stream,
                     Children    => Trace_Incremental_Parse > Detail,
                     Non_Grammar => True,
                     Augmented   => True));

               if Floating_Non_Grammar.Length > 0 then
                  Parser.Trace.Put_Line
                    ("floating_non_grammar: " & Lexer.Image
                       (Floating_Non_Grammar, Tree.Lexer.Descriptor.all));
               end if;

               if WisiToken.Trace_Incremental_Parse > Detail then
                  Parser.Trace.New_Line;
               end if;
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

               --  It is tempting to skip Unchanged_Loop if Shift_Bytes = 0 and
               --  Shift_Chars = 0 and Shift_Lines = 0. But we need to scan all
               --  Non_Grammar for Floating_Non_Grammar, which changes Shift_Lines.

            else
               Unchanged_Loop :
               loop
                  exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
                  exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  exit Unchanged_Loop when not Contains
                    --  Virtual_Terminals have null byte_region so they are contained in
                    --  any region.
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
                    (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines, Stable_Region.Last, Floating_Non_Grammar);

                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line
                       ("  => " & Tree.Image
                          (Terminal.Node, Non_Grammar => True, Terminal_Node_Numbers => True, Augmented => True));
                  end if;

                  Tree.Next_Terminal (Terminal);
               end loop Unchanged_Loop;
            end if;

            declare
               Cur : Valid_Node_Access_Lists.Cursor := Parser.Deleted_Nodes.First;
            begin
               loop
                  exit when not Valid_Node_Access_Lists.Has_Element (Cur);

                  declare
                     Deleted_Node : constant Valid_Node_Access := Parser.Deleted_Nodes (Cur);
                  begin
                     if Contains (Deleted_Region, Tree.Byte_Region (Deleted_Node)) then
                        declare
                           To_Delete : Valid_Node_Access_Lists.Cursor := Cur;
                        begin
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("delete deleted_node " & Tree.Image (Deleted_Node));
                           end if;
                           Valid_Node_Access_Lists.Next (Cur);
                           Parser.Deleted_Nodes.Delete (To_Delete);
                        end;

                     elsif Contains (Stable_Region, Tree.Byte_Region (Deleted_Node)) then
                        pragma Assert (Tree.Non_Grammar_Const (Deleted_Node).Length = 0);
                        Tree.Shift
                          (Deleted_Node, Shift_Bytes, Shift_Chars, Shift_Lines, Buffer_Pos'Last, Floating_Non_Grammar);
                        Valid_Node_Access_Lists.Next (Cur);
                     else
                        Valid_Node_Access_Lists.Next (Cur);
                     end if;
                  end;
               end loop;
            end;

            --  Scanning is needed if Inserted_Region is not empty and has not
            --  already been scanned, or if the deleted region overlaps or is
            --  adjacent to a preceding or following grammar or non-grammar token.
            --  Set Lex_Start_* to the scan start position; the start of Terminal
            --  or in the preceding non_grammar or whitespace.
            --
            --  If two edit regions affect the same token, scanning the first will
            --  also scan the second.
            declare
               Terminal_Byte_Region : constant Buffer_Region := Tree.Byte_Region (Terminal.Node);

               --  We don't modify Shift_Lines until after we use it to set Lex_Start_Line
               Delta_Lines : Base_Line_Number_Type := 0;

               Do_Scan        : Boolean := False;
               Lex_Start_Byte : Buffer_Pos;
               Lex_Start_Char : Buffer_Pos;
               Lex_Start_Line : Line_Number_Type;

               Last_Grammar : Terminal_Ref := Tree.Prev_Terminal (Terminal); -- Shifted
            begin
               if Terminal_Byte_Region.Last + Shift_Bytes > Scanned_Byte_Pos and
                 Inserted_Region.Last > Scanned_Byte_Pos
               then
                  --  else Terminal and Inserted_Region was scanned by previous KMN
                  --  test case: ada_mode-recover_align_1.adb

                  --  Unchanged_Loop exited because Terminal is at least partly out of
                  --  Stable_Region or adjacent to the edit start, or because it reached
                  --  EOI. Therefore the edit start is in Terminal, or in a non-grammar
                  --  token, or in whitespace before Terminal. Terminal is not shifted.

                  if Terminal_Byte_Region.First + Shift_Bytes <= Inserted_Region.First then
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
                     --  decrement Shift_Lines, but only after we use Shift_Lines to set
                     --  Lex_Start_Line.
                     declare
                        procedure Handle_Non_Grammar (Non_Grammar : in out WisiToken.Lexer.Token_Arrays.Vector)
                        is
                           Delete : SAL.Base_Peek_Type := 0;
                        begin
                           if Non_Grammar.Length = 0 then
                              --  Edit start is in whitespace before Terminal
                              Lex_Start_Byte := Inserted_Region.First;
                              Lex_Start_Char := Inserted_Region_Chars.First;
                              Lex_Start_Line := Tree.Line_Region (Last_Grammar).First;
                              --  start_line test case ada_mode-recover_incremental_01.adb
                              Do_Scan        := True;
                           else
                              --  Non_Grammar is shifted.
                              for I in Non_Grammar.First_Index .. Non_Grammar.Last_Index loop
                                 if Non_Grammar (I).Byte_Region.Last + 1 >= Inserted_Region.First and
                                   Non_Grammar (I).Byte_Region.Last > Scanned_Byte_Pos
                                   --  test case: ada_mode-recover_align_1.adb
                                 then
                                    Delete  := I;
                                    Do_Scan := True;
                                    exit;
                                 end if;
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
                                 --  Since Non_Grammar (Delete) is before Terminal, it has already been
                                 --  shifted, so we don't add Shift_* here.

                                 declare
                                    Token : WisiToken.Lexer.Token renames Non_Grammar (Delete);
                                 begin
                                    Lex_Start_Byte := Buffer_Pos'Min (Token.Byte_Region.First, Inserted_Region.First);
                                    Lex_Start_Char := Buffer_Pos'Min
                                      (Token.Char_Region.First, Inserted_Region_Chars.First);
                                    Lex_Start_Line := Token.Line_Region.First;
                                 end;

                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("delete non_grammar" & Tree.Get_Node_Index (Last_Grammar.Node)'Image &
                                         Delete'Image & " .." & Non_Grammar.Last_Index'Image);
                                 end if;
                                 for I in Delete .. Non_Grammar.Last_Index loop
                                    if Non_Grammar (I).ID = Tree.Lexer.Descriptor.New_Line_ID then
                                       Delta_Lines := @ - 1;
                                    end if;
                                 end loop;

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
                           Handle_Non_Grammar (Tree.Non_Grammar_Var (Last_Grammar.Node));
                        end if;
                     end;
                  end if;
               end if;

               if Do_Scan then
                  Shift_Lines := @ + Delta_Lines;

                  if Trace_Incremental_Parse > Outline then
                     Parser.Trace.Put_Line
                       ("lexer.set_position" & Lex_Start_Byte'Image & Lex_Start_Char'Image & Lex_Start_Line'Image);
                  end if;

                  Parser.Tree.Lexer.Set_Position
                    (Byte_Position => Lex_Start_Byte,
                     Char_Position => Lex_Start_Char,
                     Line          => Lex_Start_Line);

                  --  Ensure Terminal.Node is first in Terminal.Element, so we can insert before it.
                  if Tree.Label (Terminal.Element) = Nonterm then
                     if Trace_Incremental_Parse > Detail then
                        Parser.Trace.Put_Line
                          ("breakdown " & Tree.Image
                             (Tree.Get_Node (Terminal.Stream, Terminal.Element), Node_Numbers => True) &
                             " target " & Tree.Image (Terminal.Node, Node_Numbers => True));
                     end if;
                     Tree.Breakdown (Terminal);
                     if Trace_Incremental_Parse > Extra then
                        Parser.Trace.Put_Line
                          ("... result " & Tree.Image (Stream, Non_Grammar => False, Augmented => False));
                     end if;
                  end if;

                  --  Delete trailing Virtual_Terminals that are immediately before the edit
                  --  region. Virtual_Terminals immediately after are deleted in
                  --  Delete_Loop below.
                  declare
                     I         : Stream_Node_Ref := Tree.Stream_Prev (Terminal);
                     To_Delete : Stream_Node_Ref;

                     procedure Delete_Empty
                     with Pre => Tree.Is_Empty_Nonterm (Tree.Get_Node (To_Delete.Stream, To_Delete.Element))
                     is begin
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line ("delete " & Tree.Image (To_Delete));
                        end if;

                        Tree.Stream_Delete (I.Stream, To_Delete.Element);
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
                           for Token of Non_Grammar loop
                              if Token.Byte_Region.First >= Lex_Start_Byte then
                                 --  This token will be scanned, so we don't copy it here (equivalent
                                 --  to deleting it). ada_mode-interactive_1.adb Proc_2, Func_2
                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("... delete " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                                 end if;
                                 if Token.ID = Tree.Lexer.Descriptor.New_Line_ID then
                                    Shift_Lines := @ - 1;
                                 end if;
                              else
                                 Tree.Non_Grammar_Var (Prev_Terminal).Append (Token);
                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("... move " & Lexer.Image (Token, Tree.Lexer.Descriptor.all) & " to " &
                                         Tree.Image (Prev_Terminal));
                                 end if;
                              end if;
                           end loop;
                        end if;

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete " & Tree.Image (To_Delete.Node, Terminal_Node_Numbers => True));
                        end if;

                        Tree.Stream_Delete (I.Stream, To_Delete.Element);
                     end Delete;

                  begin
                     Delete_Virtuals_Loop :
                     loop
                        exit Delete_Virtuals_Loop when I.Node = Invalid_Node_Access;

                        case Tree.Label (I.Element) is
                        when Source_Terminal =>
                           exit Delete_Virtuals_Loop;

                        when Nonterm =>
                           --  Check if trailing terminals are virtual
                           declare
                              Last_Term : constant Node_Access := Tree.Last_Terminal
                                (Tree.Get_Node (I.Stream, I.Element));
                           begin
                              if Last_Term = Invalid_Node_Access then
                                 --  empty nonterm
                                 To_Delete := I;
                                 Tree.Stream_Prev (I);
                                 Delete_Empty;

                              elsif Tree.Label (Last_Term) = Source_Terminal then
                                 exit Delete_Virtuals_Loop;

                              else
                                 --  Delete trailing terminal
                                 declare
                                    Temp : Terminal_Ref := (I.Stream, I.Element, Last_Term);
                                 begin
                                    Tree.Right_Breakdown (Temp);
                                    To_Delete := Temp;
                                    I := Tree.Stream_Prev (Temp);
                                    Delete;
                                 end;
                              end if;
                           end;

                        when Virtual_Terminal =>
                           To_Delete := I;
                           Tree.Stream_Prev (I);
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
                        Token : Lexer.Token;
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
                                    --  Token starts immediately after deleted region; it may
                                    --  have been truncated (test_incremental.adb Edit_Code_4 'Cc')
                                 ));

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
                           if Token.ID = Parser.Tree.Lexer.Descriptor.New_Line_ID then
                              Shift_Lines := @ + 1;
                           end if;
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

               if Do_Scan then
                  Delete_Loop :
                  --  Delete tokens that were deleted or modified, and leading
                  --  Virtual_Terminals. Deleted non_grammar New_Lines decrement
                  --  Shift_Lines.
                  loop
                     exit Delete_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

                     exit Delete_Loop when
                       Tree.Label (Terminal.Node) in Syntax_Trees.Source_Terminal | Syntax_Trees.Nonterm and then
                       not
                       ((KMN.Deleted_Bytes > 0 and
                           Tree.Byte_Region (Terminal.Node).First <= Deleted_Region.Last + 1)  -- deleted or modified
                          or
                          (KMN.Inserted_Bytes > 0 and
                             Tree.Byte_Region (Terminal.Node).First <= Stable_Region.Last + 1) --  modified
                          or
                          Tree.Byte_Region (Terminal.Node).First + Shift_Bytes <= Scanned_Byte_Pos);

                     --  Ensure Terminal is Single, so we can delete it.
                     if Tree.Label (Terminal.Element) = Nonterm then
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("breakdown single " & Tree.Image
                                (Tree.Get_Node (Terminal.Stream, Terminal.Element), Node_Numbers => True) &
                                " target " & Tree.Image (Terminal.Node, Node_Numbers => True));
                        end if;
                        Tree.Breakdown (Terminal);
                        if Tree.Label (Terminal.Element) = Nonterm then
                           Tree.Left_Breakdown (Terminal);
                        end if;
                        if Trace_Incremental_Parse > Extra then
                           Parser.Trace.Put_Line
                             ("... result " & Tree.Image (Stream));
                        end if;
                     end if;

                     declare
                        To_Delete : Stream_Node_Ref := Terminal;
                     begin
                        Tree.Next_Terminal (Terminal);
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete " &
                                Tree.Image (To_Delete.Element, Terminal_Node_Numbers => True, Non_Grammar => True));
                        end if;

                        for Token of Tree.Non_Grammar_Const (To_Delete.Node) loop
                           if Token.ID = Tree.Lexer.Descriptor.New_Line_ID then
                              Shift_Lines := @ - 1;
                           end if;

                           if Token.Byte_Region.Last + Shift_Bytes <= Scanned_Byte_Pos then
                              --  Token was scanned. FIXME: if Scanned_Byte_Pos covers future KMN,
                              --  Shift_Bytes is inaccurate here. need test case; ada_mode.ads?
                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("delete non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                              end if;
                           else
                              --  Token was on a node deleted by error recover, then moved to
                              --  Node.
                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("float non_grammar " & Lexer.Image (Token, Tree.Lexer.Descriptor.all));
                              end if;
                              Floating_Non_Grammar.Append (Token);
                           end if;
                        end loop;

                        pragma Assert (To_Delete.Node /= Tree.SOI and To_Delete.Node /= Tree.EOI);
                        Tree.Stream_Delete (Stream, To_Delete.Element);
                     end;
                  end loop Delete_Loop;
               end if;

               --  See if any of Wrapped_Lexer_Errors, Deleted_Nodes or Floating_Non_Grammar can be handled
               --  here.
               declare
                  Check_Region : constant Buffer_Region :=
                    (Lex_Start_Byte - Shift_Bytes, Tree.Byte_Region (Terminal.Node).First - 1);

                  Error_Cur     : Wrapped_Lexer_Error_Lists.Cursor := Parser.Wrapped_Lexer_Errors.First;
                  Inc_Error_Cur : Boolean                          := True;

                  Deleted_Node       : Valid_Node_Access_Lists.Cursor := Parser.Deleted_Nodes.First;
                  Inc_Deleted_Node   : Boolean                        := True;
                  Deleted_Byte_First : Buffer_Pos;

                  Last_Deleted_Non_Grammar : SAL.Base_Peek_Type := Lexer.Token_Arrays.No_Index;
                  Last_Copied_Non_Grammar  : SAL.Base_Peek_Type := Lexer.Token_Arrays.No_Index;

                  procedure Delete
                  is
                     To_Delete : Valid_Node_Access_Lists.Cursor := Deleted_Node;
                  begin
                     Deleted_Node := Valid_Node_Access_Lists.Next (Deleted_Node);
                     Parser.Deleted_Nodes.Delete (To_Delete);
                  end Delete;

                  function Find_Element
                    (Target_Bytes : in Buffer_Pos;
                     After        : in Boolean)
                    return Stream_Index
                  --  If After is True, return stream element that is after or contains
                  --  Target_Bytes, where prev element is before Target_Bytes. Else
                  --  return element that is before Target_Bytes, where next is after.
                  is
                     Before      : Stream_Index := Terminal.Element;
                     Next_Before : Stream_Index := Tree.Stream_Prev (Stream, Before);
                  begin
                     loop
                        loop
                           exit when not Tree.Is_Empty_Or_Virtual_Nonterm
                             (Tree.Get_Node (Stream, Next_Before));
                           --  Empty test case: ada_mode-recover_repair_2.adb
                           --  All virtual case: ada_mode-recover_42.adb

                           Next_Before := Tree.Stream_Prev (Stream, @);
                        end loop;

                        exit when Target_Bytes in Tree.Byte_Region (Next_Before).Last + 1 ..
                            (if After
                             then Tree.Byte_Region (Before).Last
                             else Tree.Byte_Region (Before).First - 1);
                        --  region bounds test case: ada_mode-recover_partial_14.adb
                        --  contains; ada_mode-recover_42.adb lexer_error string_literal

                        Before := Next_Before;
                        Next_Before := Tree.Stream_Prev (Stream, Next_Before);
                     end loop;
                     return (if After then Before else Next_Before);
                  end Find_Element;

               begin
                  Lexer_Errors :
                  loop
                     exit Lexer_Errors when not Wrapped_Lexer_Error_Lists.Has_Element (Error_Cur);
                     Inc_Error_Cur := True;

                     if Parser.Wrapped_Lexer_Errors (Error_Cur).Recover_Token_Ref.Element = Invalid_Stream_Index and
                       --  Not yet restored
                       Parser.Wrapped_Lexer_Errors (Error_Cur).Recover_Token_Ref.Node /= Invalid_Node_Access
                       --  A grammar token
                     then
                        declare
                           Error_Node : constant Valid_Node_Access := Parser.Wrapped_Lexer_Errors
                             (Error_Cur).Recover_Token_Ref.Node;
                           Error_Byte_Region : constant Buffer_Region := Tree.Byte_Region
                             (Error_Node); -- not shifted

                           To_Delete : Wrapped_Lexer_Error_Lists.Cursor := Error_Cur;
                        begin
                           if Contains (Check_Region, Error_Byte_Region.First) then
                              if Error_Byte_Region.First + Shift_Bytes < Scanned_Byte_Pos then
                                 --  A new error has been created if the lexer error is not fixed.
                                 Error_Cur := Wrapped_Lexer_Error_Lists.Next (Error_Cur);
                                 Inc_Error_Cur := False;
                                 Parser.Wrapped_Lexer_Errors.Delete (To_Delete);

                              else
                                 declare
                                    Element : constant Stream_Index := Find_Element
                                      (Error_Byte_Region.Last, After => True);

                                    function Equals_Node
                                      (Tree : in Syntax_Trees.Tree; Node : in Valid_Node_Access)
                                      return Boolean
                                    is
                                       pragma Unreferenced (Tree);
                                    begin
                                       return Node = Error_Node;
                                    end Equals_Node;

                                    Element_Node : constant Node_Access := Tree.Find_Descendant
                                      (Tree.Get_Node (Stream, Element), Equals_Node'Access);
                                 begin
                                    if Element_Node = Error_Node then
                                       --  Error_Node was not deleted from Tree. It was shifted above.

                                       Tree.Clear_Parent (Error_Node, Clear_Children => False);

                                       Parser.Wrapped_Lexer_Errors (Error_Cur).Recover_Token_Ref :=
                                         (Stream, Element, Error_Node);

                                       if Trace_Incremental_Parse > Detail then
                                          Parser.Trace.Put_Line
                                            ("set lexer_error element " & Tree.Image
                                               (Error_Node, Node_Numbers => True, Non_Grammar => True));
                                       end if;
                                    else
                                       --  Error_Node was deleted from Tree. It also appears in
                                       --  Deleted_Nodes. Need test case.
                                       raise SAL.Not_Implemented;
                                    end if;
                                 end;
                              end if;
                           end if;
                        end;
                     end if;
                     if Inc_Error_Cur then
                        Error_Cur := Wrapped_Lexer_Error_Lists.Next (Error_Cur);
                     end if;
                  end loop Lexer_Errors;

                  Deleted_Nodes :
                  loop
                     exit Deleted_Nodes when not Valid_Node_Access_Lists.Has_Element (Deleted_Node);
                     Inc_Deleted_Node := True;

                     Deleted_Byte_First := Tree.Byte_Region (Parser.Deleted_Nodes (Deleted_Node)).First;

                     if Contains (Check_Region, Deleted_Byte_First) then
                        if Deleted_Byte_First + Shift_Bytes <= Scanned_Byte_Pos then
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("delete deleted_node " & Tree.Image
                                   (Parser.Deleted_Nodes (Deleted_Node), Node_Numbers => True));
                           end if;
                           Inc_Deleted_Node := False;
                           Delete;

                        else
                           --  Find element to insert before
                           declare
                              Before : constant Stream_Index := Find_Element (Deleted_Byte_First, After => True);
                           begin
                              Tree.Shift
                                (Parser.Deleted_Nodes (Deleted_Node),
                                 Shift_Bytes, Shift_Chars, Shift_Lines, Buffer_Pos'Last, Floating_Non_Grammar);

                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("shift and restore deleted_node " & Tree.Image
                                      (Parser.Deleted_Nodes (Deleted_Node), Node_Numbers => True));
                              end if;
                              Tree.Stream_Insert (Stream, Parser.Deleted_Nodes (Deleted_Node), Before => Before);
                              Inc_Deleted_Node := False;
                              Delete;
                           end;
                        end if;
                     end if;
                     if Inc_Deleted_Node then
                        Deleted_Node := Valid_Node_Access_Lists.Next (Deleted_Node);
                     end if;
                  end loop Deleted_Nodes;

                  for I in Floating_Non_Grammar.First_Index .. Floating_Non_Grammar.Last_Index loop
                     if Floating_Non_Grammar (I).Byte_Region.First + Shift_Bytes <= Scanned_Byte_Pos then
                        --  test case: ada_mode-recover_align_1.adb
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("delete floating_non_grammar " & Lexer.Image
                                (Floating_Non_Grammar (I), Tree.Lexer.Descriptor.all));
                        end if;
                        Last_Deleted_Non_Grammar := I;

                     elsif Contains (Check_Region, Floating_Non_Grammar (I).Byte_Region.First) then
                        --  Find element to append non_grammar to
                        declare
                           Element : constant Stream_Index := Find_Element
                             (Floating_Non_Grammar (I).Byte_Region.First + Shift_Bytes,
                              After => False);

                           Tok : Lexer.Token := Floating_Non_Grammar (I);
                        begin
                           if Tok.ID = Tree.Lexer.Descriptor.New_Line_ID then
                              Shift_Lines := @ + 1;
                           end if;
                           Tok.Byte_Region := @ + Shift_Bytes;
                           Tok.Char_Region := @ + Shift_Chars;
                           Tok.Line_Region := @ + Shift_Lines;

                           Tree.Non_Grammar_Var (Tree.Get_Node (Stream, Element)).Append (Tok);

                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("floating non_grammar " & Lexer.Image
                                   (Floating_Non_Grammar (I), Tree.Lexer.Descriptor.all) & " to " & Tree.Image
                                     (Tree.Get_Node (Stream, Element), Node_Numbers => True, Non_Grammar => True));
                           end if;
                           Last_Copied_Non_Grammar := I;
                        end;
                     else
                        exit;
                     end if;
                  end loop;

                  if Last_Deleted_Non_Grammar /= Lexer.Token_Arrays.No_Index then
                     if Last_Deleted_Non_Grammar = Floating_Non_Grammar.Last_Index then
                        Floating_Non_Grammar.Clear;
                     else
                        Floating_Non_Grammar.Set_First_Last
                          (Last_Deleted_Non_Grammar + 1, Floating_Non_Grammar.Last_Index);
                     end if;
                  end if;

                  if Last_Copied_Non_Grammar /= Lexer.Token_Arrays.No_Index then
                     if Last_Copied_Non_Grammar = Floating_Non_Grammar.Last_Index then
                        Floating_Non_Grammar.Clear;
                     else
                        Floating_Non_Grammar.Set_First_Last
                          (Last_Copied_Non_Grammar + 1, Floating_Non_Grammar.Last_Index);
                     end if;
                  end if;
               end;
            end;

            KMN_Node := Next (KMN_Node);

            if not Has_Element (KMN_Node) then
               --  Finally shift EOI.
               pragma Assert
                 (Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID and
                    Tree.Non_Grammar_Const (Terminal.Node).Length = 1); -- EOI_ID

               Tree.Shift
                 (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Lines, Buffer_Pos'Last, Floating_Non_Grammar);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line
                    ("final shift " & Tree.Image (Terminal.Node, Terminal_Node_Numbers => True));
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

      --  We don't check for Deleted_Nodes or Wrapped_Lexer_Errors empty;
      --  those are not deleted if they are in a nonterm that is not broken
      --  down.

      --  Delete remnants of prevous error recovery that might be wrong.
      --  FIXME: insert remaining Deleted_Nodes here? need test case.
      declare
         Ref : Rooted_Ref := Tree.Stream_First (Stream);

         procedure Delete_Empty
         is
            To_Delete : Stream_Index := Ref.Element;
         begin
            if Trace_Incremental_Parse > Detail then
               Parser.Trace.Put_Line
                 ("delete " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
            end if;

            Tree.Stream_Next (Ref, Rooted => True);
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
                 ("delete " & Tree.Image (To_Delete, Node_Numbers => True, Non_Grammar => True));
            end if;

            Tree.Stream_Next (Ref, Rooted => True);
            Tree.Stream_Delete (Ref.Stream, To_Delete);
         end Delete;

      begin
         loop
            exit when Ref.Node = Tree.EOI;

            case Tree.Label (Ref.Node) is
            when Source_Terminal =>
               Tree.Stream_Next (Ref, Rooted => True);

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
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("left breakdown " & Tree.Image
                                (Tree.Get_Node (Ref.Stream, Ref.Element), Node_Numbers => True) &
                                " target " & Tree.Image (First, Node_Numbers => True));
                        end if;
                        Tree.Left_Breakdown (Ref);
                        --  Modifies Ref, so we can't also check Last here.

                     elsif Tree.Label (Last) in Virtual_Terminal | Virtual_Identifier then
                        declare
                           Temp : Rooted_Ref := Ref;
                        begin
                           --  Leave Ref before affected nodes, since we may uncover more that
                           --  need deleting.
                           Ref := Tree.Stream_Prev (Ref);
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("right breakdown " & Tree.Image
                                   (Tree.Get_Node (Ref.Stream, Ref.Element), Node_Numbers => True) &
                                   " target " & Tree.Image (Last, Node_Numbers => True));
                           end if;
                           Tree.Right_Breakdown (Temp);
                           Tree.Stream_Next (Ref, Rooted => True);
                        end;

                     else
                        Tree.Stream_Next (Ref, Rooted => True);
                     end if;
                  end if;
               end;
            end case;
         end loop;

         if Trace_Incremental_Parse > Detail then
            Parser.Trace.New_Line;
         end if;

      end;
   end Edit_Tree;

end WisiToken.Parse;
