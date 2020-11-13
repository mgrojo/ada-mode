--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2020 Free Software Foundation, Inc.
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
      Token  : in     Base_Token;
      Ref    : in     Syntax_Trees.Single_Terminal_Ref)
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Last_Grammar_Node := Ref.Node;

      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Parser.Last_Grammar_Node);
      end if;

      if Token.Line /= Invalid_Line_Number then
         --  Some lexers don't support line numbers.
         if Parser.Lexer.First then
            if Parser.Line_Begin_Token.Length = 0 then
               Parser.Line_Begin_Token.Set_First_Last (Token.Line, Token.Line);
            else
               Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line);
            end if;
            Parser.Line_Begin_Token (Token.Line) := Ref;

         elsif Token.ID = Parser.Descriptor.EOI_ID then
            Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line + 1);
            Parser.Line_Begin_Token (Token.Line + 1) := Ref;
         end if;
      end if;
   end Process_Grammar_Token;

   procedure Process_Non_Grammar_Token
     (Parser : in out Base_Parser'Class;
      Token  : in     Base_Token)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.User_Data_Access;
   begin
      if Token.ID = Parser.Descriptor.New_Line_ID then
         if Token.Line > Parser.Line_Begin_Char_Pos.Last_Index then
            Parser.Line_Begin_Char_Pos.Set_First_Last (Parser.Line_Begin_Char_Pos.First_Index, Token.Line);
         end if;

         Parser.Line_Begin_Char_Pos (Token.Line) := Parser.Lexer.Line_Start_Char_Pos;
      end if;

      if Parser.Last_Grammar_Node = Syntax_Trees.Invalid_Node_Access then
         Parser.Tree.Leading_Non_Grammar.Append (Token);
      else
         declare
            Containing : Base_Token_Array_Var_Ref renames Parser.Tree.Non_Grammar_Var (Parser.Last_Grammar_Node);
         begin
            Containing.Append (Token);
         end;
      end if;
      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Parser.Last_Grammar_Node);
      end if;
   end Process_Non_Grammar_Token;

   function Next_Grammar_Token (Parser : in out Base_Parser'Class) return Token_ID
   is
      use Syntax_Trees;

      Token : Base_Token;
      Error : Boolean;
      Ref   : Terminal_Ref;
   begin
      loop
         Error := Parser.Lexer.Find_Next (Token);

         if Trace_Lexer > Outline then
            Parser.Trace.Put_Line (Image (Token, Parser.Descriptor.all));
         end if;

         if Token.ID >= Parser.Descriptor.First_Terminal then
            Ref := Parser.Tree.Add_Terminal (Parser.Tree.Shared_Stream, Token);
            Process_Grammar_Token (Parser, Token, Ref);
         else
            Process_Non_Grammar_Token (Parser, Token);
            Ref := Invalid_Stream_Node_Ref;
         end if;

         if Error then
            Parser.Wrapped_Lexer_Errors.Append
              ((Recover_Token_Ref => Ref,
                Error             => Parser.Lexer.Errors (Parser.Lexer.Errors.Last)));
         end if;

         exit when Token.ID >= Parser.Descriptor.First_Terminal;
      end loop;

      return Token.ID;
   end Next_Grammar_Token;

   procedure Lex_All (Parser : in out Base_Parser'Class)
   is
      EOF_ID : constant Token_ID := Parser.Descriptor.EOI_ID;
   begin
      Parser.Lexer.Errors.Clear;
      Parser.Line_Begin_Token.Clear;
      Parser.Line_Begin_Char_Pos.Clear;
      Parser.Last_Grammar_Node := WisiToken.Syntax_Trees.Invalid_Node_Access;

      loop
         exit when EOF_ID = Next_Grammar_Token (Parser);
      end loop;
      if Trace_Parse > Outline then
         Parser.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Parser.Last_Grammar_Node)'Image & " tokens lexed");
      end if;

   end Lex_All;

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

      Deleted_Byte_Region : constant Buffer_Region :=
        (Stable_Byte_Region.Last + 1, Stable_Byte_Region.Last + KMN.Deleted_Bytes);
      Deleted_Char_Region : constant Buffer_Region :=
        (Stable_Char_Region.Last + 1, Stable_Char_Region.Last + KMN.Deleted_Chars);

      Inserted_Byte_Region : constant Buffer_Region :=
        (Edited_Stable_Byte_First + KMN.Stable_Bytes,
         Edited_Stable_Byte_First + KMN.Stable_Bytes + KMN.Inserted_Bytes - 1);
      Inserted_Char_Region : constant Buffer_Region :=
        (Edited_Stable_Char_First + KMN.Stable_Chars,
         Edited_Stable_Char_First + KMN.Stable_Chars + KMN.Inserted_Chars - 1);
   begin
      if not Contains (Outer => Initial_Text_Byte_Region, Inner => Stable_Byte_Region) then
         raise User_Error with "KMN stable byte region outside initial source text";
      end if;
      if not Contains (Outer => Initial_Text_Char_Region, Inner => Stable_Char_Region) then
         raise User_Error with "KMN stable char region outside initial source text";
      end if;

      if not Contains (Outer => Initial_Text_Byte_Region, Inner => Deleted_Byte_Region) then
         raise User_Error with "KMN deleted byte region outside initial source text";
      end if;
      if not Contains (Outer => Initial_Text_Char_Region, Inner => Deleted_Char_Region) then
         raise User_Error with "KMN deleted char region outside initial source text";
      end if;

      if not Contains (Outer => Edited_Text_Byte_Region, Inner => Inserted_Byte_Region) then
         raise User_Error with "KMN inserted byte region outside initial source text";
      end if;
      if not Contains (Outer => Edited_Text_Char_Region, Inner => Inserted_Char_Region) then
         raise User_Error with "KMN inserted char region outside edited source text";
      end if;
   end Validate_KMN;

   procedure Validate_KMN
     (List                     : in KMN_Lists.List;
      Stable_Byte_First        : in Buffer_Pos;
      Stable_Char_First        : in Buffer_Pos;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region)
   is
      Initial_Byte_Pos : Buffer_Pos := Stable_Byte_First;
      Initial_Char_Pos : Buffer_Pos := Stable_Char_First;
      Edited_Byte_Pos  : Buffer_Pos := Stable_Byte_First;
      Edited_Char_Pos  : Buffer_Pos := Stable_Char_First;
   begin
      for KMN of List loop
         Validate_KMN
           (KMN,
            Initial_Stable_Byte_First => Initial_Byte_Pos,
            Initial_Stable_Char_First => Initial_Char_Pos,
            Edited_Stable_Byte_First  => Edited_Byte_Pos,
            Edited_Stable_Char_First  => Edited_Char_Pos,
            Initial_Text_Byte_Region  => Initial_Text_Byte_Region,
            Initial_Text_Char_Region  => Initial_Text_Char_Region,
            Edited_Text_Byte_Region   => Edited_Text_Byte_Region,
            Edited_Text_Char_Region   => Edited_Text_Char_Region);

         Initial_Byte_Pos := @ + KMN.Stable_Bytes + KMN.Deleted_Bytes;
         Initial_Char_Pos := @ + KMN.Stable_Chars + KMN.Deleted_Chars;
         Edited_Byte_Pos  := @ + KMN.Stable_Bytes + KMN.Inserted_Bytes;
         Edited_Char_Pos  := @ + KMN.Stable_Chars + KMN.Inserted_Chars;
      end loop;
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
      Old_Line         : Line_Number_Type      := 1;
      New_Byte_Pos     : Base_Buffer_Pos       := 0;
      Scanned_Byte_Pos : Base_Buffer_Pos       := 0;
      Shift_Bytes      : Base_Buffer_Pos       := 0;
      Shift_Chars      : Base_Buffer_Pos       := 0;
      Shift_Line       : Base_Line_Number_Type := 0;

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;

      Next_Terminal_Index : Node_Index := 1;

      procedure Breakdown (Single : in Boolean := False)
      is
         Target : constant Valid_Node_Access := Terminal.Node;
      begin
         loop
            exit when Tree.First_Terminal (Stream, Terminal.Element).Node = Target and
              (not Single or Single_Terminal (Terminal));

            if Tree.Label (Terminal.Element) = Nonterm then
               Tree.Left_Breakdown (Terminal);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("left_breakdown stream: " & Tree.Image (Stream, Non_Grammar => True));
                  Parser.Trace.Put_Line ("terminal: " & Tree.Image (Terminal));
               end if;
            else
               Tree.Next_Terminal (Terminal);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("terminal: " & Tree.Image (Terminal));
               end if;
            end if;
         end loop;
         --  Leading virtual terminals are deleted in Delete_Loop below
      end Breakdown;

   begin
      Parser.Tree.Start_Edit;

      Stream := Tree.Shared_Stream;

      Terminal := Tree.First_Shared_Terminal (Stream, Tree.Stream_First (Stream));

      if Edits.Length = 0 then
         --  FIXME: update parser.line_begin_token (*).element
         return;
      end if;

      KMN_Loop :
      loop
         Parser.Last_Grammar_Node := Invalid_Node_Access;

         declare
            KMN : constant WisiToken.Parse.KMN := Element (KMN_Node);

            Stable_Region : constant Buffer_Region :=
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Deleted_Region : constant Buffer_Region :=
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);

            Do_Scan : Boolean := KMN.Inserted_Bytes > 0;

            Last_Stable_Line : Line_Number_Type      := Invalid_Line_Number;
            Line_Delta       : Base_Line_Number_Type := 0; -- counts new_lines inserted
         begin
            --  Parser.Lexer contains the edited text, so we can't check that
            --  stable, deleted are inside the initial text. Caller should use
            --  Validate_KMN.

            if not Contains (Outer => Parser.Lexer.Buffer_Region_Byte, Inner => Inserted_Region) then
               raise User_Error with "KMN insert region outside edited source text";
            end if;

            if Trace_Incremental_Parse > Outline then
               Parser.Trace.New_Line;
               Parser.Trace.Put_Line
                 ("KMN: " & Image (Stable_Region) & Image (Deleted_Region) & Image (Inserted_Region));
               Parser.Trace.Put_Line ("shift: " & Shift_Bytes'Image & " " & Shift_Chars'Image & " " & Shift_Line'Image);
               Parser.Trace.Put_Line ("terminal:" & Tree.Image (Terminal));

               if WisiToken.Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line (Tree.Image (Stream, Children => True, Non_Grammar => True));
                  Parser.Trace.New_Line;

               else
                  Parser.Trace.Put_Line (Tree.Image (Stream, Non_Grammar => True));
               end if;
            end if;

            Unchanged_Loop :
            loop
               exit Unchanged_Loop when Terminal = Invalid_Stream_Node_Ref;
               exit Unchanged_Loop when not Contains
                 --  FIXME: virtual_terminals have null byte_region, non-null char_region
                 (Inner          => Tree.Byte_Region (Terminal.Node),
                  Outer          => Stable_Region,
                  First_Boundary => Inclusive,
                  Last_Boundary  => (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                                     then Inclusive
                                     else Exclusive));

               exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Descriptor.EOI_ID;

               Tree.Shift (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Line);

               Parser.Last_Grammar_Node := Terminal.Node;
               --  for non_grammar, Shift_Line below

               Tree.Set_Terminal_Index (Terminal.Node, Next_Terminal_Index);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("stable shift " & Tree.Image (Terminal.Node, Terminal_Node_Numbers => True));
               end if;

               Next_Terminal_Index := @ + 1;

               Tree.Next_Terminal (Terminal);
            end loop Unchanged_Loop;

            --  FIXME: delete trailing virtual_terminals in stable_region

            declare
               --  Terminal is the terminal token overlapping or after the end of the
               --  stable region.
               Non_Grammar : Base_Token_Array_Const_Ref renames Tree.Non_Grammar_Const (Terminal.Node);
            begin
               if Non_Grammar.Length > 0 then
                  Last_Stable_Line := Non_Grammar (Non_Grammar.Last_Index).Line;
               else
                  Last_Stable_Line := Tree.Base_Token (Terminal.Node).Line;
               end if;
            end;

            --  Scanning is needed if Inserted_Region is not empty, or if the
            --  deleted region overlaps or is adjacent to a preceding or following
            --  token. If insert/delete is inside or after a comment, the scanning
            --  may already have been done. Set Do_Scan, Old_* to the scan start
            --  position; the start of token index or in the preceding
            --  non_grammar. FIXME: handle Tree.Leading_Non_Grammar.
            if Tree.Byte_Region (Terminal.Node).Last + Shift_Bytes > Scanned_Byte_Pos then
               if Tree.ID (Terminal.Node) /= Parser.Descriptor.EOI_ID and
                 ((Length (Inserted_Region) > 0 and then
                     Tree.Byte_Region (Terminal.Node).Last + Shift_Bytes >= Inserted_Region.First - 1)
                 or
                 (Length (Deleted_Region) > 0 and then
                    Tree.Byte_Region (Terminal.Node).Last >= Deleted_Region.First - 1))
               then
                  --  Terminal is a possibly modified token; it will be deleted
                  --  in Delete_Loop below.
                  Do_Scan := True;
                  declare
                     Token : constant Base_Token := Tree.Base_Token (Terminal.Node);
                  begin
                     Old_Byte_Pos := Token.Byte_Region.First;
                     Old_Char_Pos := Token.Char_Region.First;
                     Old_Line     := Token.Line;
                  end;

               else
                  if Parser.Last_Grammar_Node /= Invalid_Node_Access then
                     declare
                        Containing : Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Parser.Last_Grammar_Node);
                        Delete     : SAL.Base_Peek_Type := 0;
                     begin
                        for I in Containing.First_Index .. Containing.Last_Index loop
                           if Overlaps (Containing (I).Byte_Region + Shift_Bytes, Inserted_Region) or
                             Overlaps (Containing (I).Byte_Region, Deleted_Region)
                           then
                              Delete  := I;
                              Do_Scan := True;
                              exit;
                           end if;
                        end loop;
                        if Delete > 0 then
                           Old_Byte_Pos := Containing (Delete).Byte_Region.First;
                           Old_Char_Pos := Containing (Delete).Char_Region.First;
                           Old_Line := Containing (Delete).Line;
                           if Delete = Containing.First_Index then
                              Containing.Clear;
                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("delete non_grammar" & Tree.Get_Node_Index (Parser.Last_Grammar_Node)'Image);
                              end if;
                           else
                              Containing.Set_First_Last (Containing.First_Index, Delete - 1);

                              if Trace_Incremental_Parse > Detail then
                                 Parser.Trace.Put_Line
                                   ("delete non_grammar" & Tree.Get_Node_Index (Parser.Last_Grammar_Node)'Image &
                                      Delete'Image & " .." & Containing.Last_Index'Image);
                              end if;
                           end if;
                        elsif Containing.Length = 0 then
                           declare
                              Token : constant Base_Token := Tree.Base_Token (Parser.Last_Grammar_Node);
                           begin
                              Old_Byte_Pos := Token.Byte_Region.Last + 1;
                              Old_Char_Pos := Token.Char_Region.Last + 1;
                              Old_Line     := Token.Line;
                           end;
                        else
                           Old_Byte_Pos := Containing (Containing.Last_Index).Byte_Region.Last + 1;
                           Old_Char_Pos := Containing (Containing.Last_Index).Char_Region.Last + 1;
                           Old_Line     := Containing (Containing.Last_Index).Line;
                        end if;
                     end;
                  end if;
               end if;
            end if;

            if Do_Scan then
               --  Scan last token in unchanged + new text + following for new/changed tokens.

               declare
                  Line          : Line_Number_Type := Old_Line + Shift_Line;
                  Prev_Token_ID : Token_ID         := Invalid_Token_ID;
               begin
                  if Old_Line in Parser.Line_Begin_Char_Pos.First_Index .. Parser.Line_Begin_Char_Pos.Last_Index
                    and then Parser.Line_Begin_Char_Pos (Old_Line) = Old_Char_Pos
                  then
                     Line          := @  + 1;
                     Prev_Token_ID := Parser.Descriptor.New_Line_ID;
                  end if;

                  if Trace_Incremental_Parse > Outline then
                     Parser.Trace.Put_Line ("lexer.set_position" & Buffer_Pos'Image (Old_Byte_Pos + Shift_Bytes));
                  end if;

                  Parser.Lexer.Set_Position
                    (Byte_Position => Buffer_Pos'Max (Buffer_Pos'First, Old_Byte_Pos + Shift_Bytes),
                     Char_Position => Buffer_Pos'Max (Buffer_Pos'First, Old_Char_Pos + Shift_Chars),
                     Line          => Line,
                     Prev_Token_ID => Prev_Token_ID);
               end;

               --  Ensure Terminal.Node is first in Terminal.Element, so we can insert before it.
               Breakdown;

               Scan_Changed_Loop :
               loop
                  declare
                     Token : Base_Token;
                     Error : constant Boolean := Parser.Lexer.Find_Next (Token);
                     Ref   : Terminal_Ref;
                  begin
                     if Trace_Lexer > Outline then
                        Parser.Trace.Put_Line ("lex: " & Image (Token, Parser.Descriptor.all));
                     end if;

                     exit Scan_Changed_Loop when Token.ID = Parser.Descriptor.EOI_ID;
                     exit Scan_Changed_Loop when
                       Token.ID >= Parser.Descriptor.First_Terminal and then
                       not (Token.Byte_Region.First - Shift_Bytes <= Stable_Region.Last or
                              (KMN.Inserted_Bytes > 0 and then Token.Byte_Region.First <= Inserted_Region.Last) or
                              (KMN.Deleted_Bytes > 0 and then
                                 Token.Byte_Region.First - Shift_Bytes = Stable_Region.Last + 1));

                     Scanned_Byte_Pos := Token.Byte_Region.Last;

                     if Token.ID >= Parser.Descriptor.First_Terminal then
                        --  grammar token
                        Ref := Tree.Insert_Source_Terminal
                          (Stream, Token, Next_Terminal_Index, Before => Terminal.Element);

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line ("scan new " & Tree.Image (Ref));
                        end if;

                        Next_Terminal_Index := @ + 1;

                        Process_Grammar_Token (Parser, Token, Ref);

                     else
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("scan new " & Image (Token, Parser.Descriptor.all));
                        end if;

                        Process_Non_Grammar_Token (Parser, Token);
                        if Token.ID = Parser.Descriptor.New_Line_ID then
                           Line_Delta := @ + 1;
                        end if;
                     end if;

                     if Error then
                        Parser.Wrapped_Lexer_Errors.Append
                          ((Recover_Token_Ref => Ref,
                            Error             => Parser.Lexer.Errors (Parser.Lexer.Errors.Last)));
                     end if;
                  end;
               end loop Scan_Changed_Loop;
            end if;

            Delete_Loop :
            --  Delete tokens that were deleted or modified.
            loop
               exit Delete_Loop when Tree.ID (Terminal.Node) = Parser.Descriptor.EOI_ID;

               exit Delete_Loop when
                 Tree.Label (Terminal.Node) in Syntax_Trees.Source_Terminal | Syntax_Trees.Nonterm and
                 not
                 ((KMN.Deleted_Bytes > 0 and
                     Tree.Byte_Region (Terminal.Node).First <= Deleted_Region.Last + 1)  -- deleted or modified
                    or
                    (KMN.Inserted_Bytes > 0 and
                       Tree.Byte_Region (Terminal.Node).First <= Stable_Region.Last + 1)); --  modified

               --  Ensure Terminal.Node is Single, so we can delete it.
               Breakdown (Single => True);

               declare
                  Temp : Stream_Index := Terminal.Element;
               begin
                  Tree.Next_Shared_Terminal (Terminal);
                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line ("delete " & Tree.Image (Temp, Terminal_Node_Numbers => True));
                  end if;

                  Tree.Stream_Delete (Stream, Temp);
               end;
            end loop Delete_Loop;

            --  Update Shift_Line. Terminal is the token after any insert, delete;
            --  the first token in the next stable region; it has the old line number.
            declare
               Old_Line_Delta : constant Base_Line_Number_Type :=
                 Tree.Base_Token (Terminal.Node).Line - Last_Stable_Line;
            begin
               Shift_Line := Shift_Line + Line_Delta - Old_Line_Delta;
            end;

            Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
            Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

            Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
            New_Byte_Pos := Inserted_Region.Last;
            pragma Assert (New_Byte_Pos - Old_Byte_Pos = Shift_Bytes);

            KMN_Node := Next (KMN_Node);
            exit KMN_Loop when not Has_Element (KMN_Node);
         end;
      end loop KMN_Loop;

      declare
         Token : constant WisiToken.Base_Token := Tree.Base_Token (Terminal.Node);
      begin
         --  EOI is from wisitoken_accept if full parse, and/or at stream end
         --  for full and partial parse; shift both.

         if Token.ID = Parser.Descriptor.EOI_ID then
            Tree.Set_Terminal_Index (Terminal.Node, Next_Terminal_Index);
            Tree.Shift (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Line);

            if Trace_Incremental_Parse > Detail then
               Parser.Trace.Put_Line ("shift EOI " & Tree.Image (Terminal));
            end if;

            Tree.Next_Shared_Terminal (Terminal);

            if Terminal /= Invalid_Stream_Node_Ref then
               --  Not partial parse
               Tree.Set_Terminal_Index (Terminal.Node, Next_Terminal_Index);
               Tree.Shift (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Line);
            end if;
         else
            raise User_Error with "edit list does not cover entire parse region";
         end if;
      end;

      Tree.Update_Cache (Stream);
      --  We do this here (not in Incremental_Parse) so nonterm in input
      --  stream after error point has correct info in error recovery. Also
      --  renumber Node_Index in Shared_Terminal nodes.

      --  FIXME: update parser.line_begin_token (*).element
   end Edit_Tree;

end WisiToken.Parse;
