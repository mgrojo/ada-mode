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
      Index  : in     Syntax_Trees.Stream_Index)
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.User_Data_Access;
   begin
      Parser.Last_Grammar_Node := Parser.Tree.Get_Node (Index);

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
            Parser.Line_Begin_Token (Token.Line) :=
              (Node  => Parser.Last_Grammar_Node,
               Index => Index);

         elsif Token.ID = Parser.Descriptor.EOI_ID then
            Parser.Line_Begin_Token.Set_First_Last (Parser.Line_Begin_Token.First_Index, Token.Line + 1);
            Parser.Line_Begin_Token (Token.Line + 1) :=
              (Node  => Parser.Last_Grammar_Node,
               Index => Index);
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
      Index : Stream_Index;
   begin
      loop
         Error := Parser.Lexer.Find_Next (Token);

         if Trace_Lexer > Outline then
            Parser.Trace.Put_Line (Image (Token, Parser.Descriptor.all));
         end if;

         if Token.ID >= Parser.Descriptor.First_Terminal then
            Index := Parser.Tree.Add_Terminal (Token);
            Process_Grammar_Token (Parser, Token, Index);
         else
            Process_Non_Grammar_Token (Parser, Token);
            Index := Invalid_Stream_Index;
         end if;

         if Error then
            Parser.Wrapped_Lexer_Errors.Append
              ((Recover_Token_Index => Index,
                Error               => Parser.Lexer.Errors (Parser.Lexer.Errors.Last)));
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
      --  Parser.Lexer contains the edited text.
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

      Terminal_Index : Stream_Index       := Tree.Stream_First (Tree.Terminal_Stream);
      Parse_Stream   : constant Stream_ID := Tree.First_Parse_Stream;

      Parse_Element : Stream_Index := Tree.Stack_Top (Parse_Stream);
      --  Parse stream element containing current terminal.

      Last_Parse_Node : Node_Access;

      Next_Element_Index : Element_Index := 1;

      procedure Breakdown
      --  Bring parse stream node matching Terminal_Index to Parse_Element.
      is begin
         loop
            exit when
              --  parse stream EOI element_index may not match terminal stream.
              Tree.ID (Parse_Stream, Parse_Element) = Parser.Descriptor.EOI_ID and
              Tree.ID (Terminal_Index) = Parser.Descriptor.EOI_ID;

            exit when
              Tree.Get_Element_Index (Parse_Stream, Parse_Element) = Tree.Get_Element_Index (Terminal_Index);

            if Tree.Label (Parse_Element) = Nonterm then

               Tree.Left_Breakdown_Edit (Parse_Stream, Parse_Element);

               if WisiToken.Trace_Incremental_Parse > Extra then
                  Parser.Trace.Put_Line ("Left_Breakdown:");
                  Parser.Trace.Put_Line (Tree.Image (Parse_Stream, Children => True, Non_Grammar => True));

               elsif Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("Left_Breakdown: " & Tree.Image (Parse_Stream, Non_Grammar => True));
               end if;
            else
               Parse_Element := Tree.Stream_Next (Parse_Stream, Parse_Element);
            end if;
         end loop;
      end Breakdown;

   begin
      Parser.Last_Grammar_Node := Invalid_Node_Access;

      if Edits.Length = 0 then
         return;
      end if;

      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Element (KMN_Node);

            Stable_Region : constant Buffer_Region :=
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Deleted_Region : constant Buffer_Region :=
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);

            Parse_Node : Node_Access := Tree.First_Shared_Terminal (Tree.Get_Node (Parse_Stream, Parse_Element));

            Do_Scan : Boolean := KMN.Inserted_Bytes > 0;
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
               Parser.Trace.Put_Line ("shift:" & Shift_Bytes'Image & Shift_Chars'Image & Shift_Line'Image);
               Parser.Trace.Put_Line ("parse_element:" & Tree.Get_Element_Index (Parse_Stream, Parse_Element)'Image);
               Parser.Trace.Put_Line ("terminal_index:" & Tree.Get_Element_Index (Terminal_Index)'Image);

               if WisiToken.Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line (Tree.Image (Parse_Stream, Children => True, Non_Grammar => True));
                  Parser.Trace.New_Line;

               else
                  Parser.Trace.Put_Line (Tree.Image (Parse_Stream, Non_Grammar => True));
               end if;
            end if;

            Unchanged_Loop :
            loop
               exit Unchanged_Loop when Terminal_Index = Invalid_Stream_Index;
               exit Unchanged_Loop when not Contains
                 (Inner          => Tree.Byte_Region (Terminal_Index),
                  Outer          => Stable_Region,
                  First_Boundary => Inclusive,
                  Last_Boundary  => (if Length (Inserted_Region) = 0 and Length (Deleted_Region) = 0
                            then Inclusive
                            else Exclusive));

               exit Unchanged_Loop when Tree.ID (Terminal_Index) = Tree.Descriptor.EOI_ID;

               Tree.Shift (Terminal_Index, Shift_Bytes, Shift_Chars, Shift_Line);

               Tree.Set_Element_Index (Terminal_Index, Next_Element_Index);

               Parser.Last_Grammar_Node := Tree.Get_Node (Terminal_Index);
               --  for non_grammar, Shift_Line below

               pragma Assert (Tree.Get_Element_Index (Parse_Node) = Tree.Get_Element_Index (Parser.Last_Grammar_Node));
               Tree.Shift (Parse_Node, Shift_Bytes, Shift_Chars, Shift_Line);

               Last_Parse_Node := Parse_Node;

               --  Containing nonterm positions are updated when shifted in Parse_Incremental.

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("stable shift " & Tree.Image (Terminal_Index, Terminal_Node_Numbers => True));
               end if;

               Next_Element_Index := @ + 1;
               Terminal_Index     := Tree.Stream_Next (Terminal_Index);
               Tree.Next_Shared_Terminal (Parse_Stream, Parse_Element, Parse_Node);
            end loop Unchanged_Loop;

            if Tree.Byte_Region (Terminal_Index).Last > Scanned_Byte_Pos then
               if (Length (Inserted_Region) > 0 and then
                     Tree.Byte_Region (Terminal_Index).Last >= Inserted_Region.First - 1)
                 or
                 (Length (Deleted_Region) > 0 and then
                    Tree.Byte_Region (Terminal_Index).Last >= Deleted_Region.First - 1)
               then
                  --  Delete possibly modified token and contained non_grammar
                  Do_Scan := True;
                  declare
                     Token : constant Base_Token := Tree.Base_Token (Terminal_Index);
                  begin
                     Old_Byte_Pos := Token.Byte_Region.First;
                     Old_Char_Pos := Token.Char_Region.First;
                     Old_Line     := Token.Line;
                  end;
                  Tree.Non_Grammar_Var (Parser.Last_Grammar_Node).Clear;
                  Tree.Non_Grammar_Var (Last_Parse_Node).Clear;

               else
                  if Parser.Last_Grammar_Node /= Invalid_Node_Access then
                     declare
                        Containing : Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Parser.Last_Grammar_Node);
                        Delete     : SAL.Base_Peek_Type := 0;
                     begin
                        for I in Containing.First_Index .. Containing.Last_Index loop
                           if Overlaps (Containing (I).Byte_Region, Inserted_Region) or
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
                              Tree.Non_Grammar_Var (Last_Parse_Node).Clear;
                           else
                              Containing.Set_First_Last (Containing.First_Index, Delete - 1);
                              Tree.Non_Grammar_Var (Last_Parse_Node).Set_First_Last
                                (Containing.First_Index, Delete - 1);
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
               --  Scan last token in unchanged + new text for new/changed tokens.

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

               Breakdown;

               Scan_Changed_Loop :
               loop
                  declare
                     Token : Base_Token;
                     Error : constant Boolean := Parser.Lexer.Find_Next (Token);
                     T_Index : Stream_Index;
                     P_Index : Stream_Index;
                  begin
                     if Trace_Lexer > Outline then
                        Parser.Trace.Put_Line (Image (Token, Parser.Descriptor.all));
                     end if;

                     exit Scan_Changed_Loop when Token.ID = Parser.Descriptor.EOI_ID;
                     exit Scan_Changed_Loop when
                       Token.ID >= Parser.Descriptor.First_Terminal and then
                       not (Overlaps (Token.Byte_Region, Stable_Region) or
                              Overlaps (Token.Byte_Region, Inserted_Region) or
                              Overlaps (Token.Byte_Region, Deleted_Region));

                     Scanned_Byte_Pos := Token.Byte_Region.Last;

                     if Token.ID >= Parser.Descriptor.First_Terminal then
                        --  grammar token
                        T_Index := Tree.Insert_Shared_Terminal (Token, Next_Element_Index, Before => Terminal_Index);
                        P_Index := Tree.Insert_Shared_Terminal
                          (Parser.User_Data, Parse_Stream, T_Index, Before => Parse_Element);

                        Last_Parse_Node := Tree.Get_Node (Parse_Stream, P_Index);

                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("scan new " & Tree.Image (T_Index, Terminal_Node_Numbers => True));
                        end if;

                        Next_Element_Index := @ + 1;

                        Process_Grammar_Token (Parser, Token, T_Index);

                        --  Further breakdown of parse tree is done in Parse_Incremental or
                        --  below in Delete
                     else
                        if Trace_Incremental_Parse > Detail then
                           Parser.Trace.Put_Line
                             ("scan new " & Image (Token, Parser.Descriptor.all));
                        end if;

                        Process_Non_Grammar_Token (Parser, Token);
                        if Last_Parse_Node /= Invalid_Node_Access then
                           Tree.Non_Grammar_Var (Last_Parse_Node).Append (Token);
                        end if;
                     end if;

                     if Error then
                        Parser.Wrapped_Lexer_Errors.Append
                          ((Recover_Token_Index => T_Index,
                            Error               => Parser.Lexer.Errors (Parser.Lexer.Errors.Last)));
                     end if;
                  end;
               end loop Scan_Changed_Loop;
            end if;

            if Parser.Last_Grammar_Node /= Invalid_Node_Access and
              Tree.Stream_Prev (Terminal_Index) /= Invalid_Stream_Index
            then
               declare
                  Last_Inserted : constant WisiToken.Base_Token := Tree.Base_Token (Parser.Last_Grammar_Node);
                  Last_Deleted  : constant WisiToken.Base_Token := Tree.Base_Token (Tree.Stream_Prev (Terminal_Index));
               begin
                  Shift_Line := Last_Inserted.Line - Last_Deleted.Line;
               end;
            end if;

            Delete_Loop :
            --  Delete tokens that were deleted or modified.
            loop
               exit Delete_Loop when Tree.ID (Terminal_Index) = Parser.Descriptor.EOI_ID;

               exit Delete_Loop when not
                 (Tree.Byte_Region (Terminal_Index).First <= Deleted_Region.Last or -- deleted
                    (KMN.Inserted_Bytes > 0 and
                       Tree.Byte_Region (Terminal_Index).First = Stable_Region.Last + 1)); --  modified

               Breakdown;
               declare
                  Temp : Stream_Index := Parse_Element;
               begin
                  Parse_Element := Tree.Stream_Next (Parse_Stream, Parse_Element);
                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line
                       ("delete " & Tree.Image (Temp, Terminal_Node_Numbers => True));
                  end if;

                  Tree.Stream_Delete (Parse_Stream, Temp);
               end;
               declare
                  Temp : Stream_Index := Terminal_Index;
               begin
                  Terminal_Index := Tree.Stream_Next (Terminal_Index);
                  Tree.Stream_Delete (Temp);
               end;
            end loop Delete_Loop;

            Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
            Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

            Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
            New_Byte_Pos := Inserted_Region.Last;

            KMN_Node := Next (KMN_Node);
            exit KMN_Loop when not Has_Element (KMN_Node);
         end;
      end loop KMN_Loop;

      Tree.Set_Stack_Top (Parse_Stream, Parse_Element);

      declare
         Token : constant WisiToken.Base_Token := Tree.Base_Token (Terminal_Index);

         Parse_Node : constant Node_Access := Tree.First_Shared_Terminal (Tree.Get_Node (Parse_Stream, Parse_Element));
      begin
         if Token.ID = Parser.Descriptor.EOI_ID then
            Tree.Shift (Terminal_Index, Shift_Bytes, Shift_Chars, Shift_Line);
            Tree.Set_Element_Index (Terminal_Index, Next_Element_Index);

            pragma Assert (Tree.ID (Parse_Node) = Parser.Descriptor.EOI_ID);
            Tree.Shift (Parse_Node, Shift_Bytes, Shift_Chars, Shift_Line);

            if Trace_Incremental_Parse > Detail then
               Parser.Trace.Put_Line ("shift " & Tree.Image (Terminal_Index, Terminal_Node_Numbers => True));
            end if;
         else
            raise User_Error with "edit list does not cover entire buffer";
         end if;
      end;
   end Edit_Tree;

end WisiToken.Parse;
