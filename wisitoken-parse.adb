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
      Token  : in     Base_Token;
      Node   : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.User_Data_Access;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;
   begin
      Parser.Last_Grammar_Node := Node;

      if Parser.User_Data /= null then
         Parser.User_Data.Lexer_To_Augmented (Parser.Tree, Token, Parser.Last_Grammar_Node);
      end if;

      if Token.Line /= Invalid_Line_Number then
         --  Some lexers don't support line numbers.
         if Lexer.First then
            if Tree.Line_Begin_Token.Length = 0 then
               Tree.Line_Begin_Token.Set_First_Last (Token.Line, Token.Line);
            elsif Token.Line > Tree.Line_Begin_Token.Last_Index then
               Tree.Line_Begin_Token.Set_First_Last (Tree.Line_Begin_Token.First_Index, Token.Line);
            end if;
            Tree.Line_Begin_Token (Token.Line) := Node;

         elsif Token.ID = Lexer.Descriptor.EOI_ID then
            Tree.Line_Begin_Token.Set_First_Last (Tree.Line_Begin_Token.First_Index, Token.Line + 1);
            Tree.Line_Begin_Token (Token.Line + 1) := Node;
         end if;
      end if;
   end Process_Grammar_Token;

   procedure Process_Non_Grammar_Token
     (Parser : in out Base_Parser'Class;
      Token  : in     Base_Token)
   is
      use all type Ada.Containers.Count_Type;
      use all type Syntax_Trees.Node_Access;
      use all type Syntax_Trees.User_Data_Access;

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;
   begin
      if Token.ID = Lexer.Descriptor.New_Line_ID then
         if Tree.Line_Begin_Char_Pos.Length = 0 then
            declare
               Byte_First : Buffer_Pos;
               Char_First : Buffer_Pos;
               Line_First : Line_Number_Type;
            begin
               Lexer.Begin_Pos (Byte_First, Char_First, Line_First);

               if Token.Line = Line_First then
                  Tree.Line_Begin_Char_Pos.Set_First_Last (Line_First, Token.Line + 1);
                  Tree.Line_Begin_Char_Pos (Line_First) := Char_First;
               else
                  Tree.Line_Begin_Char_Pos.Set_First_Last (Token.Line + 1, Token.Line + 1);
               end if;
            end;
         elsif Token.Line + 1 > Tree.Line_Begin_Char_Pos.Last_Index then
            Tree.Line_Begin_Char_Pos.Set_First_Last (Tree.Line_Begin_Char_Pos.First_Index, Token.Line + 1);
         end if;

         Tree.Line_Begin_Char_Pos (Token.Line + 1) := Lexer.Line_Start_Char_Pos;
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

      Tree  : Syntax_Trees.Tree renames Parser.Tree;
      Lexer : WisiToken.Lexer.Handle renames Parser.Tree.Lexer;

      Token : Base_Token;
      Error : Boolean;
      Ref   : Terminal_Ref;
   begin
      loop
         Error := Lexer.Find_Next (Token);

         if Trace_Lexer > Outline then
            Parser.Trace.Put_Line (Image (Token, Lexer.Descriptor.all));
         end if;

         if Token.ID >= Lexer.Descriptor.First_Terminal then
            Ref := Tree.Add_Terminal (Parser.Tree.Shared_Stream, Token);
            Process_Grammar_Token (Parser, Token, Ref.Node);
         else
            if Trace_Lexer > Detail then
               Parser.Trace.Put_Line
                 (if Parser.Last_Grammar_Node = Invalid_Node_Access
                  then "leading non-grammar"
                  else "non-grammar in " & Parser.Tree.Image (Parser.Last_Grammar_Node));
            end if;
            Process_Non_Grammar_Token (Parser, Token);
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
      EOF_ID : constant Token_ID := Parser.Tree.Lexer.Descriptor.EOI_ID;
   begin
      Parser.Tree.Lexer.Errors.Clear;
      Parser.Tree.Line_Begin_Token.Clear;
      Parser.Tree.Line_Begin_Char_Pos.Clear;
      Parser.Last_Grammar_Node := WisiToken.Syntax_Trees.Invalid_Node_Access;

      declare
         Byte_First : Buffer_Pos;
         Char_First : Buffer_Pos;
         Line_First : Line_Number_Type;
      begin
         Parser.Tree.Lexer.Begin_Pos (Byte_First, Char_First, Line_First);

         Parser.Tree.Line_Begin_Char_Pos.Set_First_Last (Line_First, Line_First);
         Parser.Tree.Line_Begin_Char_Pos (Line_First) := Char_First;
      end;

      loop
         exit when EOF_ID = Next_Grammar_Token (Parser);
      end loop;
      if Trace_Parse > Outline then
         Parser.Trace.Put_Line (Syntax_Trees.Get_Node_Index (Parser.Last_Grammar_Node)'Image & " tokens lexed");
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
      Stable_Byte_First        : in Buffer_Pos;
      Stable_Char_First        : in Buffer_Pos;
      Initial_Text_Byte_Region : in Buffer_Region;
      Initial_Text_Char_Region : in Buffer_Region;
      Edited_Text_Byte_Region  : in Buffer_Region;
      Edited_Text_Char_Region  : in Buffer_Region)
   is
      Initial_Byte_First : Base_Buffer_Pos := Stable_Byte_First;
      Initial_Char_First : Base_Buffer_Pos := Stable_Char_First;
      Edited_Byte_First  : Base_Buffer_Pos := Stable_Byte_First;
      Edited_Char_First  : Base_Buffer_Pos := Stable_Char_First;
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
      Old_Line         : Line_Number_Type      := 1;
      New_Byte_Pos     : Base_Buffer_Pos       := 0;
      Scanned_Byte_Pos : Base_Buffer_Pos       := 0;
      Shift_Bytes      : Base_Buffer_Pos       := 0;
      Shift_Chars      : Base_Buffer_Pos       := 0;
      Shift_Line       : Base_Line_Number_Type := 0;

      Stream : Syntax_Trees.Stream_ID; -- Tree.Shared_Stream that we are editing.

      Terminal : Terminal_Ref;

      Next_Terminal_Index : Node_Index := 1;
      Last_Shifted_Line_Begin_Char_Pos : Line_Number_Type := Line_Number_Type'First;

      procedure Breakdown (Single : in Boolean := False)
      with Post =>
        (if Single
         then Single_Terminal (Terminal)
         else Tree.First_Terminal (Stream, Terminal.Element).Node = Terminal.Node)
      is
         Target : constant Valid_Node_Access := Terminal.Node;
      begin
         if Trace_Incremental_Parse > Detail then
            Parser.Trace.Put_Line
              ("breakdown " & Tree.Image (Tree.Get_Node (Terminal.Stream, Terminal.Element)) &
                 " target " & Tree.Image (Target) &
                 (if Single then " single" else ""));
         end if;
         loop
            exit when Tree.First_Terminal (Stream, Terminal.Element).Node = Target and
              (not Single or Single_Terminal (Terminal));

            if Tree.Label (Terminal.Element) = Nonterm then
               if Trace_Incremental_Parse > Extra then
                  Parser.Trace.Put_Line
                    ("left_breakdown: " & Tree.Image
                       (Tree.Get_Node (Terminal.Stream, Terminal.Element), Non_Grammar => True));
               end if;

               Tree.Left_Breakdown (Terminal);

               if Trace_Incremental_Parse > Extra then
                  Parser.Trace.Put_Line ("... first terminal: " & Tree.Image (Terminal));
               end if;
            else
               Tree.Next_Terminal (Terminal);

               if Trace_Incremental_Parse > Extra then
                  Parser.Trace.Put_Line ("next terminal: " & Tree.Image (Terminal));
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

            Stable_Region_Chars : constant Buffer_Region :=
              (Old_Char_Pos + 1, Old_Char_Pos + KMN.Stable_Chars);

            Deleted_Region : constant Buffer_Region :=
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Deleted_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);

            Do_Scan : Boolean := KMN.Inserted_Bytes > 0;
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
                 ("KMN: " & Image (Stable_Region) & Image (Deleted_Region) & Image (Inserted_Region));
               Parser.Trace.Put_Line ("old  :" & Old_Byte_Pos'Image & Old_Char_Pos'Image & Old_Line'Image);
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

               exit Unchanged_Loop when Tree.ID (Terminal.Node) = Tree.Lexer.Descriptor.EOI_ID;

               Tree.Shift (Terminal.Node, Shift_Bytes, Shift_Chars, Shift_Line);

               declare
                  Non_Grammar : Base_Token_Array_Const_Ref renames Tree.Non_Grammar_Const (Terminal.Node);
                  Line : constant Line_Number_Type :=
                    (if Non_Grammar.Length > 0
                     then
                       (declare Token : Base_Token renames Non_Grammar (Non_Grammar.Last_Index);
                        begin
                           (if Token.ID = Tree.Lexer.Descriptor.New_Line_ID
                            then Token.Line + 1
                            else Token.Line))
                     else Tree.Base_Token (Terminal.Node).Line);
               begin
                  if Last_Shifted_Line_Begin_Char_Pos < Line then
                     for L in Last_Shifted_Line_Begin_Char_Pos + 1 .. Line loop
                        Tree.Line_Begin_Char_Pos (L) := Tree.Line_Begin_Char_Pos (L) + Shift_Chars;
                     end loop;
                     Last_Shifted_Line_Begin_Char_Pos := Line;
                  end if;
               end;

               Parser.Last_Grammar_Node := Terminal.Node;

               Tree.Set_Terminal_Index (Terminal.Node, Next_Terminal_Index);

               if Trace_Incremental_Parse > Detail then
                  Parser.Trace.Put_Line ("stable shift " & Tree.Image (Terminal.Node, Terminal_Node_Numbers => True));
               end if;

               Next_Terminal_Index := @ + 1;

               Tree.Next_Terminal (Terminal);
            end loop Unchanged_Loop;

            --  FIXME: delete trailing virtual_terminals in stable_region

            --  Scanning is needed if Inserted_Region is not empty, or if the
            --  deleted region overlaps or is adjacent to a preceding or following
            --  token. If insert/delete is inside or after a comment, the scanning
            --  may already have been done. Set Do_Scan, Old_* to the scan start
            --  position; the start of Terminal or in the preceding
            --  non_grammar. FIXME: handle Tree.Leading_Non_Grammar.
            declare
               Token : constant Base_Token := Tree.Base_Token (Terminal.Node);

               Prev_Token_ID : Token_ID := Invalid_Token_ID; --  New_Line or Invalid, for Lexer.Set_Position

               Delta_Line : Base_Line_Number_Type := 0;

               procedure Set_Old (Token : in Base_Token)
               is begin
                  Old_Byte_Pos := Buffer_Pos'Min (Token.Byte_Region.First, Stable_Region.Last + 1);
                  Old_Char_Pos := Buffer_Pos'Min (Token.Char_Region.First, Stable_Region_Chars.Last + 1);
                  Old_Line     := Token.Line;
               end Set_Old;

            begin
               if Token.Byte_Region.Last + Shift_Bytes > Scanned_Byte_Pos and
                 (Length (Deleted_Region) > 0 or
                    Length (Inserted_Region) > 0)
               then
                  --  Edit point is in Terminal or non-grammar or whitespace before Terminal.

                  if Overlaps (Token.Byte_Region + Shift_Bytes, Adjust (Inserted_Region, -1, +1)) or
                    Overlaps (Token.Byte_Region, Adjust (Deleted_Region, -1, +1))
                  then
                     if Token.ID = Tree.Lexer.Descriptor.EOI_ID then
                        --  We never re-scan eoi; we just shift it.
                        null;

                     else
                        --  Terminal is possibly modified; it will be deleted in Delete_Loop
                        --  below.
                        Do_Scan := True;
                        Set_Old (Token);

                        if Old_Line in Tree.Line_Begin_Token.First_Index .. Tree.Line_Begin_Token.Last_Index
                          and then Tree.Line_Begin_Token (Old_Line) = Terminal.Node
                        then
                           Prev_Token_ID := Tree.Lexer.Descriptor.New_Line_ID;
                           Last_Shifted_Line_Begin_Char_Pos := Token.Line;

                        else
                           Prev_Token_ID := Invalid_Token_ID;
                        end if;
                     end if;

                  else
                     --  Edit start is in some preceding non-grammar token or whitespace;
                     --  delete tokens after the edit start. Deleted New_Lines decrement
                     --  Shift_Line, but only after we use Shift_Line in
                     --  Lexer.Set_Position.
                     declare
                        procedure Handle_Non_Grammar (Containing : in out Base_Token_Arrays.Vector)
                        is
                           Delete        : SAL.Base_Peek_Type := 0;
                           Last_New_Line : SAL.Base_Peek_Type := 0;
                        begin
                           if Containing.Length = 0 then
                              --  Edit is in whitespace before Terminal
                              if Parser.Last_Grammar_Node = Invalid_Node_Access then
                                 --  No previous grammar or non-grammar token
                                 Old_Byte_Pos  := Stable_Region.Last + 1;
                                 Old_Char_Pos  := Stable_Region_Chars.Last + KMN.Stable_Chars + 1;
                                 Prev_Token_ID := Tree.Lexer.Descriptor.New_Line_ID;
                                 pragma Assert (Last_Shifted_Line_Begin_Char_Pos = Line_Number_Type'First);

                              else
                                 declare
                                    Token : constant Base_Token := Tree.Base_Token (Parser.Last_Grammar_Node);
                                 begin
                                    Old_Byte_Pos := Token.Byte_Region.Last + 1;
                                    Old_Char_Pos := Token.Char_Region.Last + 1;
                                    Old_Line     := Token.Line;
                                 end;

                                 Prev_Token_ID := Invalid_Token_ID;
                              end if;
                           else
                              for I in Containing.First_Index .. Containing.Last_Index loop
                                 if Containing (I).ID = Tree.Lexer.Descriptor.New_Line_ID then
                                    Last_New_Line := I;
                                 end if;

                                 if Containing (I).Byte_Region.Last >= Stable_Region.Last + 1 then
                                    Delete  := I;
                                    Do_Scan := True;
                                    exit;
                                 end if;
                              end loop;

                              if Delete > 0 then
                                 --  Edit is in or before a nongrammar (ie a comment); scan the entire token.
                                 Set_Old (Containing (Delete));

                                 if Trace_Incremental_Parse > Detail then
                                    Parser.Trace.Put_Line
                                      ("delete non_grammar" & Tree.Get_Node_Index (Parser.Last_Grammar_Node)'Image &
                                         Delete'Image & " .." & Containing.Last_Index'Image);
                                 end if;
                                 for I in Delete .. Containing.Last_Index loop
                                    if Containing (I).ID = Tree.Lexer.Descriptor.New_Line_ID then
                                       Delta_Line := @ - 1;
                                    end if;
                                 end loop;

                                 Containing.Set_First_Last (Containing.First_Index, Delete - 1);

                                 if Last_New_Line = Delete - 1 then
                                    Prev_Token_ID := Tree.Lexer.Descriptor.New_Line_ID;
                                    if Last_New_Line /= 0 and then
                                      Containing (Last_New_Line).Line < Last_Shifted_Line_Begin_Char_Pos
                                    then
                                       declare
                                          Line : constant Line_Number_Type := Containing (Last_New_Line).Line;
                                       begin
                                          Last_Shifted_Line_Begin_Char_Pos := Line;
                                          Tree.Line_Begin_Char_Pos (Line) :=
                                            Tree.Line_Begin_Char_Pos (Line) + Shift_Chars;
                                       end;
                                    end if;
                                 else
                                    Prev_Token_ID := Invalid_Token_ID;
                                 end if;

                              else
                                 --  Edit is in whitespace between Last_Grammar_Node and Terminal
                                 declare
                                    Token : constant Base_Token := Tree.Base_Token (Parser.Last_Grammar_Node);
                                 begin
                                    Old_Byte_Pos  := Token.Byte_Region.Last + 1;
                                    Old_Char_Pos  := Token.Char_Region.Last + 1;
                                    Old_Line      := Token.Line;
                                    Prev_Token_ID := Invalid_Token_ID;
                                 end;
                              end if;
                           end if;
                        end Handle_Non_Grammar;
                     begin
                        if Parser.Last_Grammar_Node = Invalid_Node_Access then
                           Handle_Non_Grammar (Tree.Leading_Non_Grammar);
                        else
                           Handle_Non_Grammar (Tree.Non_Grammar_Var (Parser.Last_Grammar_Node));
                        end if;
                     end;
                  end if;
               end if;

               if Do_Scan then
                  --  Scan a possibly modified token + new text + following posibbly
                  --  modified token for new/changed tokens.
                  declare
                     Start_Byte : constant Buffer_Pos := Buffer_Pos'Max (Buffer_Pos'First, Old_Byte_Pos + Shift_Bytes);
                     Start_Char : constant Buffer_Pos := Buffer_Pos'Max (Buffer_Pos'First, Old_Char_Pos + Shift_Chars);
                     Start_Line : constant Line_Number_Type := Line_Number_Type'Max
                       (Line_Number_Type'First, Old_Line + Shift_Line);
                  begin
                     Shift_Line := @ + Delta_Line;

                     if Trace_Incremental_Parse > Outline then
                        Parser.Trace.Put_Line
                          ("lexer.set_position" & Start_Byte'Image & Start_Char'Image & Start_Line'Image &
                             " prev_token_id " &
                             (if Prev_Token_ID = Invalid_Token_ID
                              then "<invalid>"
                              else Image (Prev_Token_ID, Tree.Lexer.Descriptor.all)));
                     end if;

                     Parser.Tree.Lexer.Set_Position
                       (Byte_Position => Start_Byte,
                        Char_Position => Start_Char,
                        Line          => Start_Line,
                        Prev_Token_ID => Prev_Token_ID);
                  end;

                  --  Ensure Terminal.Node is first in Terminal.Element, so we can insert before it.
                  Breakdown;

                  Scan_Changed_Loop :
                  loop
                     declare
                        Token : Base_Token;
                        Error : constant Boolean := Parser.Tree.Lexer.Find_Next (Token);
                        Ref   : Terminal_Ref;
                     begin
                        if Trace_Lexer > Outline then
                           Parser.Trace.Put_Line ("lex: " & Image (Token, Parser.Tree.Lexer.Descriptor.all));
                        end if;

                        exit Scan_Changed_Loop when Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID;
                        exit Scan_Changed_Loop when
                          Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal and then
                          not (Token.Byte_Region.First - Shift_Bytes <= Stable_Region.Last or
                                 (KMN.Inserted_Bytes > 0 and then
                                    --  Token starts in inserted region (test_incremental.adb Edit_Code_4 '1 +')
                                    Token.Byte_Region.First <= Inserted_Region.Last
                                 ) or
                                 --  Token starts immediately after deleted or inserted region; it may
                                 --  have been truncated (test_incremental.adb Edit_Code_4 'Cc') or
                                 --  extended; the old token will be deleted below (Edit_Code_8 ';'):
                                 Token.Byte_Region.First - (Shift_Bytes + KMN.Inserted_Bytes) = Stable_Region.Last + 1
                              );

                        Scanned_Byte_Pos := Token.Byte_Region.Last;

                        if Token.ID >= Parser.Tree.Lexer.Descriptor.First_Terminal then
                           --  grammar token
                           Ref := Tree.Insert_Source_Terminal
                             (Stream, Token, Next_Terminal_Index, Before => Terminal.Element);

                           Next_Terminal_Index := @ + 1;

                           Process_Grammar_Token (Parser, Token, Ref.Node);

                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line ("scan new " & Tree.Image (Ref));
                           end if;

                        else
                           --  non-grammar token
                           if Trace_Incremental_Parse > Detail then
                              Parser.Trace.Put_Line
                                ("scan new " & Image (Token, Parser.Tree.Lexer.Descriptor.all));
                           end if;

                           Process_Non_Grammar_Token (Parser, Token);
                           if Token.ID = Parser.Tree.Lexer.Descriptor.New_Line_ID then
                              Shift_Line := @ + 1;
                              Last_Shifted_Line_Begin_Char_Pos := Token.Line + 1;
                           end if;
                        end if;

                        if Error then
                           Parser.Wrapped_Lexer_Errors.Append
                             ((Recover_Token_Ref => Ref,
                               Error             => Parser.Tree.Lexer.Errors (Parser.Tree.Lexer.Errors.Last)));
                        end if;
                     end;
                  end loop Scan_Changed_Loop;
               end if;
            end;

            Delete_Loop :
            --  Delete tokens that were deleted or modified. Deleted non-grammar
            --  New_Lines decrement Shift_Line.
            loop
               exit Delete_Loop when Tree.ID (Terminal.Node) = Parser.Tree.Lexer.Descriptor.EOI_ID;

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
                  Temp : Stream_Node_Ref := Terminal;
                  Line : constant Line_Number_Type := Tree.Base_Token (Temp.Node).Line;
               begin
                  Tree.Next_Shared_Terminal (Terminal);
                  if Trace_Incremental_Parse > Detail then
                     Parser.Trace.Put_Line ("delete " & Tree.Image (Temp.Element, Terminal_Node_Numbers => True));
                  end if;

                  for Token of Tree.Non_Grammar_Const (Temp.Node) loop
                     if Token.ID = Tree.Lexer.Descriptor.New_Line_ID then
                        Shift_Line := @ - 1;
                     end if;
                  end loop;

                  if Line in
                    Tree.Line_Begin_Token.First_Index .. Tree.Line_Begin_Token.Last_Index
                    and then Tree.Line_Begin_Token (Line) = Temp.Node
                  then
                     Tree.Line_Begin_Token (Line) := Invalid_Node_Access;
                     --  FIXME: set to next token on line
                  end if;

                  Tree.Stream_Delete (Stream, Temp.Element);
               end;
            end loop Delete_Loop;

            Shift_Bytes := @ - KMN.Deleted_Bytes + KMN.Inserted_Bytes;
            Shift_Chars := @ - KMN.Deleted_Chars + KMN.Inserted_Chars;

            Old_Byte_Pos := Stable_Region.Last + KMN.Deleted_Bytes;
            Old_Char_Pos := Stable_Region_Chars.Last + KMN.Deleted_Chars;
            New_Byte_Pos := Inserted_Region.Last;
            pragma Assert (New_Byte_Pos - Old_Byte_Pos = Shift_Bytes);

            KMN_Node := Next (KMN_Node);
            exit KMN_Loop when not Has_Element (KMN_Node);
         end;
      end loop KMN_Loop;

      declare
         Token : constant WisiToken.Base_Token := Tree.Base_Token (Terminal.Node);
      begin
         --  EOI is at stream end.

         if Token.ID = Parser.Tree.Lexer.Descriptor.EOI_ID then
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
      --  IMPROVEME: between this and shift_stable, every node in the tree
      --  is visited; clearly not incremental. To eliminate this, we would
      --  need to delete the caches in the nonterm nodes (byte_region,
      --  char_region, first_terminal) and change the terminal nodes to
      --  store byte, char length, not region. Then every use of the
      --  currently cached values would compute them.

      --  FIXME: update parser.line_begin_token (*).element
   end Edit_Tree;

end WisiToken.Parse;
