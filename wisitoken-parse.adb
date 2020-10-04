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

   procedure Edit_Tree
     (Parser : in out Base_Parser'Class;
      Edits  : in     KMN_Lists.List)
   is
      --  Similar to [Lahav 2004] Algorithms 3, 4. That assumes creating a
      --  separate temp list of new tokens, and then merging that into the
      --  parse tree, is faster than merging new tokens in one by one; we
      --  just do the latter. We also don't modify the edit list.
      use KMN_Lists;
      use all type WisiToken.Syntax_Trees.Element_Index;
      use all type WisiToken.Syntax_Trees.Stream_Index;

      Tree : Syntax_Trees.Tree renames Parser.Tree;

      KMN_Node     : Cursor                := Edits.First;
      Old_Byte_Pos : Base_Buffer_Pos       := 0;
      New_Byte_Pos : Base_Buffer_Pos       := 0;
      Shift_Bytes  : Base_Buffer_Pos       := 0;
      Shift_Chars  : Base_Buffer_Pos       := 0;
      Shift_Line   : Base_Line_Number_Type := 0;

      Terminal_Index : Syntax_Trees.Stream_Index := Tree.Stream_First (Tree.Terminal_Stream);
      --  FIXME: Parse_Node : Syntax_Trees.Stream_Index := Tree.First (Parser.Stream);

      Next_Element_Index : Syntax_Trees.Element_Index := 1;
   begin
      KMN_Loop :
      loop
         declare
            KMN : constant WisiToken.Parse.KMN := Element (KMN_Node);

            Stable_Region : constant Buffer_Region :=
              (Old_Byte_Pos + 1, Old_Byte_Pos + KMN.Stable_Bytes);

            Deleted_Region : constant Buffer_Region :=
              (Stable_Region.Last + 1, Stable_Region.Last + KMN.Stable_Bytes);

            Inserted_Region : constant Buffer_Region :=
              (New_Byte_Pos + KMN.Stable_Bytes + 1, New_Byte_Pos + KMN.Stable_Bytes + KMN.Inserted_Bytes);
         begin
            Unchanged_Loop :
            loop
               exit Unchanged_Loop when Terminal_Index = Syntax_Trees.Invalid_Stream_Index;
               exit Unchanged_Loop when not Contains
                 (Inner => Tree.Byte_Region (Terminal_Index), Outer => Stable_Region);

               Tree.Shift (Terminal_Index, Shift_Bytes, Shift_Chars, Shift_Line);

               Tree.Set_Element_Index (Terminal_Index, Next_Element_Index);
               Next_Element_Index := @ + 1;

               Parser.Last_Grammar_Node := Tree.Get_Node (Terminal_Index); -- for Shift_Line below

               Terminal_Index := Tree.Stream_Next (Terminal_Index);
            end loop Unchanged_Loop;

            --  FIXME: update byte_, char_regions in portion of parse tree
            --  containing stable_region

            declare
               Token : constant WisiToken.Base_Token := Tree.Base_Token (Tree.Stream_Prev (Terminal_Index));
            begin
               Parser.Lexer.Set_Position
                 (Byte_Position => Token.Byte_Region.Last + Shift_Bytes + 1,
                  Char_Position => Token.Char_Region.Last + Shift_Chars + 1,
                  Line          => Token.Line + Shift_Line);
            end;

            Scan_Changed_Loop :
            loop
               declare
                  Token : Base_Token;
                  Error : constant Boolean := Parser.Lexer.Find_Next (Token);
                  Index : Syntax_Trees.Stream_Index;
               begin

                  if Trace_Lexer > Outline then
                     Parser.Trace.Put_Line (Image (Token, Parser.Descriptor.all));
                  end if;

                  exit Scan_Changed_Loop when Token.Byte_Region not in Inserted_Region;

                  if Token.ID >= Parser.Descriptor.First_Terminal then
                     --  grammar token
                     Index := Tree.Insert_Terminal (Token, Next_Element_Index, Before => Terminal_Index);

                     Next_Element_Index := @ + 1;

                     Process_Grammar_Token (Parser, Token, Index);

                     --  FIXME: breakdown parse tree
                  else
                     Process_Non_Grammar_Token (Parser, Token);
                  end if;

                  if Error then
                     Parser.Wrapped_Lexer_Errors.Append
                       ((Recover_Token_Index => Index,
                         Error               => Parser.Lexer.Errors (Parser.Lexer.Errors.Last)));
                  end if;
               end;
            end loop Scan_Changed_Loop;

            declare
               Last_Inserted : constant WisiToken.Base_Token := Tree.Base_Token (Parser.Last_Grammar_Node);
               Last_Deleted  : constant WisiToken.Base_Token := Tree.Base_Token (Tree.Stream_Prev (Terminal_Index));
            begin
               Shift_Line := Last_Inserted.Line - Last_Deleted.Line;
            end;

            Delete_Loop :
            loop
               exit Delete_Loop when Terminal_Index = Syntax_Trees.Invalid_Stream_Index;
               exit Delete_Loop when not Contains
                 (Inner => Tree.Byte_Region (Terminal_Index), Outer => Deleted_Region);

               declare
                  Temp : Syntax_Trees.Stream_Index := Terminal_Index;
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
   end Edit_Tree;

end WisiToken.Parse;
