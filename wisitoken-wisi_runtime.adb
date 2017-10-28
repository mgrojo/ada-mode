--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Bounded;
with WisiToken.Parser.LR;
with WisiToken.Token_Line_Comment;
package body WisiToken.Wisi_Runtime is

   --  body subprograms, alphabetical

   Navigate_Cache_Code : constant String := "1 ";
   Face_Property_Code  : constant String := "2 ";
   Indent_Code         : constant String := "3 ";
   Error_Code          : constant String := "4 ";
   Recover_Code        : constant String := "5 ";

   Chars_Per_Int : constant Integer := Integer'Width;

   procedure Indent_Apply_Int (Indent : in out Indent_Type; Offset : in Integer)
   is begin
      --  [1] wisi--apply-int
      case Indent.Label is
      when Not_Set =>
         Indent := (Int, Offset);

      when Int =>
         Indent.Int_Indent := Indent.Int_Indent + Offset;

      when Anchor =>
         Indent.Anchor_Indent := Indent.Anchor_Indent + Offset;

      when Anchored | Nested_Anchor =>
         null;
      end case;
   end Indent_Apply_Int;

   procedure Indent_1
     (Data         : in out Parse_Data_Type;
      First_Line   : in     Line_Number_Type;
      Last_Line    : in     Line_Number_Type;
      Delta_Indent : in     Delta_Type)
   is
      --  [1] wisi--indent-token-1
      --  FIXME: implement 'hanging, 'anchored
      --  FIXME: implement wisi-indent-comment-col-0
   begin
      for Line in First_Line .. Last_Line loop
         declare
            --  We can't use a Reference here, because the Element in reference
            --  types is constrained (as are all allocated objects of access
            --  types; AARM 4.8 (6/3)), and we may need to change the Label.
            --  FIXME: maybe not according to AARM 3.10 (26.d/2); these objects
            --  are not actually allocated (unless the implementation of Vectors
            --  does that?)
            Indent : Indent_Type := Data.Indents (Line);
         begin
            case Delta_Indent.Label is
            when Int =>
               Indent_Apply_Int (Indent, Delta_Indent.Offset);
            when Anchored =>
               raise SAL.Not_Implemented;
            when Hanging =>
               raise SAL.Not_Implemented;
            end case;

            Data.Indents.Replace_Element (Line, Indent);
         end;
      end loop;
   end Indent_1;

   procedure Put (Cache : in Navigate_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 11 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");

      procedure Append (Item : in Nil_Buffer_Pos)
      is begin
         if Item.Set then
            Append (Line, Buffer_Pos'Image (Item.Item));
         else
            Append (Line, " -1");
         end if;
      end Append;
   begin
      Append (Line, Navigate_Cache_Code);
      Append (Line, Buffer_Pos'Image (Cache.Pos));
      Append (Line, Token_ID'Image (Cache.Statement_ID));
      Append (Line, Token_ID'Image (Cache.ID));
      Append (Line, Integer'Image (Cache.Length));
      Append (Line, Integer'Image (Navigate_Class_Type'Pos (Cache.Class)));
      Append (Cache.Containing_Pos);
      Append (Cache.Prev_Pos);
      Append (Cache.Next_Pos);
      Append (Cache.End_Pos);
      Append (Line, ']');
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Put (Cache : in Face_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 4 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");
   begin
      if Cache.Face.Set then
         Append (Line, Face_Property_Code);
         Append (Line, Buffer_Pos'Image (Cache.Region.First));
         Append (Line, Buffer_Pos'Image (Cache.Region.Last));
         Append (Line, Integer'Image (Cache.Face.Item));
         Append (Line, ']');
         Ada.Text_IO.Put_Line (To_String (Line));
      end if;
   end Put;

   procedure Put (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Item : in Indent_Type)
   is begin
      --  All Anchors must be resolved at this point, but not all lines have
      --  an indent computed. A negative indent is an error, either in the
      --  grammar indent rules, or the algorithms in this package, so it is
      --  a hard error.
      case Item.Label is
      when Not_Set =>
         null;

      when Int =>
         if Item.Int_Indent < 0 then
            Put_Error (Data, Line_Number, "indent " & Integer'Image (Item.Int_Indent) & " is < 0.");
         else
            Ada.Text_IO.Put_Line
              ('[' & Indent_Code & Int_Image (Integer (Line_Number)) & Integer'Image (Item.Int_Indent) & ']');
         end if;

      when Anchor | Anchored | Nested_Anchor =>
         raise Programmer_Error with "Indent item has non-int label";
      end case;
   end Put;

   procedure Put (Item : in WisiToken.Parser.LR.Configuration; Descriptor : in WisiToken.Descriptor'Class)
   is
      use Ada.Containers;
      subtype Bounded_Token_ID is Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 10 + Bounded_Token_ID'Width * Integer
           (Item.Popped.Length + Count_Type (Item.Pushed.Depth) + Item.Inserted.Length + Item.Deleted.Length));
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");

      procedure To_Codes (Tokens : in Token_Arrays.Vector)
      is
         First : Boolean := True;
      begin
         for ID of Tokens loop
            Append
              (Line,
               (if First
                then Int_Image (ID)
                else Token_ID'Image (ID)));
            First := False;
         end loop;
      end To_Codes;

      procedure To_Codes (Stack : in WisiToken.Parser.LR.Parser_Stacks.Stack_Type)
      is
         First : Boolean := True;
      begin
         for I in SAL.Peek_Type'First .. Stack.Depth loop
            Append
              (Line,
               (if First
                then Int_Image ((Stack.Peek (I).ID))
                else Token_ID'Image (Stack.Peek (I).ID)));
            First := False;
         end loop;
      end To_Codes;

   begin
      Append (Line, Recover_Code);
      Append (Line, '[');
      To_Codes (Item.Popped);
      Append (Line, "][");
      To_Codes (Item.Pushed);
      To_Codes (Item.Inserted);
      Append (Line, "][");
      To_Codes (Item.Deleted);
      Append (Line, "]]");
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Set_End
     (Data           : in out Parse_Data_Type;
      Containing_Pos : in     Buffer_Pos;
      End_Pos        : in     Buffer_Pos)
   is
      use Navigate_Cursor_Lists;
      I            : Cursor := Data.End_Positions.First;
      Delete_Cache : Boolean;
      Temp         : Cursor;
   begin
      loop
         exit when not Has_Element (I);
         declare
            Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Element (I));
         begin
            if Cache.Pos in Containing_Pos .. End_Pos then
               Cache.End_Pos := (True, End_Pos);
               Delete_Cache := True;
            else
               Delete_Cache := False;
            end if;
         end;
         if Delete_Cache then
            Temp := Next (I);
            Delete (Data.End_Positions, I);

            I := Temp;
         else
            Next (I);
         end if;

      end loop;
   end Set_End;

   ----------
   --  public subprograms

   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Descriptor       : access constant WisiToken.Descriptor'Class;
      Lexer            : in     WisiToken.Lexer.Handle;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type)
   is begin
      Data.Descriptor       := Descriptor;
      Data.Lexer            := Lexer;
      Data.Source_File_Name := +Source_File_Name;
      Data.Parse_Action     := Parse_Action;

      case Parse_Action is
      when Navigate =>
         Data.Navigate_Caches.Finalize;
         Data.Navigate_Caches.Initialize;
         Data.End_Positions.Clear;

      when Face =>
         Data.Face_Caches.Finalize;
         Data.Face_Caches.Initialize;

      when Indent =>
         Data.Indents.Set_Length (Ada.Containers.Count_Type (Line_Count));
      end case;
   end Initialize;

   function Source_File_Name (Data : in Parse_Data_Type) return String
   is begin
      return -Data.Source_File_Name;
   end Source_File_Name;

   function Parse_Action (Data : in Parse_Data_Type) return Parse_Action_Type
   is begin
      return Data.Parse_Action;
   end Parse_Action;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Statement_Param_Array)
   is
      First_Item         : Boolean     := True;
      Override_Start_Set : Boolean     := False;
      Override_Start     : Navigate_Class_Type;
      Containing_Pos     : Nil_Buffer_Pos := Nil; --  wisi first-keyword-pos
   begin
      for Pair of Params loop
         declare
            Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token
              (Tokens.Constant_Reference (Pair.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               declare
                  Cursor : Navigate_Cache_Trees.Cursor := Navigate_Cache_Trees.Find
                    (Data.Navigate_Caches.Iterate, Navigate_Cache_Trees.Unknown, Token.Char_Region.First);
               begin
                  if Navigate_Cache_Trees.Has_Element (Cursor) then
                     declare
                        Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                     begin
                        Cache.Class          := (if Override_Start_Set then Override_Start else Pair.Class);
                        Cache.Statement_ID   := Nonterm.ID;
                        Cache.Containing_Pos := Containing_Pos;
                     end;
                  else
                     Cursor := Data.Navigate_Caches.Insert
                       ((Pos            => Token.Char_Region.First,
                         Statement_ID   => Nonterm.ID,
                         ID             => Token.ID,
                         Length         => Length (Token.Char_Region),
                         Class          => Pair.Class,
                         Containing_Pos => Containing_Pos,
                         others         => Nil));
                  end if;

                  Data.End_Positions.Append (Cursor);
               end;

               if First_Item then
                  First_Item := False;
                  if Override_Start_Set or Pair.Class = Statement_Start then
                     Override_Start_Set := False;
                     Containing_Pos     := (True, Token.Char_Region.First);
                  end if;
               end if;

               if Pair.Class = Statement_End and Containing_Pos.Set then
                  Set_End (Data, Containing_Pos.Item, Token.Char_Region.First);
               end if;

            else
               --  Token.Region is null
               if First_Item and Pair.Class = Statement_Start then
                  --  We don't reset First_Item here; next token may also be a start, if
                  --  this one is empty.
                  Override_Start_Set := True;
                  Override_Start     := Pair.Class;
               end if;
            end if;
         end;
      end loop;
   end Statement_Action;

   procedure Containing_Action
     (Data       : in out Parse_Data_Type;
      Nonterm    : in     Augmented_Token'Class;
      Tokens     : in     Augmented_Token_Array;
      Containing : in     Positive_Index_Type;
      Contained  : in     Positive_Index_Type)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-containing-action
      use Navigate_Cache_Trees;
      use WisiToken.Token_Line_Comment;
      Containing_Tok    : Token renames Token (Tokens (Containing).Element.all);
      Containing_Region : Buffer_Region renames Containing_Tok.Char_Region;
      Contained_Tok     : Token renames Token (Tokens (Contained).Element.all);
      Contained_Region  : Buffer_Region renames Contained_Tok.Char_Region;
      Iterator          : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;
      Cursor            : Navigate_Cache_Trees.Cursor;
      Mark              : constant Buffer_Pos           := Containing_Region.First;
   begin
      if not (Containing_Region /= Null_Buffer_Region or Containing_Tok.Virtual) then
         raise Parse_Error with Error_Message
           (File_Name => -Data.Source_File_Name,
            Line      => Containing_Tok.Line,
            Col       => Containing_Tok.Col,
            Message   => "wisi-containing-action: containing-region " &
              Containing_Tok.Image (Data.Descriptor.all, ID_Only => True) &
              " is empty. grammar error; bad action.");
      end if;

      if not (Contained_Tok.Char_Region = Null_Buffer_Region or
                Containing_Tok.Virtual or
                Contained_Tok.Virtual or
                Data.Navigate_Caches.Present (Containing_Region.First))
      then
         raise Parse_Error with Error_Message
           (File_Name => -Data.Source_File_Name,
            Line      => Containing_Tok.Line,
            Col       => Containing_Tok.Col,
            Message   => "wisi-containing-action: containing token " &
              Containing_Tok.Image (Data.Descriptor.all, ID_Only => True) &
              " has no cache. grammar error; missing action.");
      end if;

      if not (Containing_Tok.Virtual or Contained_Tok.Virtual)
        and Contained_Tok.Char_Region /= Null_Buffer_Region
      then
         --  Contained region is nil when empty production.
         Cursor := Previous (Iterator, Contained_Tok.Char_Region.Last);

         while Has_Element (Cursor) loop
            declare
               Cache : Navigate_Cache_Type renames Variable_Ref (Data.Navigate_Caches, Cursor).Element.all;
            begin

               exit when Cache.Pos < Contained_Region.First or
                 (Containing_Region.First = Contained_Region.First and
                    Cache.Pos <= Contained_Region.First);

               --  Skip blocks that are already marked.

               if Cache.Containing_Pos.Set then
                  Cursor := Find (Iterator, Descending, Cache.Containing_Pos.Item);
               else
                  Cache.Containing_Pos := (True, Mark);
                  Cursor := Previous (Iterator, Cursor);
               end if;

            end;
         end loop;
      end if;
   end Containing_Action;

   function "&" (List : in Token_ID_Lists.List; Item : in Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Motion_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-motion-action
      use Navigate_Cache_Trees;
      use all type Ada.Containers.Count_Type;

      Start             : Nil_Buffer_Pos := (Set => False);
      Prev_Keyword_Mark : Nil_Buffer_Pos := (Set => False);
      Iter              : constant Iterator := Data.Navigate_Caches.Iterate;
      Prev_Cache_Cur    : Cursor;
      Cache_Cur         : Cursor;
      Point             : Buffer_Pos;

      function Match (IDs : in Token_ID_Lists.List) return Boolean
      is
         Cache : Navigate_Cache_Type renames Constant_Ref (Data.Navigate_Caches, Cache_Cur).Element.all;
      begin
         --  [1] wisi--match-token
         if (Start.Set and then Point = Start.Item) or else
           Cache.Containing_Pos = Start
         then
            for ID of IDs loop
               if ID = Cache.ID then
                  return True;
               end if;
            end loop;
         end if;
         return False;
      end Match;

   begin
      for I in Params.First_Index .. Params.Last_Index loop
         declare
            Token  : Token_Line_Comment.Token renames Token_Line_Comment.Token (Tokens (Params (I).Index).Element.all);
            Region : constant Buffer_Region := Token.Char_Region;
         begin
            if not Start.Set then
               Start := (True, Region.First);
            end if;

            if Region /= Null_Buffer_Region then
               Cache_Cur := Find (Iter, Ascending, Region.First);
               if not Has_Element (Cache_Cur) then
                  raise Parse_Error with Error_Message
                    (File_Name => -Data.Source_File_Name,
                     Line      => Token.Line,
                     Col       => Token.Col,
                     Message   => "wisi-motion-action: token " &
                       Token.Image (Data.Descriptor.all, ID_Only => False) &
                       " has no cache; add to statement-action.");
               end if;

               if Params (I).IDs.Length = 0 then
                  if Prev_Keyword_Mark.Set then
                     Variable_Ref (Data.Navigate_Caches, Cache_Cur).Element.Prev_Pos      := Prev_Keyword_Mark;
                     Variable_Ref (Data.Navigate_Caches, Prev_Cache_Cur).Element.Next_Pos := (True, Region.First);
                  end if;

                  Prev_Keyword_Mark := (True, Region.First);
                  Prev_Cache_Cur    := Cache_Cur;

               else
                  Point := Region.First;
                  loop
                     exit when Point >= Region.Last;
                     if Match (Params (I).IDs) then
                        if Prev_Keyword_Mark.Set then
                           if not Constant_Ref (Data.Navigate_Caches, Cache_Cur).Element.Prev_Pos.Set and
                             not Constant_Ref (Data.Navigate_Caches, Prev_Cache_Cur).Element.Next_Pos.Set
                           then
                              Variable_Ref (Data.Navigate_Caches, Cache_Cur).Element.Prev_Pos      := Prev_Keyword_Mark;
                              Variable_Ref (Data.Navigate_Caches, Prev_Cache_Cur).Element.Next_Pos := (True, Point);
                              Prev_Keyword_Mark := (True, Point);
                              Prev_Cache_Cur    := Cache_Cur;
                           end if;
                        else
                           Prev_Keyword_Mark := (True, Point);
                           Prev_Cache_Cur    := Cache_Cur;
                        end if;
                     end if;

                     Cache_Cur := Next (Iter, Cache_Cur);
                     Point     := Constant_Ref (Data.Navigate_Caches, Cache_Cur).Element.Pos;
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-face-apply-action
      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
              (Tokens (Param.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find (Iter, Ascending, Token.Char_Region.First);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Variable_Ref (Data.Face_Caches, Cache_Cur).Element.all;
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                        --  Check for suffix
                        Suffix_Cur := Next (Iter, Cache_Cur);
                        if Has_Element (Suffix_Cur) then
                           declare
                              Suf_Cache : Face_Cache_Type renames Variable_Ref (Data.Face_Caches, Suffix_Cur).Element.all;
                           begin
                              if Suffix = Suf_Cache.Class and
                                Inside (Suf_Cache.Region.First, Token.Char_Region)
                              then
                                 Suf_Cache.Face := (True, Param.Suffix_Face);
                              end if;
                           end;
                        end if;

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
               else
                  Data.Face_Caches.Insert ((Token.Char_Region, Suffix, (True, Param.Suffix_Face)));
               end if;
            end if;
         end;
      end loop;
   end Face_Apply_Action;

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-face-apply-list-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
              (Tokens (Param.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Constant_Ref (Data.Face_Caches, Cache_Cur).Element.Region.First > Token.Char_Region.Last;
                  declare
                     Cache : Face_Cache_Type renames Variable_Ref (Data.Face_Caches, Cache_Cur).Element.all;
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                     when Suffix =>
                        Cache.Face := (True, Param.Suffix_Face);
                     end case;
                  end;
                  Cache_Cur := Next (Iter, Cache_Cur);
               end loop;
            end if;
         end;
      end loop;
   end Face_Apply_List_Action;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Mark_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-face-apply-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
              (Tokens (Param.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find (Iter, Ascending, Token.Char_Region.First);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Variable_Ref (Data.Face_Caches, Cache_Cur).Element.all;
                     Other_Cur : Cursor := Find_In_Range (Iter, Ascending, Cache.Region.Last + 1, Token.Char_Region.Last);
                     Temp : Cursor;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Constant_Ref (Data.Face_Caches, Other_Cur).Element.Region.First > Token.Char_Region.Last;
                        Temp := Other_Cur;
                        Other_Cur := Next (Iter, Other_Cur);
                        Delete (Data.Face_Caches, Temp);
                     end loop;

                     Cache.Class       := Param.Class;
                     Cache.Region.Last := Token.Char_Region.Last;
                  end;
               else
                  Data.Face_Caches.Insert ((Token.Char_Region, Param.Class, (Set => False)));
               end if;
            end if;
         end;
      end loop;
   end Face_Mark_Action;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Face_Remove_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-face-remove-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
      Temp      : Cursor;
   begin
      for I of Params loop
         declare
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token
              (Tokens (I).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else Constant_Ref (Data.Face_Caches, Cache_Cur).Element.Region.First > Token.Char_Region.Last;
                  Temp := Cache_Cur;
                  Cache_Cur := Next (Iter, Cache_Cur);
                  Delete (Data.Face_Caches, Temp);
               end loop;
            end if;
         end;
      end loop;
   end Face_Remove_Action;

   procedure Indent_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Tokens  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      --  [1] wisi-indent-action
      for I in Tokens.First_Index .. Tokens.Last_Index loop
         declare
            use all type Ada.Containers.Count_Type;
            Token : Token_Line_Comment.Token renames Token_Line_Comment.Token (Tokens (I).Element.all);
            Pair  : Indent_Pair renames Params (I);
         begin
            if Token.Char_Region /= Null_Buffer_Region and Token.First then
               if Pair.Code_Delta /= Null_Delta then
                  if Token.ID in Data.Descriptor.First_Nonterminal .. Data.Descriptor.Last_Nonterminal then
                     Indent_1 (Data, Token.First_Indent_Line, Token.Last_Indent_Line, Pair.Code_Delta);
                  else
                     Indent_1 (Data, Token.Line, Token.Line, Pair.Code_Delta);
                  end if;
               end if;

               if Token.First_Trailing_Comment_Line /= Invalid_Line_Number and
                 (Pair.Comment_Present and then
                    Pair.Comment_Delta /= Null_Delta)
               then
                  Indent_1
                    (Data, Token.First_Trailing_Comment_Line, Token.Last_Trailing_Comment_Line, Pair.Comment_Delta);
               end if;
            end if;
         end;
      end loop;
   end Indent_Action;

   function Anchored_0
     (Data         : in out Parse_Data_Type;
      Index        : in     Integer;
      Indent_Delta : in     Integer)
     return Delta_Type
   is
      pragma Unreferenced (Indent_Delta);
      pragma Unreferenced (Index);
      pragma Unreferenced (Data);
   begin
      --  FIXME:
      return Null_Delta;
   end Anchored_0;

   function Anchored_1
     (Data         : in out Parse_Data_Type;
      Index        : in     Integer;
      Indent_Delta : in     Integer)
     return Delta_Type
   is
      pragma Unreferenced (Indent_Delta);
      pragma Unreferenced (Index);
      pragma Unreferenced (Data);
   begin
      --  FIXME:
      return Null_Delta;
   end Anchored_1;

   procedure Put (Data : in Parse_Data_Type)
   is begin
      for Cache of Data.Navigate_Caches loop
         Put (Cache);
      end loop;

      for Cache of Data.Face_Caches loop
         Put (Cache);
      end loop;

      for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         Put (Data, I, Data.Indents (I));
      end loop;
   end Put;

   procedure Put
     (Errors     : in WisiToken.Token_Region.Error_List_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
   is
      use all type Ada.Containers.Count_Type;
      use Ada.Text_IO;
   begin
      for I in Errors.First_Index .. Errors.Last_Index loop
         if Errors (I).Length > 0 then
            --  We don't include parser id here; not very useful.
            for Item of Errors (I) loop
               Put_Line
                 ('[' & Error_Code & Buffer_Pos'Image (Item.Error_Token.Char_Region.First) &
                    " ""syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                    ", found '" & Item.Error_Token.Image (Descriptor, ID_Only => True) & "'""]");

               if Item.Recover /= null then
                  Put (WisiToken.Parser.LR.Configuration (Item.Recover.all), Descriptor);
               end if;
            end loop;
         end if;
      end loop;
   end Put;

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Message : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (-Data.Source_File_Name, Line_Number, 0, Message) & """");
   end Put_Error;

end WisiToken.Wisi_Runtime;
