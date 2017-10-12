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

   Cache_Code   : constant String := "1 ";
   Indent_Code  : constant String := "2 ";
   Error_Code   : constant String := "3 ";
   Recover_Code : constant String := "4 ";

   Chars_Per_Int : constant Integer := Integer'Width;

   procedure Put (Cache : in Cache_Type)
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
      Append (Line, Cache_Code);
      Append (Line, Integer'Image (Parse_Action_Type'Pos (Cache.Label)));
      Append (Line, Buffer_Pos'Image (Cache.Pos));
      Append (Line, Token_ID'Image (Cache.Statement_ID));
      Append (Line, Token_ID'Image (Cache.ID));
      Append (Line, Integer'Image (Cache.Length));
      Append (Line, Integer'Image (Class_Type'Pos (Cache.Class)));
      Append (Cache.Containing_Pos);
      Append (Cache.Prev_Pos);
      Append (Cache.Next_Pos);
      Append (Cache.End_Pos);
      Append (Line, ']');
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Put (Item : in Indent_Type)
   is begin
      --  All Anchors must be resolved at this point.
      if Item.Label /= Int then
         raise Programmer_Error with "Indent item has non-int label";
      end if;

      Ada.Text_IO.Put_Line
        ('[' & Indent_Code & Int_Image (Integer (Item.Begin_Pos)) & Integer'Image (Item.Int_Indent) & ']');
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
      use Cursor_Lists;
      I            : Cursor := Data.End_Positions.First;
      Delete_Cache : Boolean;
      Temp         : Cursor;
   begin
      loop
         exit when not Has_Element (I);
         declare
            Cache : Cache_Type renames Data.Caches (Element (I));
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
            Cursor_Lists.Next (I);
         end if;

      end loop;
   end Set_End;

   ----------
   --  public subprograms

   procedure Initialize
     (Data         : in out Parse_Data_Type;
      Parse_Action : in     Parse_Action_Type;
      Line_Count   : in     Ada.Containers.Count_Type := 0)
   is begin
      case Parse_Action is
      when Navigate | Face =>
         Data.Parse_Action := Parse_Action;
         Data.Caches.Finalize;
         Data.End_Positions.Clear;

      when Indent =>
         Data.Parse_Action := Indent;
         Data.Indents.Set_Length (Line_Count);
         for Item of Data.Indents loop
            Item := (Int, 1, 1);
         end loop;
      end case;
   end Initialize;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Statement_Param_Array)
   is
      First_Item         : Boolean     := True;
      Override_Start_Set : Boolean     := False;
      Override_Start     : Class_Type;
      Containing_Pos     : Nil_Buffer_Pos := Nil; --  wisi first-keyword-pos
   begin
      for Pair of Params loop
         declare
            Token : WisiToken.Token_Line_Comment.Token renames WisiToken.Token_Line_Comment.Token
              (Source.Constant_Reference (Pair.Index).Element.all);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               declare
                  Cursor : Cache_Trees.Cursor := Cache_Trees.Find
                    (Data.Caches.Iterate, Cache_Trees.Unknown, Token.Char_Region.First);
               begin
                  if Cache_Trees.Has_Element (Cursor) then
                     declare
                        Cache : Cache_Type renames Data.Caches (Cursor);
                     begin
                        Cache.Class          := (if Override_Start_Set then Override_Start else Pair.Class);
                        Cache.Statement_ID   := Nonterm.ID;
                        Cache.Containing_Pos := Containing_Pos;
                     end;
                  else
                     Cursor := Data.Caches.Insert
                       ((Label          => Navigate,
                         Pos            => Token.Char_Region.First,
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
      Source     : in     Augmented_Token_Array;
      Containing : in     Positive_Index_Type;
      Contained  : in     Positive_Index_Type)
   is
      pragma Unreferenced (Nonterm);

      --  [1] wisi-containing-action
      use Cache_Trees;
      use WisiToken.Token_Line_Comment;
      Containing_Tok    : Token renames Token (Source (Containing).Element.all);
      Containing_Region : Buffer_Region renames Containing_Tok.Char_Region;
      Contained_Tok     : Token renames Token (Source (Contained).Element.all);
      Contained_Region  : Buffer_Region renames Contained_Tok.Char_Region;
      Iterator          : constant Cache_Trees.Iterator := Data.Caches.Iterate;
      Cursor            : Cache_Trees.Cursor;
      Mark              : constant Buffer_Pos           := Containing_Region.First;
   begin
      if not (Containing_Region /= Null_Buffer_Region or Containing_Tok.Virtual) then
         raise Parse_Error with "wisi-containing-action: containing-region '" &
           Image (Containing_Tok, Data.Descriptor.all, ID_Only => True) &
           "' is empty. grammar error; bad action.";
      end if;

      if not (Contained_Tok.Char_Region = Null_Buffer_Region or
                Containing_Tok.Virtual or
                Contained_Tok.Virtual or
                Data.Caches.Present (Containing_Region.First))
      then
         raise Parse_Error with "wisi-containing-action: containing token '" &
           Data.Lexer.Buffer_Text (Containing_Tok.Byte_Region) &
           "' has no cache. grammar error; missing action.";
      end if;

      if not (Containing_Tok.Virtual or Contained_Tok.Virtual)
        and Contained_Tok.Char_Region /= Null_Buffer_Region
      then
         --  Contained region is nil when empty production.
         Cursor := Previous (Iterator, Contained_Tok.Char_Region.Last);

         while Has_Element (Cursor) loop
            declare
               Cache : Cache_Type renames Variable_Ref (Data.Caches, Cursor).Element.all;
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
      Source  : in     Augmented_Token_Array;
      Params  : in     Motion_Param_Array)
   is begin
      --  FIXME:
      null;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Face_Apply_Param_Array)
   is begin
      --  FIXME:
      null;
   end Face_Apply_Action;

   procedure Indent_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Augmented_Token'Class;
      Source  : in     Augmented_Token_Array;
      Params  : in     Indent_Param_Array)
   is begin
      --  FIXME:
      null;
   end Indent_Action;

   function Anchored_0
     (Data         : in out Parse_Data_Type;
      Index        : in     Integer;
      Indent_Delta : in     Integer)
     return Integer
   is
      pragma Unreferenced (Indent_Delta);
      pragma Unreferenced (Index);
      pragma Unreferenced (Data);
   begin
      --  FIXME:
      return 0;
   end Anchored_0;

   procedure Put (Data : in Parse_Data_Type)
   is begin
      for Cache of Data.Caches loop
         Put (Cache);
      end loop;

      for Item of Data.Indents loop
         Put (Item);
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
                    """syntax error: expecting " & Image (Descriptor, Item.Expecting) &
                    ", found '" & Item.Error_Token.Image (Descriptor, ID_Only => True) & "'""]");

               if Item.Recover /= null then
                  Put (WisiToken.Parser.LR.Configuration (Item.Recover.all), Descriptor);
               end if;
            end loop;
         end if;
      end loop;
   end Put;

end WisiToken.Wisi_Runtime;
