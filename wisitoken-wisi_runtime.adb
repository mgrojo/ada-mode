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
with WisiToken.LR;
package body WisiToken.Wisi_Runtime is

   Navigate_Cache_Code : constant String := "1 ";
   Face_Property_Code  : constant String := "2 ";
   Indent_Code         : constant String := "3 ";
   Error_Code          : constant String := "4 ";
   Recover_Code        : constant String := "5 ";

   Chars_Per_Int : constant Integer := Integer'Width;

   ----------
   --  body subprogram specs (as needed), alphabetical

   function Indent_Zero_P (Indent : in Indent_Type) return Boolean;

   function Max_Anchor_ID
     (Data       : in out Parse_Data_Type;
      First_Line : in     Line_Number_Type;
      Last_Line  : in     Line_Number_Type)
     return Integer;

   function Paren_In_Anchor_Line
     (Data         : in Parse_Data_Type'Class;
      Anchor_Token : in Semantic_State.Augmented_Token;
      Offset       : in Integer)
     return Integer;

   ----------
   --  body subprograms bodies, alphabetical

   function Image (Anchor_IDs : in Anchor_ID_Vectors.Vector) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String := +"(";
   begin
      for I in Anchor_IDs.First_Index .. Anchor_IDs.Last_Index loop
         Result := Result & Integer'Image (Anchor_IDs (I));
         if I /= Anchor_IDs.Last_Index then
            Result := Result & ", ";
         else
            Result := Result & ")";
         end if;
      end loop;
      return -Result;
   end Image;

   function Image (Indent : in Indent_Type) return String
   is begin
      case Indent.Label is
      when Not_Set =>
         return "(" & Indent_Label'Image (Indent.Label) & ")";

      when Int =>
         return "(" & Indent_Label'Image (Indent.Label) & Integer'Image (Indent.Int_Indent) & ")";

      when Anchor =>
         return "(" & Indent_Label'Image (Indent.Label) & Image (Indent.Anchor_IDs) & ", " & Integer'Image
           (Indent.Anchor_Indent) & ")";

      when Anchored =>
         return "(" & Indent_Label'Image (Indent.Label) & Integer'Image (Indent.Anchored_ID) & ", " & Integer'Image
           (Indent.Anchored_Delta) & ")";

      when Anchor_Anchored =>
         return "(" & Indent_Label'Image (Indent.Label) & Image (Indent.Anchor_Anchored_IDs) & Integer'Image
           (Indent.Anchor_Anchored_ID) & ", " & Integer'Image (Indent.Anchor_Anchored_Delta) & ")";
      end case;
   end Image;

   procedure Indent_Apply_Anchored
     (Delta_Indent : in     Anchored_Delta;
      Indent       : in out Indent_Type)
   is begin
      --  [2] wisi-elisp-parse--apply-anchored

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchored, Delta_Indent.Anchored_ID, Delta_Indent.Anchored_Delta);

      when Int =>
         if Indent.Int_Indent = 0 or Delta_Indent.Anchored_Accumulate then
            Indent := (Anchored, Delta_Indent.Anchored_ID, Indent.Int_Indent + Delta_Indent.Anchored_Delta);
         end if;

      when Anchor =>
         if Delta_Indent.Anchored_Accumulate or Indent.Anchor_Indent = 0 then
            Indent :=
              (Anchor_Anchored,
               Indent.Anchor_IDs,
               Delta_Indent.Anchored_ID,
               Delta_Indent.Anchored_Delta + Indent.Anchor_Indent);
         end if;

      when Anchored | Anchor_Anchored =>
         --  already anchored
         null;
      end case;
   end Indent_Apply_Anchored;

   procedure Indent_Apply_Int (Indent : in out Indent_Type; Offset : in Integer)
   is begin
      --  [2] wisi-elisp-parse--apply-int
      case Indent.Label is
      when Not_Set =>
         Indent := (Int, Offset);

      when Int =>
         Indent.Int_Indent := Indent.Int_Indent + Offset;

      when Anchor =>
         Indent.Anchor_Indent := Indent.Anchor_Indent + Offset;

      when Anchored | Anchor_Anchored =>
         null;
      end case;
   end Indent_Apply_Int;

   procedure Indent_Line
     (Data         : in out Parse_Data_Type;
      Line         : in     Line_Number_Type;
      Delta_Indent : in     Delta_Type)
   is
      --  See note in Indent_Anchored_2 for why we can't use renames here.
      Indent : Indent_Type := Data.Indents (Line);
   begin
      case Delta_Indent.Label is
      when Simple =>
         case Delta_Indent.Simple_Delta.Label is
         when Int =>
            Indent_Apply_Int (Indent, Delta_Indent.Simple_Delta.Int_Delta);

         when Anchored =>
            Indent_Apply_Anchored (Delta_Indent.Simple_Delta, Indent);
         end case;

      when Hanging =>
         if Delta_Indent.Hanging_Accumulate or Indent_Zero_P (Data.Indents (Line)) then
            if Line = Delta_Indent.Hanging_First_Line then
               --  Apply delta_1
               case Delta_Indent.Hanging_Delta_1.Label is
               when Int =>
                  Indent_Apply_Int (Indent, Delta_Indent.Hanging_Delta_1.Int_Delta);
               when Anchored =>
                  Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_1, Indent);
               end case;
            else
               if Delta_Indent.Hanging_Paren_State = Data.Semantic_State.Line_Paren_State (Line) then
                  case Delta_Indent.Hanging_Delta_2.Label is
                  when Int =>
                     Indent_Apply_Int (Indent, Delta_Indent.Hanging_Delta_2.Int_Delta);
                  when Anchored =>
                     Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_2, Indent);
                  end case;
               end if;
            end if;
         end if;
      end case;
      Data.Indents.Replace_Element (Line, Indent);
   end Indent_Line;

   function Indent_Zero_P (Indent : in Indent_Type) return Boolean
   is begin
      --  wisi-elisp-parse--indent-zero-p
      case Indent.Label is
      when Not_Set =>
         return True;

      when Int =>
         return Indent.Int_Indent = 0;

      when Anchor =>
         return Indent.Anchor_Indent = 0;

      when Anchored =>
         return Indent.Anchored_Delta = 0;

      when Anchor_Anchored =>
         return Indent.Anchor_Anchored_Delta = 0;
      end case;
   end Indent_Zero_P;

   function Max_Anchor_ID
     (Data       : in out Parse_Data_Type;
      First_Line : in     Line_Number_Type;
      Last_Line  : in     Line_Number_Type)
     return Integer
   is
      Result : Integer := First_Anchor_ID - 1;
   begin
      for Line in First_Line .. Last_Line loop
         declare
            Indent : Indent_Type renames Data.Indents (Line);
         begin
            case Indent.Label is
            when Not_Set | Int =>
               null;
            when Anchor =>
               Result := Integer'Max (Result, Indent.Anchor_IDs (Indent.Anchor_IDs.First_Index));
            when Anchored =>
               Result := Integer'Max (Result, Indent.Anchored_ID);
            when Anchor_Anchored =>
               Result := Integer'Max (Result, Indent.Anchor_Anchored_ID);
            end case;
         end;
      end loop;
      return Result;
   end Max_Anchor_ID;

   function Paren_In_Anchor_Line
     (Data         : in Parse_Data_Type'Class;
      Anchor_Token : in Semantic_State.Augmented_Token;
      Offset       : in Integer)
     return Integer
   is
      use all type SAL.Base_Peek_Type;
      use all type Ada.Containers.Count_Type;

      Descriptor     : WisiToken.Descriptor'Class renames Data.Semantic_State.Trace.Descriptor.all;
      Left_Paren_ID  : WisiToken.Token_ID renames Descriptor.Left_Paren_ID;
      Right_Paren_ID : WisiToken.Token_ID renames Descriptor.Right_Paren_ID;

      I              : Positive_Index_Type := Anchor_Token.First_All_Tokens_Index;
      Paren_Count    : Integer             := 0;
      Paren_Char_Pos : Buffer_Pos          := Invalid_Buffer_Pos;
      Text_Begin_Pos : Buffer_Pos          := Invalid_Buffer_Pos;
   begin
      --  [1] wisi-elisp-parse--paren-in-anchor-line. That uses elisp syntax-ppss; here
      --  we search All_Tokens.
      loop
         declare
            Tok : Semantic_State.Augmented_Token renames Data.Semantic_State.All_Tokens (I).Element.all;
         begin
            if Tok.ID = Left_Paren_ID then
               Paren_Count := Paren_Count + 1;
               if Paren_Count = 1 then
                  Paren_Char_Pos := Tok.Char_Region.First;
               end if;

            elsif Tok.ID = Right_Paren_ID then
               Paren_Count := Paren_Count - 1;

            end if;

            if Tok.First then
               Text_Begin_Pos := Tok.Char_Region.First;
               exit;
            end if;
         end;
         I := I - 1;
      end loop;

      if Paren_Char_Pos /= Invalid_Buffer_Pos and Text_Begin_Pos /= Invalid_Buffer_Pos then
         return 1 + Offset + Integer (Paren_Char_Pos - Text_Begin_Pos);
      else
         return Offset;
      end if;
   end Paren_In_Anchor_Line;

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
      Append (Line, WisiToken.Token_ID'Image (Cache.Statement_ID));
      Append (Line, WisiToken.Token_ID'Image (Cache.ID));
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
      --  an indent computed. A negative indent is an error in either the
      --  grammar indent rules or the algorithms in this package.
      case Item.Label is
      when Not_Set =>
         Ada.Text_IO.Put_Line
           ('[' & Indent_Code & Int_Image (Integer (Line_Number)) & " 0]");

      when Int =>
         if Item.Int_Indent < 0 then
            Put_Error (Data, Line_Number, "indent " & Integer'Image (Item.Int_Indent) & " is < 0.");

         else
            Ada.Text_IO.Put_Line
              ('[' & Indent_Code & Int_Image (Integer (Line_Number)) & Integer'Image (Item.Int_Indent) & ']');
         end if;

      when Anchor | Anchored | Anchor_Anchored =>
         raise Programmer_Error with "Indent item has non-int label: " & Indent_Label'Image (Item.Label);
      end case;
   end Put;

   procedure Put (Item : in WisiToken.LR.Configuration; Descriptor : in WisiToken.Descriptor'Class)
   is
      use all type SAL.Base_Peek_Type;
      subtype Bounded_Token_ID is WisiToken.Token_ID range Descriptor.First_Terminal .. Descriptor.Last_Terminal;
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 10 + Bounded_Token_ID'Width * Integer
           (Item.Popped.Length + Item.Pushed.Depth + Item.Inserted.Length + Item.Deleted.Length));
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");

      procedure To_Codes (Tokens : in WisiToken.LR.Fast_Token_ID_Vectors.Vector)
      is
         First : Boolean := True;
      begin
         for ID of Tokens loop
            Append
              (Line,
               (if First
                then Int_Image (ID)
                else WisiToken.Token_ID'Image (ID)));
            First := False;
         end loop;
      end To_Codes;

      procedure To_Codes (Stack : in WisiToken.LR.Parser_Stacks.Stack_Type)
      is
         First : Boolean := True;
      begin
         for I in SAL.Peek_Type'First .. Stack.Depth loop
            Append
              (Line,
               (if First
                then Int_Image ((Stack.Peek (I).Token.ID)) -- elisp does not need Name
                else WisiToken.Token_ID'Image (Stack.Peek (I).Token.ID)));
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

   procedure Resolve_Anchors (Data : in out Parse_Data_Type)
   is
      Anchor_Indent : array (First_Anchor_ID .. Data.Max_Anchor_ID) of Integer;
   begin
      if Trace_Parse > Detail then
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Ada.Text_IO.Put_Line (Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
      end if;

      if Data.Max_Anchor_ID >= First_Anchor_ID then
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            declare
               Indent : constant Indent_Type := Data.Indents (I);
            begin
               case Indent.Label is
               when Not_Set | Int =>
                  null;

               when Anchor =>
                  for I of Indent.Anchor_IDs loop
                     Anchor_Indent (I) := Indent.Anchor_Indent;
                  end loop;
                  Data.Indents.Replace_Element (I, (Int, Indent.Anchor_Indent));

               when Anchored =>
                  Data.Indents.Replace_Element (I, (Int, Anchor_Indent (Indent.Anchored_ID) + Indent.Anchored_Delta));

               when Anchor_Anchored =>
                  declare
                     Temp : constant Integer :=
                       Anchor_Indent (Indent.Anchor_Anchored_ID) + Indent.Anchor_Anchored_Delta;
                  begin
                     for I of Indent.Anchor_Anchored_IDs loop
                        Anchor_Indent (I) := Temp;
                     end loop;
                     Data.Indents.Replace_Element (I, (Int, Temp));
                  end;
               end case;
            end;
         end loop;
      end if;
   end Resolve_Anchors;

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
   --  public subprograms (declaration order)

   procedure Initialize
     (Data             : in out Parse_Data_Type;
      Semantic_State   : in     WisiToken.Semantic_State.Semantic_State_Access;
      Source_File_Name : in     String;
      Parse_Action     : in     Parse_Action_Type;
      Line_Count       : in     Line_Number_Type;
      Params           : in     String)
   is
      pragma Unreferenced (Params);
   begin
      Data.Semantic_State := Semantic_State;
      Data.Semantic_State.Initialize (Line_Count);

      Data.Source_File_Name := +Source_File_Name;
      Data.Parse_Action     := Parse_Action;

      case Parse_Action is
      when Navigate | Face =>
         null;
      when Indent =>
         Data.Indents.Set_Length (Ada.Containers.Count_Type (Line_Count));
      end case;

      Data.Reset (Init_Done => True);
   end Initialize;

   procedure Reset (Data : in out Parse_Data_Type; Init_Done : in Boolean := False)
   is begin
      if not Init_Done then
         Data.Semantic_State.Reset;
      end if;

      case Data.Parse_Action is
      when Navigate =>
         Data.Navigate_Caches.Finalize;
         Data.Navigate_Caches.Initialize;
         Data.End_Positions.Clear;

      when Face =>
         Data.Face_Caches.Finalize;
         Data.Face_Caches.Initialize;

      when Indent =>
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Data.Indents.Replace_Element (I, (Label => Not_Set));
         end loop;
         Data.Max_Anchor_ID := First_Anchor_ID - 1;
      end case;
   end Reset;

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
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Statement_Param_Array)
   is
      First_Item         : Boolean     := True;
      Override_Start_Set : Boolean     := False;
      Override_Start     : Navigate_Class_Type;
      Containing_Pos     : Nil_Buffer_Pos := Nil; --  wisi first-keyword-pos
   begin
      for Pair of Params loop
         declare
            Token : WisiToken.Semantic_State.Augmented_Token renames Tokens.Constant_Reference (Pair.Index);
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
                         Class          => (if Override_Start_Set then Override_Start else Pair.Class),
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
      Nonterm    : in     Semantic_State.Augmented_Token;
      Tokens     : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Containing : in     Positive_Index_Type;
      Contained  : in     Positive_Index_Type)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-containing-action
      use Navigate_Cache_Trees;
      use WisiToken.Semantic_State;
      Containing_Tok    : Augmented_Token renames Tokens (Containing);
      Containing_Region : Buffer_Region renames Containing_Tok.Char_Region;
      Contained_Tok     : Augmented_Token renames Tokens (Contained);
      Contained_Region  : Buffer_Region renames Contained_Tok.Char_Region;
      Iterator          : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;
      Cursor            : Navigate_Cache_Trees.Cursor;
      Mark              : constant Buffer_Pos                    := Containing_Region.First;
   begin
      if Containing_Region = Null_Buffer_Region then
         --  Either Containing is a virtual token from error recovery, or there
         --  is a bad grammar action. Assume the first.
         return;
      end if;

      if not Data.Navigate_Caches.Present (Containing_Region.First) then
         raise Parse_Error with Error_Message
           (File_Name => -Data.Source_File_Name,
            Line      => Containing_Tok.Line,
            Col       => Containing_Tok.Col,
            Message   => "wisi-containing-action: containing token " &
              Containing_Tok.Image (Data.Semantic_State.Trace.Descriptor.all, ID_Only => True) &
              " has no cache. grammar error; missing action.");
      end if;

      if Contained_Tok.Char_Region /= Null_Buffer_Region then
         --  Contained region is nil in an empty production.
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

   function "+" (Item : in WisiToken.Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List do
         Result.Append (Item);
      end return;
   end "+";

   function "&" (List : in Token_ID_Lists.List; Item : in WisiToken.Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in WisiToken.Token_ID) return Token_ID_Lists.List
   is begin
      return Result : Token_ID_Lists.List do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Motion_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-motion-action
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
         --  [2] wisi-elisp-parse--match-token
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
      for Param of Params loop
         declare
            Token  : Semantic_State.Augmented_Token renames Tokens (Param.Index);
            Region : constant Buffer_Region := Token.Char_Region;
         begin
            if Region /= Null_Buffer_Region then
               if not Start.Set then
                  Start := (True, Region.First);
               end if;

               Cache_Cur := Find (Iter, Ascending, Region.First);
               if not Has_Element (Cache_Cur) then
                  raise Parse_Error with Error_Message
                    (File_Name => -Data.Source_File_Name,
                     Line      => Token.Line,
                     Col       => Token.Col,
                     Message   => "wisi-motion-action: token " &
                       Token.Image (Data.Semantic_State.Trace.Descriptor.all, ID_Only => False) &
                       " has no cache; add to statement-action.");
               end if;

               if Param.IDs.Length = 0 then
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
                     if Match (Param.IDs) then
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
                     exit when Cache_Cur = No_Element;

                     Point := Constant_Ref (Data.Navigate_Caches, Cache_Cur).Element.Pos;
                  end loop;
               end if;
            end if;
         end;
      end loop;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-face-apply-action
      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Semantic_State.Augmented_Token renames Tokens (Param.Index);
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
                              Suf_Cache : Face_Cache_Type renames Variable_Ref
                                (Data.Face_Caches, Suffix_Cur).Element.all;
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
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-face-apply-list-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Semantic_State.Augmented_Token renames Tokens (Param.Index);
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
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Face_Mark_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-face-apply-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         declare
            Token : Semantic_State.Augmented_Token renames Tokens (Param.Index);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find (Iter, Ascending, Token.Char_Region.First);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Variable_Ref (Data.Face_Caches, Cache_Cur).Element.all;
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Region.Last + 1, Token.Char_Region.Last);
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
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Face_Remove_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      --  [2] wisi-face-remove-action
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
      Temp      : Cursor;
   begin
      for I of Params loop
         declare
            Token : Semantic_State.Augmented_Token renames Tokens (I);
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Constant_Ref (Data.Face_Caches, Cache_Cur).Element.Region.First > Token.Char_Region.Last;
                  Temp := Cache_Cur;
                  Cache_Cur := Next (Iter, Cache_Cur);
                  Delete (Data.Face_Caches, Temp);
               end loop;
            end if;
         end;
      end loop;
   end Face_Remove_Action;

   function "+" (Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Item);
      end return;
   end "+";

   function "&" (List : in Indent_Arg_Arrays.Vector; Item : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector := List do
         Result.Append (Item);
      end return;
   end "&";

   function "&" (Left, Right : in Integer) return Indent_Arg_Arrays.Vector
   is begin
      return Result : Indent_Arg_Arrays.Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Params  : in     Indent_Param_Array)
   is
      pragma Unreferenced (Nonterm);
   begin
      --  [2] wisi-indent-action
      for I in Tokens.First_Index .. Tokens.Last_Index loop
         declare
            use all type SAL.Base_Peek_Type;
            Token             : Semantic_State.Augmented_Token renames Tokens (I);
            Pair              : Indent_Pair renames Params (I);
            Code_Delta        : Delta_Type;
            Comment_Param     : Indent_Param;
            Comment_Param_Set : Boolean := False;
            Comment_Delta     : Delta_Type;
         begin
            if Token.Char_Region /= Null_Buffer_Region then
               if Token.First_Indent_Line /= Invalid_Line_Number then
                  Code_Delta := Indent_Compute_Delta (Data, Tokens, Pair.Code_Delta, Token, Indenting_Comment => False);

                  if Code_Delta /= Null_Delta then
                     Indent_Token_1 (Data, Token, Code_Delta, Indenting_Comment => False);
                  end if;
               end if;

               if Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  if Pair.Comment_Present then
                     Comment_Param     := Pair.Comment_Delta;
                     Comment_Param_Set := True;

                  elsif I < Tokens.Last_Index  then
                     Comment_Param     := Params (I + 1).Code_Delta;
                     Comment_Param_Set := True;
                  end if;

                  if Comment_Param_Set then
                     Comment_Delta := Indent_Compute_Delta
                       (Data, Tokens, Comment_Param, Token, Indenting_Comment => True);

                     if Comment_Delta /= Null_Delta then
                        Indent_Token_1 (Data, Token, Comment_Delta, Indenting_Comment => True);
                     end if;
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Indent_Action_0;

   procedure Indent_Action_1
     (Data    : in out Parse_Data_Type'Class;
      Nonterm : in     Semantic_State.Augmented_Token;
      Tokens  : in     Semantic_State.Augmented_Token_Arrays.Vector;
      N       : in     Positive_Index_Type;
      Params  : in     Indent_Param_Array)
   is begin
      --  [2] wisi-indent-action*
      for I in Tokens.First_Index .. N loop
         if Semantic_State.Augmented_Token (Tokens (I).Element.all).First then
            Indent_Action_0 (Data, Nonterm, Tokens, Params);
            return;
         end if;
      end loop;
   end Indent_Action_1;

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tokens            : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Indenting_Token   : in     Semantic_State.Augmented_Token;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Option            : in     Boolean;
      Accumulate        : in     Boolean)
     return Delta_Type
   is begin
      --  [2] wisi-elisp-parse--hanging-1
      if Indenting_Comment then
         return Indent_Compute_Delta (Data, Tokens, (Simple, Delta_1), Indenting_Token, Indenting_Comment);
      else
         return
           (Hanging,
            Hanging_First_Line  => Indenting_Token.Line,
            Hanging_Paren_State => Indenting_Token.Paren_State,
            Hanging_Delta_1     => Indent_Compute_Delta
              (Data, Tokens, (Simple, Delta_1), Indenting_Token, Indenting_Comment).Simple_Delta,
            Hanging_Delta_2     =>
              (if (not Option) or
                 Indenting_Token.Line = Indenting_Token.First_Indent_Line -- first token in tok is first on line
               then Indent_Compute_Delta
                 (Data, Tokens, (Simple, Delta_2), Indenting_Token, Indenting_Comment).Simple_Delta
               else Indent_Compute_Delta
                 (Data, Tokens, (Simple, Delta_1), Indenting_Token, Indenting_Comment).Simple_Delta),
            Hanging_Accumulate => Accumulate);
      end if;
   end Indent_Hanging_1;

   procedure Put (Data : in out Parse_Data_Type)
   is begin
      case Data.Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            Put (Cache);
         end loop;

      when Face =>
         for Cache of Data.Face_Caches loop
            Put (Cache);
         end loop;

      when Indent =>
         --  We don't need "Indent_Leading_Comments"; they are indented to 0,
         --  which is the default.

         Resolve_Anchors (Data);

         --  Can't set indent for first line
         for I in Data.Indents.First_Index + 1 .. Data.Indents.Last_Index loop
            Put (Data, I, Data.Indents (I));
         end loop;
      end case;
   end Put;

   procedure Put
     (Errors     : in WisiToken.Semantic_State.Error_List_Arrays.Vector;
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
                    " ""syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                    ", found '" & Item.Error_Token.Image (Descriptor, ID_Only => True) & "'""]");

               if Item.Recover /= null then
                  Put (WisiToken.LR.Configuration (Item.Recover.all), Descriptor);
               end if;
            end loop;
         end if;
      end loop;
   end Put;

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Message : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (-Data.Source_File_Name, Line_Number, 0, Message) & """)");
   end Put_Error;

   ----------
   --  Spec visible private subprograms, alphabetical

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type;
      Anchor_Token : in Semantic_State.Augmented_Token;
      Offset       : in Integer)
     return Integer
   is
      --  [2] compute delta in wisi-elisp-parse--anchored-1.

      Line_Begin_Pos : constant Buffer_Pos :=
        (if Anchor_Token.First and
           Anchor_Token.First_Indent_Line = Anchor_Token.Line
         then Anchor_Token.Char_Region.First
         else Data.Semantic_State.All_Tokens
           (Data.Semantic_State.Find_Line_Begin (Anchor_Token.Line, Anchor_Token)).Char_Region.First);
   begin
      return Offset + Integer (Anchor_Token.Char_Region.First - Line_Begin_Pos);
   end Current_Indent_Offset;

   function Find_Token_On_Stack (Data : in Parse_Data_Type; ID : in Token_ID) return Semantic_State.Augmented_Token
   is
      use all type SAL.Base_Peek_Type;
      I : Positive_Index_Type := Data.Semantic_State.Stack.Last_Index;
   begin
      loop
         exit when I < Data.Semantic_State.Stack.First_Index;
         exit when Data.Semantic_State.Stack (I).ID = ID;
         I := I - 1;
      end loop;
      if I < Data.Semantic_State.Stack.First_Index then
         raise Programmer_Error with
           "token ID " & Image (ID, Data.Semantic_State.Trace.Descriptor.all) & " not found on parse stack";
      end if;
      return Semantic_State.Augmented_Token (Data.Semantic_State.Stack (I).Element.all);
   end Find_Token_On_Stack;

   function Indent_Anchored_2
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     Line_Number_Type;
      Last_Line   : in     Line_Number_Type;
      Offset      : in     Integer;
      Accumulate  : in     Boolean)
     return Delta_Type
   is
      use Anchor_ID_Vectors;
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent    : Indent_Type      := Data.Indents (Anchor_Line);
      Anchor_ID : constant Integer := 1 + Max_Anchor_ID (Data, Anchor_Line, Last_Line);
   begin
      --  [2] wisi-elisp-parse--anchored-2
      Data.Max_Anchor_ID := Integer'Max (Data.Max_Anchor_ID, Anchor_ID);

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchor, To_Vector (Anchor_ID, 1), 0);

      when Int =>
         Indent := (Anchor, To_Vector (Anchor_ID, 1), Indent.Int_Indent);

      when Anchor =>
         Indent.Anchor_IDs := Anchor_ID & Indent.Anchor_IDs;

      when Anchored =>
         Indent := (Anchor_Anchored, To_Vector (Anchor_ID, 1), Indent.Anchored_ID, Indent.Anchored_Delta);

      when Anchor_Anchored =>
         Indent.Anchor_Anchored_IDs := Anchor_ID & Indent.Anchor_Anchored_IDs;
      end case;

      Data.Indents.Replace_Element (Anchor_Line, Indent);

      return (Simple, (Anchored, Anchor_ID, Offset, Accumulate));
   end Indent_Anchored_2;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tokens            : in     Semantic_State.Augmented_Token_Arrays.Vector;
      Param             : in     Indent_Param;
      Indenting_Token   : in     Semantic_State.Augmented_Token;
      Indenting_Comment : in     Boolean)
     return Delta_Type
   is begin
      --  [2] wisi-elisp-parse--indent-compute-delta, which evals wisi-anchored*, wisi-hanging*.
      case Param.Label is
      when Simple =>
         case Param.Param.Label is
         when Int =>
            return (Simple, (Int, Param.Param.Int_Delta));

         when Anchored_Label =>
            declare
               Anchor_Token : Semantic_State.Augmented_Token renames Tokens (Param.Param.Anchored_Index);
            begin
               if Indenting_Token.Byte_Region = Null_Buffer_Region or
                 Anchor_Token.Byte_Region = Null_Buffer_Region
               then
                  --  One of these is a virtual token
                  return Null_Delta;
               else
                  case Anchored_Label'(Param.Param.Label) is
                  when Anchored_0 =>
                     --  [2] wisi-anchored, wisi-anchored-1
                     return Indent_Anchored_2
                       (Data,
                        Anchor_Line => Anchor_Token.Line,
                        Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                        Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                        Accumulate  => True);

                  when Anchored_1 =>
                     --  [2] wisi-anchored%
                     return Indent_Anchored_2
                       (Data,
                        Anchor_Line => Anchor_Token.Line,
                        Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                        Offset      => Paren_In_Anchor_Line (Data, Anchor_Token, Param.Param.Anchored_Delta),
                        Accumulate  => True);

                  when Anchored_2 =>
                     --  [2] wisi-anchored%-
                     return Indent_Anchored_2
                       (Data,
                        Anchor_Line => Anchor_Token.Line,
                        Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                        Offset      => Paren_In_Anchor_Line (Data, Anchor_Token, Param.Param.Anchored_Delta),
                        Accumulate  => False);

                  when Anchored_3 =>
                     --  [2] wisi-anchored*
                     if Indenting_Token.First then
                        return Indent_Anchored_2
                          (Data,
                           Anchor_Line => Anchor_Token.Line,
                           Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                           Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                           Accumulate  => True);

                     else
                        return Null_Delta;
                     end if;

                  when Anchored_4 =>
                     --  [2] wisi-anchored*-
                     return Indent_Anchored_2
                       (Data,
                        Anchor_Line => Anchor_Token.Line,
                        Last_Line   => Indenting_Token.Last_Line (Indenting_Comment),
                        Offset      => Current_Indent_Offset (Data, Anchor_Token, Param.Param.Anchored_Delta),
                        Accumulate  => False);

                  end case;
               end if;
            end;

         when Language =>
            return Param.Param.Function_Ptr (Data, Tokens, Indenting_Token, Indenting_Comment, Param.Param.Args);
         end case;

      when Hanging_Label =>
         case Hanging_Label'(Param.Label) is
         when Hanging_0 => -- wisi-hanging
            return Indent_Hanging_1
              (Data, Tokens, Indenting_Token, Indenting_Comment, Param.Hanging_Delta_1, Param.Hanging_Delta_2,
               Option => False, Accumulate => True);
         when Hanging_1 => -- wisi-hanging%
            return Indent_Hanging_1
              (Data, Tokens, Indenting_Token, Indenting_Comment, Param.Hanging_Delta_1, Param.Hanging_Delta_2,
               Option => True, Accumulate => True);
         when Hanging_2 => -- wisi-hanging%-
            return Indent_Hanging_1
              (Data, Tokens, Indenting_Token, Indenting_Comment, Param.Hanging_Delta_1, Param.Hanging_Delta_2,
               Option => True, Accumulate => False);
         end case;
      end case;
   end Indent_Compute_Delta;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Indenting_Token   : in     Semantic_State.Augmented_Token;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean)
   is
      First_Line : constant Line_Number_Type := Indenting_Token.First_Line (Indenting_Comment);
      Last_Line  : constant Line_Number_Type := Indenting_Token.Last_Line (Indenting_Comment);
   begin
      for Line in First_Line .. Last_Line loop
         if Data.Indent_Comment_Col_0 then
            declare
               use all type Ada.Text_IO.Count;

               I : constant Positive_Index_Type := Data.Semantic_State.Find_Line_Begin (Line, Indenting_Token);
            begin
               if Data.Semantic_State.All_Tokens (I).ID = Data.Semantic_State.Trace.Descriptor.Comment_ID and
                 Data.Semantic_State.All_Tokens (I).Col = 0
               then
                  null;
               else
                  Indent_Line (Data, Line, Delta_Indent);
               end if;
            end;
         else
            Indent_Line (Data, Line, Delta_Indent);
         end if;
      end loop;
   end Indent_Token_1;

end WisiToken.Wisi_Runtime;
