--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2021 Free Software Foundation, Inc.
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

with Ada.Exceptions;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL;
with WisiToken.In_Parse_Actions;
package body Wisi is
   use WisiToken;

   Chars_Per_Int : constant Integer := Integer'Width;

   ----------
   --  body subprograms bodies, alphabetical

   function Image (Indent : in Indent_Type) return String
   is
      Prefix : constant String := "(" & Trimmed_Image (Indent.Controlling_Token_Line) & ": " &
        Indent_Label'Image (Indent.Label);
   begin
      case Indent.Label is
      when Not_Set =>
         return Prefix & ")";

      when Int =>
         return Prefix & Integer'Image (Indent.Int_Indent) & ")";

      when Anchored =>
         return Prefix & "," & Indent.Anchor_Line'Image & "," & Indent.Anchor_Delta'Image & ")";

      end case;
   end Image;

   procedure Indent_Apply_Anchored
     (Delta_Indent : in     Simple_Delta_Type;
      Indent       : in out Indent_Type)
   with Pre => Delta_Indent.Label = Anchored
   is begin
      case Indent.Label is
      when Not_Set =>
         Indent :=
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchor_Line, Delta_Indent.Anchored_Delta);

      when Int =>
         Indent :=
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchor_Line,
            Delta_Indent.Anchored_Delta + Indent.Int_Indent);

      when Anchored =>
         --  Already anchored, as in nested parens.
         null;
      end case;
   end Indent_Apply_Anchored;

   procedure Indent_Apply_Int
     (Indent                 : in out Indent_Type;
      Offset                 : in     Integer;
      Controlling_Token_Line : in     Base_Line_Number_Type)
   is begin
      --  Add an Int indent to Indent
      case Indent.Label is
      when Not_Set =>
         Indent := (Int, Controlling_Token_Line, Offset);

      when Int =>
         if Controlling_Token_Line = Invalid_Line_Number or
           Indent.Controlling_Token_Line = Invalid_Line_Number or
           Controlling_Token_Line /= Indent.Controlling_Token_Line
         then
            Indent.Controlling_Token_Line := Controlling_Token_Line;
            Indent.Int_Indent := Indent.Int_Indent + Offset;
         end if;

      when Anchored =>
         null;
      end case;
   end Indent_Apply_Int;

   procedure Indent_Line
     (Data         : in out Parse_Data_Type;
      Line         : in     Line_Number_Type;
      Delta_Indent : in     Delta_Type)
   is
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent : Indent_Type := Data.Indents (Line);
   begin
      case Delta_Indent.Label is
      when Simple =>
         case Delta_Indent.Simple_Delta.Label is
         when None =>
            null;

         when Int =>
            Indent_Apply_Int
              (Indent, Delta_Indent.Simple_Delta.Int_Delta, Delta_Indent.Simple_Delta.Controlling_Token_Line);

         when Anchored =>
            Indent_Apply_Anchored (Delta_Indent.Simple_Delta, Indent);
         end case;

      when Hanging =>
         if Line = Delta_Indent.Hanging_First_Line then
            --  Apply delta_1
            case Delta_Indent.Hanging_Delta_1.Label is
            when None =>
               null;
            when Int =>
               Indent_Apply_Int
                 (Indent, Delta_Indent.Hanging_Delta_1.Int_Delta, Delta_Indent.Hanging_Delta_1.Controlling_Token_Line);
            when Anchored =>
               Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_1, Indent);
            end case;
         else
            --  Apply delta_2
            case Delta_Indent.Hanging_Delta_2.Label is
            when None =>
               null;
            when Int =>
               Indent_Apply_Int
                 (Indent, Delta_Indent.Hanging_Delta_2.Int_Delta,
                  Delta_Indent.Hanging_Delta_2.Controlling_Token_Line);
            when Anchored =>
               Indent_Apply_Anchored (Delta_Indent.Hanging_Delta_2, Indent);
            end case;
         end if;
      end case;

      if Trace_Action > Extra then
         Data.Trace.Put_Line ("indent_line: " & Line_Number_Type'Image (Line) & " => " & Image (Indent));
      end if;

      Data.Indents.Replace_Element (Line, Indent);
   end Indent_Line;

   function Paren_In_Anchor_Line
     (Data         : in out Parse_Data_Type'Class;
      Tree         : in     WisiToken.Syntax_Trees.Tree;
      Anchor_Token : in     Syntax_Trees.Valid_Node_Access;
      Offset       : in     Integer)
     return Integer
   --  If there is a left_paren in Anchor_Token.Line_Region.First
   --  containing Anchor_Token, return offset of that paren from first
   --  char in line + Offset. Else return Offset.
   is
      Left_Paren_ID  : Token_ID renames Data.Left_Paren_ID;
      Right_Paren_ID : Token_ID renames Data.Right_Paren_ID;

      Begin_Token : constant Syntax_Trees.Valid_Node_Access := Tree.Line_Begin_Token
        (Tree.Line_Region (Anchor_Token).First);

      I : Syntax_Trees.Node_Access := Tree.First_Terminal (Anchor_Token);

      Paren_Count    : Integer    := 0;
      Paren_Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      Text_Begin_Pos : Buffer_Pos := Invalid_Buffer_Pos;
   begin
      loop
         declare
            Tok_Char_Region : constant Buffer_Region := Tree.Char_Region (I);
         begin
            if Tree.ID (I) = Left_Paren_ID then
               Paren_Count := Paren_Count + 1;
               if Paren_Count = 1 then
                  Paren_Char_Pos := Tok_Char_Region.First;
               end if;

            elsif Tree.ID (I) = Right_Paren_ID then
               Paren_Count := Paren_Count - 1;

            end if;

            if I = Begin_Token then
               Text_Begin_Pos := Tok_Char_Region.First;
               exit;
            end if;
         end;

         I := Tree.Prev_Terminal (I);
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

   procedure Put (Cache : in WisiToken.Buffer_Region)
   is begin
      Ada.Text_IO.Put_Line
        ("[" & Name_Property_Code & Buffer_Pos'Image (Cache.First) & Buffer_Pos'Image (Cache.Last) & "]");
   end Put;

   procedure Put (Cache : in Face_Cache_Type)
   is
      package Bounded is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 2 + 4 * Chars_Per_Int);
      use Bounded;

      Line : Bounded_String := To_Bounded_String ("[");
   begin
      if Cache.Face.Set then
         Append (Line, Face_Property_Code);
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.First));
         Append (Line, Buffer_Pos'Image (Cache.Char_Region.Last));
         Append (Line, Integer'Image (Cache.Face.Item));
         Append (Line, ']');
         Ada.Text_IO.Put_Line (To_String (Line));
      end if;
   end Put;

   procedure Put
     (Tree : in Syntax_Trees.Tree;
      Line : in Line_Number_Type;
      Item : in Indent_Type)
   is begin
      --  All Anchors must be resolved at this point, but not all lines have
      --  an indent computed. A negative indent is an error in either the
      --  grammar indent rules or the algorithms in this package.
      case Item.Label is
      when Not_Set =>
         --  Especially with partial parse, we have no idea what this indent should be.
         null;

      when Int =>
         declare
            --  We can easily get negative indents when there are syntax errors.
            Ind : constant Integer := Integer'Max (0, Item.Int_Indent);
            Line_Begin_Char_Pos : Base_Buffer_Pos :=
              (if Line = Line_Number_Type'First
               then Buffer_Pos'First
               else Base_Buffer_Pos'Last);
            Node : constant Syntax_Trees.Node_Access :=
              (if Line = Line_Number_Type'First
               then Syntax_Trees.Invalid_Node_Access
               else Tree.Find_New_Line (Line, Line_Begin_Char_Pos));
            pragma Unreferenced (Node);
         begin
            if Debug_Mode and Ind > 100 then
               --  This is better than hanging Emacs by returning a huge bogus indent.
               raise SAL.Programmer_Error with "indent > 100";
            end if;
            Ada.Text_IO.Put_Line
              --  elisp doesn't need line number, but it is very helpful for debugging
              ('[' & Indent_Code & Line'Image & Line_Begin_Char_Pos'Image & Ind'Image & ']');
         end;

      when Anchored =>
         raise SAL.Programmer_Error with "Indent item has non-int label: " & Indent_Label'Image (Item.Label);
      end case;
   end Put;

   procedure Put
     (Item : in Parse.LR.Recover_Op_Arrays.Vector;
      Data : in Parse_Data_Type;
      Tree : in Syntax_Trees.Tree)
   is
      use Ada.Strings.Unbounded;
      use Parse.LR;
      use Parse.LR.Recover_Op_Arrays;
      use all type Ada.Containers.Count_Type;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      --  Output is a sequence of edit regions; each is:
      --  [error-pos edit-pos [inserted token-ids] [deleted token-ids] deleted-region]

      type State_Label is
        (None,     -- not started yet
         Inserted, -- edit-pos, some insert ids appended
         Deleted); -- some delete ids appended

      State : State_Label := None;
      --  State of the current edit region.

      Last_Edit_Pos  : Buffer_Pos          := Invalid_Buffer_Pos;
      Line           : Unbounded_String    := To_Unbounded_String ("[");
      Deleted_Region : Buffer_Region       := Null_Buffer_Region;
      Last_Deleted   : Recover_Op (Delete) :=
        (Delete, Invalid_Buffer_Pos, Invalid_Token_ID, Syntax_Trees.Sequential_Index'Last,
         Syntax_Trees.Invalid_Node_Access, Syntax_Trees.Invalid_Node_Access);

      procedure Start_Edit_Region (Error_Pos, Edit_Pos : in Buffer_Pos)
      is begin
         Append (Line, "[");
         Append (Line, Trimmed_Image (Error_Pos));
         Append (Line, Edit_Pos'Image);
         Append (Line, "[");
      end Start_Edit_Region;

      procedure Terminate_Edit_Region
      is begin
         case State is
         when None =>
            null;
         when Inserted =>
            Append (Line, "][]" & Image (Deleted_Region) & "]");
         when Deleted =>
            --  Emacs (cdr (region)) is after last char to be deleted.
            Append
              (Line, "]" & "(" & Trimmed_Image (Integer (Deleted_Region.First)) & " ." &
                 Buffer_Pos'Image (Deleted_Region.Last + 1) & ")" & "]");
         end case;
         Deleted_Region := Null_Buffer_Region;
      end Terminate_Edit_Region;
   begin
      if Trace_Action > Outline then
         Data.Trace.Put_Line ("recover: " & Parse.LR.Image (Item, Tree));
      end if;

      if Length (Item) = 0 or not Tree.Parents_Set then
         --  Parents not set due to failed recover.
         return;
      end if;

      Append (Line, Recover_Code);
      for I in First_Index (Item) .. Last_Index (Item) loop
         declare
            use WisiToken.Syntax_Trees;

            Op : constant Recover_Op := Element (Item, I);

            Edit_Pos_Node : constant Node_Access :=
              --  Can be Invalid_Node_Access when recover fails.
              (case Op.Op is
               when Insert =>
                 (if Op.Ins_Node = Invalid_Node_Access
                  then Invalid_Node_Access
                  else Tree.First_Source_Terminal (Op.Ins_Node, Trailing_Non_Grammar => True, Following => True)),

               when Delete => Op.Del_Node);

            Edit_Pos : constant Buffer_Pos :=
              (if Edit_Pos_Node = Invalid_Node_Access
               then Invalid_Buffer_Pos
               else Tree.Char_Region (Edit_Pos_Node, Trailing_Non_Grammar => True).First);
         begin
            if Last_Edit_Pos = Invalid_Buffer_Pos then
               Last_Edit_Pos := Edit_Pos;

            elsif Edit_Pos /= Last_Edit_Pos then
               Terminate_Edit_Region;
               State         := None;
               Last_Edit_Pos := Edit_Pos;
            end if;

            case Op.Op is
            when Insert =>
               case State is
               when None =>
                  Start_Edit_Region (Op.Error_Pos, Edit_Pos);

               when Inserted =>
                  null;

               when Deleted =>
                  Terminate_Edit_Region;
                  Start_Edit_Region (Op.Error_Pos, Edit_Pos);

               end case;
               Append (Line, Token_ID'Image (Op.Ins_ID));
               State := Inserted;

            when Delete =>
               Deleted_Region := Deleted_Region and Tree.Char_Region (Op.Del_Node);
               declare
                  Skip : Boolean := False;
               begin
                  case State is
                  when None =>
                     Start_Edit_Region (Op.Error_Pos, Edit_Pos);
                     Append (Line, "][");

                  when Inserted =>
                     Append (Line, "][");

                  when Deleted =>
                     if Data.Embedded_Quote_Escape_Doubled and then
                       ((Last_Deleted.Del_ID = Descriptor.String_1_ID and
                           Op.Del_ID = Descriptor.String_1_ID) or
                          (Last_Deleted.Del_ID = Descriptor.String_2_ID and
                             Op.Del_ID = Descriptor.String_2_ID))
                     then
                        declare
                           Tok_1_Char_Region : constant Buffer_Region := Tree.Char_Region (Last_Deleted.Del_Node);
                           Tok_2_Char_Region : constant Buffer_Region := Tree.Char_Region (Op.Del_Node);
                        begin
                           if Tok_1_Char_Region.Last + 1 = Tok_2_Char_Region.First then
                              --  Buffer text was '"""', lexer repair changed it to '""""'. The
                              --  repaired text looks like a single string with an embedded quote.
                              --  But here, it is two STRING_LITERAL tokens. Don't send the second
                              --  delete to elisp. See test/ada_mode-recover_string_quote_1.adb
                              Skip := True;
                           end if;
                        end;
                     end if;
                  end case;
                  State := Deleted;

                  if not Skip then
                     Append (Line, Token_ID'Image (Op.Del_ID));
                  end if;
               end;
               Last_Deleted := Op;
            end case;
         end;
      end loop;

      case State is
      when None =>
         null;
      when Inserted | Deleted =>
         Terminate_Edit_Region;
      end case;
      Append (Line, "]");
      Ada.Text_IO.Put_Line (To_String (Line));
   end Put;

   procedure Resolve_Anchors
     (Data : in out Parse_Data_Type;
      Tree : in     Syntax_Trees.Tree)
   is
      Begin_Indent : Integer renames Data.Begin_Indent;

      SOI_Lines : constant Line_Region := Tree.Line_Region (Tree.SOI, Trailing_Non_Grammar => True);
      EOI_Lines : constant Line_Region := Tree.Line_Region (Tree.EOI, Trailing_Non_Grammar => True);

      function Anchor_Indent (Line : in Line_Number_Type) return Integer
      is
         Item : Indent_Type renames Data.Indents (Line);
      begin
         case Item.Label is
         when Not_Set | Anchored =>
            raise SAL.Programmer_Error with "non-int anchor indent line" & Line'Image;

         when Int =>
            return Item.Int_Indent;
         end case;
      end Anchor_Indent;

   begin
      if Trace_Action > Outline then
         Data.Trace.New_Line;
         Data.Trace.Put_Line ("Begin_Indent: " & Integer'Image (Data.Begin_Indent));
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Data.Trace.Put_Line (Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
         Data.Trace.Put_Line ("resolve anchors");
      end if;

      for Line in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         declare
            Indent : constant Indent_Type := Data.Indents (Line);
         begin
            case Indent.Label is
            when Not_Set =>
               if Contains (SOI_Lines, Line) or Contains (EOI_Lines, Line) then
                  Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Data.Begin_Indent));
               end if;

            when Int =>
               Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Indent.Int_Indent + Begin_Indent));

            when Anchored =>
               Data.Indents.Replace_Element
                 (Line, (Int, Invalid_Line_Number, Anchor_Indent (Indent.Anchor_Line) + Indent.Anchor_Delta));

            end case;
         end;
      end loop;

      if Trace_Action > Outline then
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            if I in Data.Action_Region_Lines.First .. Data.Action_Region_Lines.Last then
               Data.Trace.Put_Line (Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
            end if;
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
               if Trace_Action > Detail then
                  Data.Trace.Put_Line ("   " & Cache.Pos'Image & " end to " & Cache.End_Pos.Item'Image);
               end if;
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

   procedure Skip
     (Source : in     String;
      Last   : in out Integer;
      Char   : in     Character)
   is begin
      loop
         if Last = Source'Last then
            raise Protocol_Error with "at" & Last'Image & ": expecting '" & Char & "' found EOI";

         elsif Source (Last + 1) = ' ' then
            Last := Last + 1;

         elsif Source (Last + 1) = Char then
            Last := Last + 1;
            exit;
         else
            raise Protocol_Error with
              "at" & Last'Image & ": expecting '" & Char & "' found '" & Source (Last + 1) & "'";
         end if;
      end loop;
   end Skip;

   function Get_String
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Index
        (Source  => Source,
         Pattern => """",
         From    => Last + 1);
      Temp : Integer := First + 1;
   begin
      loop
         Last := Index
           (Source  => Source,
            Pattern => """",
            From    => Temp);
         exit when Last = 0;
         exit when Source (Last - 1) /= '\';
         Temp := Last + 1;
      end loop;

      if First = 0 or Last = 0 then
         raise Protocol_Error with "at" & Last'Image & ": no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

   function Get_Enum
     (Source : in     String;
      Last   : in out Integer)
     return String
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 1;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First + 1); -- Skip a leading space if present.

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;
      return Source (First .. Last);
   end Get_Enum;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 1;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First + 1); -- Skip a leading space if present.

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when others =>
      raise Protocol_Error with "at" & First'Image & ": bad integer '" & Source (First .. Last) & "'";
   end Get_Integer;

   function Image (Item : in Change) return String
   is begin
      return "(" &
        Item.Begin_Byte_Pos'Image & "," &
        Item.Begin_Char_Pos'Image & "," &
        Item.Inserted_End_Byte_Pos'Image & "," &
        Item.Inserted_End_Char_Pos'Image & "," &
        " +""" & (-Item.Inserted_Text) & """," &
        Item.Deleted_Bytes'Image & "," &
        Item.Deleted_Chars'Image & ")";
   end Image;

   function Get_Emacs_Change_List
     (Command_Line : in     String;
      Last         : in out Integer)
     return Change_Lists.List
   is
      function Substitute_Escapes (Item : in String) return String
      is begin
         if Item'Length = 0 then
            return Item;
         else
            declare
               I      : Integer := Item'First;
               J      : Integer := Item'First;
               Result : String (Item'Range);
            begin
               loop
                  if Item (I) = '\' and I < Item'Last then
                     if Item (I + 1) = 'n' then
                        Result (J) := ASCII.LF;
                        I := @ + 2;
                     elsif Item (I + 1) = '"' then
                        Result (J) := '"';
                        I := @ + 2;
                     else
                        Result (J) := Item (I);
                        I := @ + 1;
                     end if;
                  else
                     Result (J) := Item (I);
                     I := @ + 1;
                  end if;
                  exit when I > Item'Last;
                  J := @ + 1;
               end loop;
               return Result (Result'First .. J);
            end;
         end if;
      end Substitute_Escapes;

   begin
      return Result : Change_Lists.List do
         Skip (Command_Line, Last, '('); --  start of changes list
         loop
            exit when Last = Command_Line'Last;
            exit when Command_Line (Last + 1) = ')';

            declare
               Item : Change;
            begin
               Skip (Command_Line, Last, '(');
               Item.Begin_Byte_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Begin_Char_Pos        := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Byte_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Inserted_End_Char_Pos := Base_Buffer_Pos (Get_Integer (Command_Line, Last));
               Item.Deleted_Bytes         := Get_Integer (Command_Line, Last);
               Item.Deleted_Chars         := Get_Integer (Command_Line, Last);
               Item.Inserted_Text         := +Substitute_Escapes (Get_String (Command_Line, Last));
               Skip (Command_Line, Last, ')');

               Result.Append (Item);
            end;
         end loop;
         Skip (Command_Line, Last, ')'); --  end of edits list
      end return;
   end Get_Emacs_Change_List;

   function To_Unix_Line_Endings (Source : in out String) return Integer
   --  Return count of line endings converted.
   is
      Read              : Integer := Source'First;
      Write             : Integer := Source'First - 1;
      Line_Ending_Count : Integer := 0;
   begin
      loop
         exit when Read > Source'Last;
         if Source (Read) = ASCII.CR and (Read < Source'Last and then Source (Read + 1) = ASCII.LF) then
            Write             := @ + 1;
            Source (Write)    := ASCII.LF;
            Read              := @ + 2;
            Line_Ending_Count := @ + 1;
         else
            Write          := @ + 1;
            Source (Write) := Source (Read);
            Read           := @ + 1;
         end if;
      end loop;
      return Line_Ending_Count;
   end To_Unix_Line_Endings;

   procedure To_Unix_Line_Endings
     (Source           : in     Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer)
   is
      Line_End_Count : constant Integer := To_Unix_Line_Endings (Source (Source'First .. Source_Byte_Last));
   begin
      Source_Byte_Last := @ - Line_End_Count;
      Source_Char_Last := @ - Line_End_Count;
   end To_Unix_Line_Endings;

   procedure Edit_Source
     (Trace            : in out WisiToken.Trace'Class;
      Source           : in out Ada.Strings.Unbounded.String_Access;
      Source_Byte_Last : in out Integer;
      Source_Char_Last : in out Integer;
      Changes          : in     Change_Lists.List;
      KMN_List         :    out WisiToken.Parse.KMN_Lists.List)
   is
      use Ada.Containers;

      --  Changes is in time order (ie _not_ in buffer pos order); KMN_List
      --  is in buffer pos order.

      Initial_Text_Byte_Region : constant Buffer_Region := (1, Base_Buffer_Pos (Source_Byte_Last));
      Initial_Text_Char_Region : constant Buffer_Region := (1, Base_Buffer_Pos (Source_Char_Last));

      Gap_First : Integer := Source_Byte_Last + 1;
      Gap_Last  : Integer := Source'Last;

      function Gap_Invariant return Boolean
      is (Gap_Last - Gap_First = Source'Last - (Source_Byte_Last + 1));

      Total_Inserted_Bytes : Integer := 0;

      function Reallocate return Boolean
      is
         Last_Begin : Base_Buffer_Pos := 0;
         Result     : Boolean         := False;
      begin
         if Changes.Length = 0 then
            return False;
         end if;

         for Change of Changes loop
            --  We loop thru all changes to compute Total_Inserted_Bytes.

            pragma Assert
              (Ada.Strings.Unbounded.Length (Change.Inserted_Text) =
                 Integer (Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos),
               "inconsistent Change: text length" & Ada.Strings.Unbounded.Length (Change.Inserted_Text)'Image &
                 " region length" & Integer (Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos)'Image);

            Total_Inserted_Bytes := @ + Ada.Strings.Unbounded.Length (Change.Inserted_Text);

            if Change.Begin_Byte_Pos < Last_Begin then
               Result := True;
            end if;
            Last_Begin := Change.Begin_Byte_Pos;
         end loop;

         if Source_Byte_Last + Total_Inserted_Bytes > Source'Last then
            return True;
         else
            return Result;
         end if;
      end Reallocate;

      procedure Move_Gap (New_Gap_First : in Integer)
      with Pre => New_Gap_First /= Gap_First and Gap_Invariant,
        Post => Gap_Invariant
      is
         --  Examples:
         --  gap_first : 15
         --  gap_last  : 19
         --
         --  new_gap_first: 5
         --     new_gap_last := 9
         --     source (10 .. 19) := source (5 .. 14)
         --
         --  new_gap_first: 25
         --  new_gap_last : 29
         --      source (15 .. 24) := source (20 .. 29)

         New_Gap_Last : constant Integer := New_Gap_First + Gap_Last - Gap_First;
      begin
         if New_Gap_First < Gap_First then
            Source (New_Gap_Last + 1 .. Gap_Last) := Source (New_Gap_First .. Gap_First - 1);
         else
            Source (Gap_First .. New_Gap_First - 1) := Source (Gap_Last + 1 .. New_Gap_Last);
         end if;

         Gap_First := New_Gap_First;
         Gap_Last  := New_Gap_Last;
      end Move_Gap;

      procedure Edit_Text (Change : in Wisi.Change)
      with Pre => Gap_Invariant, Post => Gap_Invariant
      --  Apply Change to Source. Leaves Gap at edit point.
      is
         use Ada.Strings.Unbounded;
         Inserted_Bytes : constant Integer := Integer (Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos);
      begin
         if Gap_First /= Integer (Change.Begin_Byte_Pos) then
            Move_Gap (Integer (Change.Begin_Byte_Pos));
         end if;

         if Change.Deleted_Bytes > 0 then
            Gap_Last         := @ + Change.Deleted_Bytes;
            pragma Assert (Gap_Last <= Source'Last);
            Source_Byte_Last := @ - Change.Deleted_Bytes;
            Source_Char_Last := @ - Change.Deleted_Chars;
         end if;

         if Inserted_Bytes > 0 then
            pragma Assert (Gap_Last + 1 - Gap_First >= Inserted_Bytes);
            Source (Gap_First .. Gap_First + Inserted_Bytes - 1) := -Change.Inserted_Text;

            Gap_First        := Gap_First + Inserted_Bytes;
            Source_Byte_Last := @ + Inserted_Bytes;
            Source_Char_Last := @ + Integer (Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos);
         end if;
      end Edit_Text;

      procedure Delete_KMNs
        (KMN_Last_Byte     : in     Zero_Buffer_Pos;
         KMN_Last_Char     : in     Zero_Buffer_Pos;
         After             : in     Parse.KMN_Lists.Cursor;
         Last_Deleted_Byte : in     Buffer_Pos;
         Last_Deleted_Char : in     Buffer_Pos;
         KMN               : in out Parse.KMN)
      --  Last_Deleted_Byte is deleted from current text by current
      --  Change. Delete KMNs after After whose Stable are entirely within
      --  Last_Deleted_Byte; merge into KMN (initially the current Change).
      --  Adjust following KMN if stable contains Last_Deleted_Byte.
      is
         use Parse.KMN_Lists;
         use all type Parse.KMN;

         Last_Byte : Zero_Buffer_Pos := KMN_Last_Byte + KMN_List (After).Stable_Bytes +
           KMN_List (After).Inserted_Bytes; -- end of After KMN and subsequent deleted KMN
         Last_Char : Zero_Buffer_Pos := KMN_Last_Char + KMN_List (After).Stable_Chars +
           KMN_List (After).Inserted_Chars;

         Cur : Cursor := Next (After);
      begin
         loop
            exit when not Has_Element (Cur);
            if Last_Byte + KMN_List (Cur).Stable_Bytes + KMN_List (Cur).Inserted_Bytes <=
              Last_Deleted_Byte
            then
               --  All of cur inserted are deleted, and some of next.
               --  test_edit_source.adb Edit_06.
               KMN.Deleted_Bytes := @ + KMN_List (Cur).Deleted_Bytes - KMN_List (Cur).Inserted_Bytes;
               KMN.Deleted_Chars := @ + KMN_List (Cur).Deleted_Chars - KMN_List (Cur).Inserted_Chars;

               Last_Byte := @ + KMN_List (Cur).Stable_Bytes + KMN_List (Cur).Inserted_Bytes;
               Last_Char := @ + KMN_List (Cur).Stable_Chars + KMN_List (Cur).Inserted_Chars;

               declare
                  To_Delete : Cursor := Cur;
               begin
                  Cur := Next (Cur);
                  KMN_List.Delete (To_Delete);
               end;

            elsif Last_Byte + KMN_List (Cur).Stable_Bytes <= Last_Deleted_Byte then
               --  Some of Cur.inserted are deleted. test_edit_source.adb Edit_05.
               declare
                  Deleted_Bytes : constant Zero_Buffer_Pos := Last_Deleted_Byte -
                    (Last_Byte + KMN_List (Cur).Stable_Bytes); -- bytes of cur.inserted that are deleted
                  Deleted_Chars : constant Zero_Buffer_Pos := Last_Deleted_Char -
                    (Last_Char + KMN_List (Cur).Stable_Chars);
               begin
                  KMN.Inserted_Bytes := @ - Deleted_Bytes + KMN_List (Cur).Inserted_Bytes;
                  KMN.Inserted_Chars := @ - Deleted_Chars + KMN_List (Cur).Inserted_Chars;

                  KMN.Deleted_Bytes := @ + KMN_List (Cur).Deleted_Bytes - Deleted_Bytes;
                  KMN.Deleted_Chars := @ + KMN_List (Cur).Deleted_Chars - Deleted_Chars;

                  KMN_List.Delete (Cur);
                  exit;
               end;
            else
               --  Last_Byte is in Cur.stable
               KMN_List (Cur).Stable_Bytes := @ - (Last_Deleted_Byte - Last_Byte);
               KMN_List (Cur).Stable_Chars := @ - (Last_Deleted_Char - Last_Char);

               if KMN_List (Cur) = (others => 0) then
                  KMN_List.Delete (Cur);
               end if;
               exit;
            end if;
         end loop;
      end Delete_KMNs;

      procedure Edit_KMN (Change : in Wisi.Change)
      --  Apply Change to KMN list
      is
         use Parse.KMN_Lists;
         use all type Parse.KMN;

         Cur : Cursor := KMN_List.First;

         KMN_Last_Byte : Base_Buffer_Pos := 0; --  Last byte of prev KMN.
         KMN_Last_Char : Base_Buffer_Pos := 0; --  Last char of prev KMN.

         function To_KMN (Item : in Wisi.Change) return Parse.KMN
         --  Assuming Change does not overlap any current KMN non-stable,
         --  return a new KMN for it.
         is (Stable_Bytes   => Item.Begin_Byte_Pos - KMN_Last_Byte - 1, -- Begin_Byte_Pos is deleted or inserted
             Stable_Chars   => Item.Begin_Char_Pos - KMN_Last_Char - 1,
             Inserted_Bytes => Item.Inserted_End_Byte_Pos - Item.Begin_Byte_Pos, -- End_Byte_Pos is after last inserted
             Inserted_Chars => Item.Inserted_End_Char_Pos - Item.Begin_Char_Pos,
             Deleted_Bytes  => Base_Buffer_Pos (Item.Deleted_Bytes),
             Deleted_Chars  => Base_Buffer_Pos (Item.Deleted_Chars));

      begin
         loop
            declare
               Cur_KMN : Parse.KMN renames KMN_List (Cur);
               KMN     : Parse.KMN := To_KMN (Change);

               Cur_Last_Inserted_Byte : constant Base_Buffer_Pos :=
                 KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
               Cur_Last_Inserted_Char : constant Base_Buffer_Pos :=
                 KMN_Last_Char + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

               Change_Last_Deleted_Byte : constant Base_Buffer_Pos :=
                 Change.Begin_Byte_Pos + Base_Buffer_Pos (Change.Deleted_Bytes) - 1;

               Change_Last_Deleted_Char : constant Base_Buffer_Pos :=
                 Change.Begin_Char_Pos + Base_Buffer_Pos (Change.Deleted_Chars) - 1;
            begin
               pragma Assert (KMN_Last_Byte < Change.Begin_Byte_Pos);

               if Change.Begin_Byte_Pos + Base_Buffer_Pos (Change.Deleted_Bytes) - 1 <
                 KMN_Last_Byte + Cur_KMN.Stable_Bytes
               then
                  --  Change is entirely within Cur_KMN.Stable_Bytes;
                  --  test_edit_source.adb Edit_01
                  --
                  --  Or Change is inserting at end of text; Edit_10.
                  Cur_KMN.Stable_Bytes := @ - (KMN.Stable_Bytes + KMN.Deleted_Bytes);
                  Cur_KMN.Stable_Chars := @ - (KMN.Stable_Chars + KMN.Deleted_Chars);

                  if KMN_List (Cur) = (others => 0) then
                     Cur_KMN := KMN;
                  else
                     KMN_List.Insert (Before => Cur, Element => KMN);
                  end if;
                  exit;

               elsif Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change starts in or immediately after Cur_KMN.Stable_Bytes, ends
                  --  in or after Cur_KMN.Insert; merge Change into Cur_KMN.

                  if Cur_Last_Inserted_Byte >= Change_Last_Deleted_Byte then
                     --  Some of Cur_KMN.Inserted are preserved; test_edit_source.adb
                     --  Edit_02, _03, Deindent.
                     --
                     --   cur_kmn       next_kmn
                     --  stable|  ins| stable| ins| ...
                     --
                     --   change
                     --     | ins     |
                     --     | del  |

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes + Cur_Last_Inserted_Byte - Change_Last_Deleted_Byte;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars + Cur_Last_Inserted_Char - Change_Last_Deleted_Char;

                     Cur_KMN.Deleted_Bytes := @ + KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 - Change.Begin_Byte_Pos;
                     Cur_KMN.Deleted_Chars := @ + KMN_Last_Char + Cur_KMN.Stable_Chars + 1 - Change.Begin_Char_Pos;
                  else
                     --  All of Cur_KMN.Inserted and some of following KMN are deleted;
                     --  test_edit_source.adb Edit_04, _05, _06.

                     --  cur_kmn      next_kmn
                     --  stable|   ins| stable| ins| ...
                     --
                     --   change
                     --     | ins    |
                     --     | del                          |

                     Delete_KMNs
                       (KMN_Last_Byte, KMN_Last_Char, Cur,
                        Last_Deleted_Byte => Change.Begin_Byte_Pos + KMN.Deleted_Bytes - 1,
                        Last_Deleted_Char => Change.Begin_Char_Pos + KMN.Deleted_Chars - 1,
                        KMN               => KMN);

                     Cur_KMN.Deleted_Bytes := @ + KMN.Deleted_Bytes - Cur_KMN.Inserted_Bytes;
                     Cur_KMN.Deleted_Chars := @ + KMN.Deleted_Chars - Cur_KMN.Inserted_Chars;

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars;
                  end if;

                  Cur_KMN.Stable_Bytes := KMN.Stable_Bytes;
                  Cur_KMN.Stable_Chars := KMN.Stable_Chars;
                  exit;

               elsif Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes + 1 then
                  --  Change starts in or immediately after Cur_KMN inserted; merge
                  --  Change into Cur_KMN. test_edit_source.adb Edit_07, _08, _09,
                  --  Insert_Deindent

                  if Cur_Last_Inserted_Byte >= Change_Last_Deleted_Byte then
                     --  Beginning and end of Cur_KMN.Inserted are preserved; test_edit_source.adb
                     --  Edit_07.
                     --
                     --   cur_kmn          next_kmn
                     --  stable|  ins   | stable| ins| ...
                     --
                     --   change
                     --          | ins     |
                     --          | del|

                     Cur_KMN.Inserted_Bytes := KMN.Inserted_Bytes + Cur_KMN.Inserted_Bytes - KMN.Deleted_Bytes;
                     Cur_KMN.Inserted_Chars := KMN.Inserted_Chars + Cur_KMN.Inserted_Chars - KMN.Deleted_Chars;

                     --  Cur_KMN.Deleted_Bytes unchanged
                  else
                     --  Remainder of Cur_KMN.Inserted and some of following KMN are deleted;
                     --  test_edit_source.adb Edit_08, _09

                     --  cur_kmn      next_kmn
                     --  stable|   ins| stable| ins| ...
                     --
                     --   change
                     --         | ins    |
                     --         | del                          |

                     Delete_KMNs
                       (KMN_Last_Byte, KMN_Last_Char, Cur,
                        Last_Deleted_Byte => Change.Begin_Byte_Pos + KMN.Deleted_Bytes - 1,
                        Last_Deleted_Char => Change.Begin_Char_Pos + KMN.Deleted_Chars - 1,
                        KMN               => KMN);

                     declare
                        Remaining_Cur_Ins_Bytes : constant Zero_Buffer_Pos :=
                          Change.Begin_Byte_Pos - (KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1);

                        Remaining_Cur_Ins_Chars : constant Zero_Buffer_Pos :=
                          Change.Begin_Char_Pos - (KMN_Last_Char + Cur_KMN.Stable_Chars + 1);
                     begin
                        Cur_KMN.Deleted_Bytes := @ + KMN.Deleted_Bytes -
                          (Cur_KMN.Inserted_Bytes - Remaining_Cur_Ins_Bytes);

                        Cur_KMN.Deleted_Chars := @ + KMN.Deleted_Chars -
                          (Cur_KMN.Inserted_Chars  - Remaining_Cur_Ins_Chars);

                        Cur_KMN.Inserted_Bytes := Remaining_Cur_Ins_Bytes + KMN.Inserted_Bytes;
                        Cur_KMN.Inserted_Chars := Remaining_Cur_Ins_Chars + KMN.Inserted_Chars;
                     end;
                  end if;

                  exit;

               else
                  --  Change is entirely after Cur_KMN
                  KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
                  KMN_Last_Char := @ + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

                  Cur := Next (Cur);

                  if not Has_Element (Cur) then
                     --  Since KMN_List starts with one KMN covering all of Source, we
                     --  should never get here.
                     raise SAL.Programmer_Error;
                  end if;
               end if;
            end;
         end loop;

         if Debug_Mode then
            WisiToken.Parse.Validate_KMN
              (List                     => KMN_List,
               Initial_Text_Byte_Region => Initial_Text_Byte_Region,
               Initial_Text_Char_Region => Initial_Text_Char_Region,
               Edited_Text_Byte_Region  => Buffer_Region'(1, Base_Buffer_Pos (Source_Byte_Last)),
               Edited_Text_Char_Region  => Buffer_Region'(1, Base_Buffer_Pos (Source_Char_Last)));
         end if;
      end Edit_KMN;

   begin
      if Reallocate then
         declare
            New_Source : constant Ada.Strings.Unbounded.String_Access := new String
              (Source'First .. Source_Byte_Last + Total_Inserted_Bytes);
         begin
            New_Source (Source'First .. Source_Byte_Last) := Source (Source'First .. Source_Byte_Last);
            Ada.Strings.Unbounded.Free (Source);
            Source := New_Source;
         end;

         Gap_Last := Source'Last;
      end if;

      --  Start with one KMN with stable region = entire source.
      KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Source_Byte_Last),
          Stable_Chars   => Base_Buffer_Pos (Source_Char_Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      for Change of Changes loop
         Edit_Text (Change);
         Edit_KMN (Change);

         if Trace_Incremental_Parse > Detail then
            Trace.Put_Line ("change:" & Image (Change));
            Trace.Put_Line ("kmn_list:");
            for KMN of KMN_List loop
               Trace.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
      end loop;

      if Gap_Last /= Source'Last then
         --  Remove the gap
         Source (Gap_First .. Source_Byte_Last) := Source (Gap_Last + 1 .. Source'Last);
      end if;
   end Edit_Source;

   function Image_Action (Action : in Syntax_Trees.Post_Parse_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image_Action;

   procedure Initialize
     (Data  : in out Parse_Data_Type;
      Trace : in     WisiToken.Trace_Access)
   is begin
      Data.Trace := Trace;
   end Initialize;

   procedure Reset_Post_Parse
     (Data                : in out Parse_Data_Type;
      Tree                : in     WisiToken.Syntax_Trees.Tree'Class;
      Post_Parse_Action   : in     Post_Parse_Action_Type;
      Action_Region_Bytes : in     WisiToken.Buffer_Region;
      Action_Region_Chars : in     WisiToken.Buffer_Region;
      Begin_Indent        : in     Integer)
   is begin
      if not Tree.Editable then
         raise Parse_Error with "previous parse failed; can't execute post_parse action.";
      end if;

      Data.Post_Parse_Action   := Post_Parse_Action;
      Data.Action_Region_Bytes := Action_Region_Bytes;
      Data.Action_Region_Chars := Action_Region_Chars;
      Data.Begin_Indent := Begin_Indent;

      case Post_Parse_Action is
      when Navigate =>
         Data.Navigate_Caches.Clear;
         Data.End_Positions.Clear;
         Data.Name_Caches.Clear;

      when Face =>
         Data.Face_Caches.Clear;

      when Indent =>
         Data.Action_Region_Lines :=
           (First => Tree.Line_At_Byte_Pos (Action_Region_Bytes.First),
            Last  => Tree.Line_At_Byte_Pos (Action_Region_Bytes.Last));

         --  We need more lines in Indents than in Action_Region, for nonterms
         --  that extend outside the action region.
         declare
            Tree_Line_Region : constant Line_Region := Tree.Line_Region (Tree.Root);
         begin
            Data.Indents.Set_First_Last
              (First => Tree_Line_Region.First,
               Last  => Tree_Line_Region.Last);
         end;

         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Data.Indents.Replace_Element (I, (Not_Set, Invalid_Line_Number));
         end loop;
      end case;

      if Data.Augmented_Cache_Version = Cache_Version'Last then
         Tree.Free_Augmented;
         Data.Augmented_Cache_Version := Cache_Version'First + 1;
      else
         Data.Augmented_Cache_Version := @ + 1;
      end if;
   end Reset_Post_Parse;

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type
   is begin
      return Data.Post_Parse_Action;
   end Post_Parse_Action;

   function Action_Region_Bytes (Data : in Parse_Data_Type) return WisiToken.Buffer_Region
   is begin
      return Data.Action_Region_Bytes;
   end Action_Region_Bytes;

   overriding
   function Copy_Augmented
     (User_Data : in Parse_Data_Type;
      Augmented : in Syntax_Trees.Augmented_Class_Access)
     return Syntax_Trees.Augmented_Class_Access
   is
      Old_Aug : constant Augmented_Access := Augmented_Access (Augmented);
      New_Aug : constant Augmented_Access := new Wisi.Augmented'(Old_Aug.all);
   begin
      return Syntax_Trees.Augmented_Class_Access (New_Aug);
   end Copy_Augmented;

   overriding
   procedure Initialize_Actions
     (Data : in out Parse_Data_Type;
      Tree : in     WisiToken.Syntax_Trees.Tree'Class)
   is
      use Syntax_Trees;
   begin
      --  Parsing is complete, with error recover insert/delete tokens in
      --  the parse tree. Insert_Token, Delete_Token have been called;

      if Trace_Action > Outline then
         Data.Trace.Put_Line ("action_region_bytes: " & Image (Data.Action_Region_Bytes));
         Data.Trace.Put_Line ("action_region_lines: " & Image (Data.Action_Region_Lines));

         if Trace_Action > Extra then
            Tree.Print_Tree
              (Data.Trace.all,
               Non_Grammar  => True,
               Line_Numbers => True);
            Data.Trace.New_Line;
         end if;
      end if;
   end Initialize_Actions;

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Trace          : in out WisiToken.Trace'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   --  Set data that allows using Inserted_Token when computing indent.
   is
      use Syntax_Trees;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Inserted_Before : constant Valid_Node_Access := Tree.Next_Terminal (Inserted_Token);

      First_Token : constant Node_Access := Tree.Line_Begin_Token (Tree.Line_Region (Inserted_Before).First);

      Insert_Location : WisiToken.Insert_Location := Before_Next;
   begin
      if First_Token = Inserted_Token then
         declare
            use all type Ada.Containers.Count_Type;
            use all type SAL.Base_Peek_Type;

            --  See test/ada_mode-interactive_2.adb, "Typing ..."; three tests.
            --
            --  When typing new code, we want a new blank line to be indented as
            --  if the code was there already. To accomplish that, we put the
            --  inserted tokens at the end of the line before the Before token;
            --  that will be after the non-grammar on the previous terminal.
            --
            --  Compare to test/ada_mode-recover_20.adb. There we are not typing
            --  new code, but there is a blank line; the right paren is placed at
            --  the end of the blank line, causing the comment to be indented.
            --
            --  Also test/ada_mode-interactive_05.adb Proc_2; error recover
            --  inserts "null;" before "end"; we want it on the blank line. So
            --  Insert_After has to see the next source_terminal, which may not be
            --  Inserted_Before.

            Next_Source_Terminal : constant Valid_Node_Access :=
              (if Tree.Label (Inserted_Before) = Syntax_Trees.Source_Terminal
               then Inserted_Before
               else Tree.Next_Source_Terminal (Inserted_Before, Trailing_Non_Grammar => False));

            Prev_Terminal : constant Valid_Node_Access := Tree.Prev_Terminal (Inserted_Token);
            --  Tree.SOI if Inserted_Token is inserted before first grammar token

            Prev_Non_Grammar  : Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Prev_Terminal);
            Token_Non_Grammar : Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Inserted_Token);

            --  Prev_Non_Grammar must have at least one New_Line, since First
            --  (Inserted_Token) is True. The whitespace after the New_Line is not
            --  given a token, but comments are.
            --
            --  If the first two tokens in Prev_Non_Grammar are both New_Lines,
            --  there is a blank line after the code line (and before any
            --  comments); assume that is the edit point; see
            --  test/ada_mode-interactive_2.adb "A := B \n+C;"
            Blank_Line       : Base_Line_Number_Type := Invalid_Line_Number;
            Blank_Line_Index : SAL.Base_Peek_Type    := 0; -- new_line ending blank line
            Comment_Present  : Boolean               := False;

            procedure Check_Non_Grammar
            --  Set Blank_Line, Blank_Line_Index if there is a blank line
            --  immediately after Prev_Terminal.
            is
               I : SAL.Base_Peek_Type := Prev_Non_Grammar.First_Index;
            begin
               loop
                  exit when I > Prev_Non_Grammar.Last_Index;

                  if I > Prev_Non_Grammar.First_Index and Prev_Non_Grammar (I).ID /= Descriptor.New_Line_ID then
                     --  Exclude comment on same line as code. test/ads_mode-recover_13.adb
                     Comment_Present := True;
                     return;
                  end if;

                  if I < Prev_Non_Grammar.Last_Index and then
                    (Prev_Non_Grammar (I).ID = Descriptor.New_Line_ID and
                       Prev_Non_Grammar (I + 1).ID = Descriptor.New_Line_ID)
                  then
                     Blank_Line       := Prev_Non_Grammar (I + 1).Line_Region.First;
                     Blank_Line_Index := I + 1;
                     return;
                  end if;

                  I := I + 1;
               end loop;
            end Check_Non_Grammar;
         begin
            Check_Non_Grammar;

            Insert_Location := Parse_Data_Type'Class (Data).Insert_After
              (Tree,
               Insert_Token        => Inserted_Token,
               Insert_Before_Token => Next_Source_Terminal,
               Comment_Present     => Comment_Present,
               Blank_Line_Present  => Blank_Line /= Invalid_Line_Number);

            pragma Assert (Prev_Non_Grammar.Length > 0); --  else First would be false in condition above.

            pragma Assert (Tree.Byte_Region (Inserted_Token) = Null_Buffer_Region); -- because it's virtual.

            case Insert_Location is
            when Between =>
               --  Insert on blank line
               --
               --  test/ada_mode-interactive_2.adb Function_Access_2 and many similar.
               --  Indent for new code line extending previous code.
               declare
                  New_Non_Grammar : WisiToken.Lexer.Token_Arrays.Vector;
               begin
                  for I in Blank_Line_Index .. Prev_Non_Grammar.Last_Index loop
                     New_Non_Grammar.Append (Prev_Non_Grammar (I));
                  end loop;
                  New_Non_Grammar.Append (Token_Non_Grammar);

                  Token_Non_Grammar := New_Non_Grammar;

                  Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Blank_Line_Index - 1);

                  if Trace_Action > WisiToken.Outline then
                     Data.Trace.Put_Line
                       ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
                          " on blank line" & Blank_Line'Image);
                  end if;
               end;

            when After_Prev =>
               if Prev_Terminal = Tree.SOI then
                  --  Don't move SOI non_grammar
                  pragma Assert
                    (Prev_Non_Grammar (Prev_Non_Grammar.First_Index).ID = Tree.Lexer.Descriptor.SOI_ID);

                  for I in Prev_Non_Grammar.First_Index + 1 .. Prev_Non_Grammar.Last_Index loop
                     Token_Non_Grammar.Append (Prev_Non_Grammar (I));
                  end loop;
                  Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Prev_Non_Grammar.First_Index);

               else
                  Token_Non_Grammar := Prev_Non_Grammar;

                  Prev_Non_Grammar := WisiToken.Lexer.Token_Arrays.Empty_Vector;
               end if;

               if Trace_Action > WisiToken.Outline then
                  Data.Trace.Put_Line
                    ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
                       " after " & Tree.Image (Prev_Terminal, Node_Numbers => True));
               end if;

            when Before_Next =>
               null;
            end case;
         end;
      end if;

      if Insert_Location = Before_Next and Trace_Action > WisiToken.Outline then
         Data.Trace.Put_Line
           ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
              " before " & Tree.Image (Inserted_Before, Node_Numbers => True));
      end if;
   end Insert_Token;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Statement_Param_Array)
   is
      use all type SAL.Base_Peek_Type;

      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      First_Item         : Boolean        := True;
      Start_Set          : Boolean        := False;
      Override_Start_Set : Boolean        := False;
      Containing_Pos     : Nil_Buffer_Pos := Nil;
   begin
      if Trace_Action > Outline then
         Data.Trace.Put_Line ("Statement_Action " & Tree.Image (Nonterm, Children => True));
      end if;

      for Pair of Params loop
         if Pair.Index > Tree.Child_Count (Nonterm) then
            raise Fatal_Error with Tree.Error_Message
              (Nonterm,
               "wisi-statement-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) &
                 " token index" & SAL.Peek_Type'Image (Pair.Index) &
                 " not in tokens range (1 .." & Tree.Child_Count (Nonterm)'Image & "); bad grammar action.");

         elsif Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Pair.Index)), Data.Action_Region_Chars) then
            declare
               use all type Syntax_Trees.Node_Label;
               Token  : constant Syntax_Trees.Node_Access :=
                 (if Pair.Class = Statement_End and then
                    Tree.Label (Tree.Child (Nonterm, Pair.Index)) = Syntax_Trees.Nonterm
                  then Tree.Last_Terminal (Tree.Child (Nonterm, Pair.Index))
                  else Tree.Child (Nonterm, Pair.Index));

               Cache_Pos : constant Buffer_Pos         := Tree.Char_Region (Token).First;
               Cursor    : Navigate_Cache_Trees.Cursor := Navigate_Cache_Trees.Find
                 (Data.Navigate_Caches.Iterate, Cache_Pos,
                  Direction => Navigate_Cache_Trees.Unknown);
            begin
               if Navigate_Cache_Trees.Has_Element (Cursor) then
                  declare
                     Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                  begin
                     if Pair.Class in Statement_Start | Statement_Override then
                        if Start_Set then
                           Cache.Class := Motion;
                        else
                           Cache.Class := Statement_Start;
                           Start_Set   := True;
                        end if;
                     elsif Override_Start_Set then
                        Cache.Class := Statement_Start;
                        Start_Set   := True;
                     else
                        Cache.Class := Pair.Class;
                     end if;
                     Cache.Statement_ID   := Tree.ID (Nonterm);
                     Cache.Containing_Pos := Containing_Pos;
                     if Trace_Action > Detail then
                        Data.Trace.Put_Line
                          ("   " & Cache.Pos'Image & " nonterm to " & Image (Cache.Statement_ID, Descriptor) &
                             " containing to" & Image (Cache.Containing_Pos));
                     end if;
                  end;
               else
                  Cursor := Data.Navigate_Caches.Insert
                    ((Pos            => Cache_Pos,
                      Statement_ID   => Tree.ID (Nonterm),
                      ID             => Tree.ID (Token),
                      Length         => Length (Tree.Char_Region (Token)),
                      Class          =>
                        (if Override_Start_Set then Statement_Start
                         else
                           (case Pair.Class is
                            when Statement_Start | Statement_Override =>
                              (if Start_Set then Motion else Statement_Start),
                            when others => Pair.Class)),
                      Containing_Pos => Containing_Pos,
                      others         => Nil));

                  if Trace_Action > Detail then
                     declare
                        Cache : Navigate_Cache_Type renames Data.Navigate_Caches.Constant_Ref (Cursor);
                     begin
                        Data.Trace.Put_Line
                          ("   " & Cache.Pos'Image & " create " & Image (Cache.ID, Descriptor) &
                             ", containing to " & Image (Data.Navigate_Caches.Constant_Ref (Cursor).Containing_Pos));
                     end;
                  end if;
               end if;

               Data.End_Positions.Append (Cursor);

               if First_Item then
                  First_Item := False;
                  if Override_Start_Set or Pair.Class in Statement_Start | Statement_Override then
                     Override_Start_Set := False;
                     Containing_Pos     := (True, Tree.Char_Region (Token).First);

                     --  Set containing on all contained caches
                     declare
                        use Navigate_Cache_Trees;
                        Iterator : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;

                        Nonterm_Char_Region : constant Buffer_Region := Tree.Char_Region (Nonterm);

                        Cursor : Navigate_Cache_Trees.Cursor :=
                          (if Length (Nonterm_Char_Region) = 0
                           then No_Element
                           else Find_In_Range
                             (Iterator, Ascending, Nonterm_Char_Region.First + 1, -- don't set containing on start
                              Nonterm_Char_Region.Last));
                     begin
                        loop
                           exit when not Has_Element (Cursor);
                           declare
                              Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                           begin
                              if not Cache.Containing_Pos.Set then
                                 Cache.Containing_Pos := Containing_Pos;
                                 if Trace_Action > Detail then
                                    Data.Trace.Put_Line
                                      ("   " & Cache.Pos'Image & " containing to " & Image
                                         (Data.Navigate_Caches.Constant_Ref (Cursor).Containing_Pos));
                                 end if;
                              end if;
                              exit when Nonterm_Char_Region.Last < Cache.Pos + 1;
                           end;
                           Cursor := Iterator.Next (Cursor);
                        end loop;
                     end;
                  end if;
               end if;

               if Pair.Class = Statement_End and Containing_Pos.Set then
                  Set_End (Data, Containing_Pos.Item, Cache_Pos);
               end if;
            end;

         else
            --  Token.Char_Region is empty or outside action_region
            if First_Item and Pair.Class = Statement_Start then
               Override_Start_Set := True;
            end if;
         end if;
      end loop;
   end Statement_Action;

   procedure Name_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Name    : in     WisiToken.Positive_Index_Type)
   is
      use all type SAL.Base_Peek_Type;
      use all type Syntax_Trees.Node_Label;
   begin
      if Name > Tree.Child_Count (Nonterm) then
         raise Grammar_Error with Tree.Error_Message
           (Nonterm,
            "wisi-name-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) & " name (" &
              Trimmed_Image (Name) & ") not in child range (1 .." &
              Tree.Child_Count (Nonterm)'Image & "); bad grammar action.");
      end if;

      if Tree.Is_Virtual (Tree.Child (Nonterm, Name)) then
         --  Virtual tokens don't appear in the actual buffer, so we can't set
         --  a text property on them.
         return;
      elsif not Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Name)), Data.Action_Region_Chars) then
         return;
      end if;

      pragma Assert (Tree.Label (Tree.Child (Nonterm, Name)) in Source_Terminal | Syntax_Trees.Nonterm);

      declare
         use Name_Cache_Trees;
         Name_Char_Region : constant Buffer_Region := Tree.Char_Region (Tree.Child (Nonterm, Name));
         Cursor     : constant Name_Cache_Trees.Cursor := Find
           (Data.Name_Caches.Iterate, Name_Char_Region.First,
            Direction => Name_Cache_Trees.Unknown);
      begin
         if Has_Element (Cursor) then
            raise Fatal_Error with Tree.Error_Message
              (Tree.Child (Nonterm, Name), Tree.Image
                 (Node         => Tree.Child (Nonterm, Name),
                  Node_Numbers => Trace_Action > Extra,
                  RHS_Index    => Trace_Action > Extra)
                 & ": wisi-name-action: name set twice.");
         else
            if Trace_Action > Detail then
               Data.Trace.Put_Line
                 ("Name_Action " & Tree.Image
                    (Nonterm,
                     Node_Numbers    => Trace_Action > Extra,
                     RHS_Index       => Trace_Action > Extra) & " " & Tree.Image
                       (Tree.Child (Nonterm, Name),
                        Node_Numbers => Trace_Action > Extra,
                        RHS_Index    => Trace_Action > Extra));
            end if;

            if Name_Char_Region /= Null_Buffer_Region then
               Data.Name_Caches.Insert (Name_Char_Region);
            end if;
         end if;
      end;
   end Name_Action;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Motion_Param_Array)
   is
      use Navigate_Cache_Trees;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Iter           : constant Iterator := Data.Navigate_Caches.Iterate;
      Prev_Cache_Cur : Cursor;
   begin
      if Trace_Action > Outline then
         Data.Trace.Put_Line
           ("Motion_Action " & Image (Tree.ID (Nonterm), Descriptor) & " " &
              Image (Tree.Byte_Region (Nonterm)));
      end if;
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Param.Index)), Data.Action_Region_Chars) then
            declare
               use all type Syntax_Trees.Node_Label;
               Token     : constant Syntax_Trees.Valid_Node_Access := Tree.Child (Nonterm, Param.Index);
               Region    : constant Buffer_Region := Tree.Char_Region (Token);
               Cache_Cur : Cursor;
               Skip      : Boolean;
               Done      : Boolean := False;
            begin
               loop
                  Skip := False;

                  case Tree.Label (Tree.Child (Nonterm, Param.Index)) is
                  when Source_Terminal =>
                     Cache_Cur := Find (Iter, Region.First);
                     Done      := True;

                  when Virtual_Terminal | Virtual_Identifier =>
                     Skip := True;
                     Done := True;

                  when Syntax_Trees.Nonterm =>
                     if Param.ID = Invalid_Token_ID then
                        Cache_Cur := Find (Iter, Region.First);
                        Done      := True;

                     else
                        Skip := True;

                        if not Has_Element (Cache_Cur) then
                           Cache_Cur := Find_In_Range (Iter, Ascending, Region.First, Region.Last);
                        end if;

                        loop
                           exit when not Has_Element (Cache_Cur);
                           if Data.Navigate_Caches (Cache_Cur).Pos > Region.Last then
                              Cache_Cur := No_Element;
                              exit;

                           elsif Data.Navigate_Caches (Cache_Cur).ID = Param.ID and
                             not Data.Navigate_Caches (Cache_Cur).Prev_Pos.Set
                           then
                              Skip := False;
                              exit;
                           end if;

                           Cache_Cur := Next (Iter, Cache_Cur);
                        end loop;
                     end if;
                  end case;

                  if not Skip then
                     if not Has_Element (Cache_Cur) then
                        raise Fatal_Error with Tree.Error_Message
                          (Tree.Child (Nonterm, Param.Index),
                           Message   => "wisi-motion-action: token " &
                             WisiToken.Image (Tree.ID (Token), Descriptor) &
                             " has no cache; add to statement-action for " &
                             Trimmed_Image (Tree.Production_ID (Nonterm)) & ".");
                     end if;

                     if Has_Element (Prev_Cache_Cur) then
                        declare
                           Cache      : Navigate_Cache_Type renames Data.Navigate_Caches (Cache_Cur);
                           Prev_Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Prev_Cache_Cur);
                        begin
                           if Cache.Prev_Pos.Set then
                              if Trace_Action > Detail then
                                 Data.Trace.Put_Line
                                   ("   " & Cache.Pos'Image & " prev already at " & Cache.Prev_Pos.Item'Image);
                              end if;
                           else
                              Cache.Prev_Pos := (True, Prev_Cache.Pos);
                              if Trace_Action > Detail then
                                 Data.Trace.Put_Line
                                   ("   " & Cache.Pos'Image & " prev to " & Cache.Prev_Pos.Item'Image);
                              end if;
                           end if;

                           if Prev_Cache.Next_Pos.Set then
                              if Trace_Action > Detail then
                                 Data.Trace.Put_Line
                                   ("   " & Prev_Cache.Pos'Image & " next already at " &
                                      Prev_Cache.Next_Pos.Item'Image);
                              end if;
                           else
                              Prev_Cache.Next_Pos := (True, Cache.Pos);
                              if Trace_Action > Detail then
                                 Data.Trace.Put_Line
                                   ("   " & Prev_Cache.Pos'Image & " next to " & Prev_Cache.Next_Pos.Item'Image);
                              end if;
                           end if;
                        end;
                     end if;

                     if Data.Navigate_Caches (Cache_Cur).Next_Pos.Set then
                        --  Set Cache_Cur to end of Cache_Cur.Next chain.
                        --  Handles 'elsif ... then' in if_statement.
                        loop
                           Cache_Cur := Find (Iter, Data.Navigate_Caches (Cache_Cur).Next_Pos.Item);
                           exit when not Data.Navigate_Caches (Cache_Cur).Next_Pos.Set;
                        end loop;
                     end if;
                     Prev_Cache_Cur := Cache_Cur;
                     Cache_Cur := Iter.Next (Cache_Cur);
                  end if;
                  exit when Done or not Has_Element (Cache_Cur);
               end loop;
            end;
         end if;
      end loop;
   end Motion_Action;

   procedure Face_Apply_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array)
   is
      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Param.Index)), Data.Action_Region_Chars) then
            if Trace_Action > Outline then
               Data.Trace.Put_Line
                 ("face_apply_action: " & Image (Tree.Char_Region (Tree.Child (Nonterm, Param.Index))) &
                    " " & Param.Prefix_Face'Image & " " & Param.Suffix_Face'Image);
            end if;

            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region (Tree.Child (Nonterm, Param.Index));
            begin
               Cache_Cur := Find (Iter, Token_Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                  begin
                     case Cache.Class is
                     when Prefix =>
                        Cache.Face := (True, Param.Prefix_Face);

                        --  Check for suffix
                        Suffix_Cur := Next (Iter, Cache_Cur);
                        if Has_Element (Suffix_Cur) then
                           declare
                              Suf_Cache : Face_Cache_Type renames Data.Face_Caches (Suffix_Cur);
                           begin
                              if Suffix = Suf_Cache.Class and
                                Contains (Token_Char_Region, Suf_Cache.Char_Region.First)
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
                  Data.Face_Caches.Insert ((Token_Char_Region, Suffix, (True, Param.Suffix_Face)));
               end if;
            end;
         end if;
      end loop;
   end Face_Apply_Action;

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Apply_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Param.Index)), Data.Action_Region_Chars) then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region (Tree.Child (Nonterm, Param.Index));
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token_Char_Region.First, Token_Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token_Char_Region.Last;
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
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
            end;
         end if;
      end loop;
   end Face_Apply_List_Action;

   procedure Face_Mark_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Mark_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tree.Child (Nonterm, Param.Index)), Data.Action_Region_Chars) then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region (Tree.Child (Nonterm, Param.Index));
            begin
               Cache_Cur := Find (Iter, Token_Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Char_Region.Last + 1, Token_Char_Region.Last);
                     To_Delete : Buffer_Pos_Lists.List;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Data.Face_Caches (Other_Cur).Char_Region.First > Token_Char_Region.Last;
                        To_Delete.Append (Data.Face_Caches (Other_Cur).Char_Region.First);
                        Other_Cur := Next (Iter, Other_Cur);
                     end loop;

                     Cache.Class            := Param.Class;
                     Cache.Char_Region.Last := Token_Char_Region.Last;

                     for Face of To_Delete loop
                        Data.Face_Caches.Delete (Face);
                     end loop;
                  end;
               else
                  Data.Face_Caches.Insert ((Token_Char_Region, Param.Class, (Set => False)));
               end if;
            end;
         end if;
      end loop;
   end Face_Mark_Action;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Face_Remove_Param_Array)
   is
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for I of Params loop
         if Overlaps (Tree.Char_Region (Tree.Child (Nonterm, I)), Data.Action_Region_Chars) then
            declare
               Token_Char_Region : constant Buffer_Region := Tree.Char_Region (Tree.Child (Nonterm, I));
               To_Delete : Buffer_Pos_Lists.List;
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token_Char_Region.First, Token_Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token_Char_Region.Last;
                  To_Delete.Append (Data.Face_Caches (Cache_Cur).Char_Region.First);
                  Cache_Cur := Next (Iter, Cache_Cur);
               end loop;
               for Face of To_Delete loop
                  Data.Face_Caches.Delete (Face);
               end loop;
            end;
         end if;
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

   function Image (Item : in Simple_Indent_Param) return String
   is begin
      return "(" & Simple_Indent_Param_Label'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Block | Int => Integer'Image (Item.Int_Delta),
         when Simple_Param_Anchored => Positive_Index_Type'Image (Item.Anchored_Index) & "," &
           Integer'Image (Item.Anchored_Delta),
         when Language => "<language_function>") & ")";
   end Image;

   function Add_Simple_Indent_Param (Left, Right : in Simple_Indent_Param) return Simple_Indent_Param
   is begin
      case Left.Label is
      when None =>
         return Right;

      when Block =>
         case Right.Label is
         when None =>
            return Left;

         when Block | Int =>
            return (Block, Left.Int_Delta + Right.Int_Delta);

         when Anchored_0 =>
            return (Anchored_0, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Anchored_1 =>
            return (Anchored_1, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Int =>
         case Right.Label is
         when None =>
            return Left;

         when Block =>
            return (Block, Left.Int_Delta + Right.Int_Delta);

         when Int =>
            return (Int, Left.Int_Delta + Right.Int_Delta);

         when Anchored_0 =>
            return (Anchored_0, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Anchored_1 =>
            return (Anchored_1, Right.Anchored_Index, Left.Int_Delta + Right.Anchored_Delta);

         when Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Simple_Param_Anchored =>
         case Right.Label is
         when None =>
            return Left;

         when Block | Int =>
            case Simple_Param_Anchored'(Left.Label) is
            when Anchored_0 =>
               return (Anchored_0, Left.Anchored_Index, Left.Anchored_Delta + Right.Int_Delta);
            when Anchored_1 =>
               return (Anchored_1, Left.Anchored_Index, Left.Anchored_Delta + Right.Int_Delta);
            end case;

         when Simple_Param_Anchored | Language =>
            raise Grammar_Error with "adding incompatible indent params";
         end case;

      when Language =>
         raise Grammar_Error with "adding incompatible indent params";
      end case;
   end Add_Simple_Indent_Param;

   function Image (Item : in Indent_Param) return String
   is begin
      return "(" & Indent_Param_Label'Image (Item.Label) & ", " &
        (case Item.Label is
         when Simple => Image (Item.Param),
         when Hanging_Label =>
            Image (Item.Hanging_Delta_1) & ", "  & Image (Item.Hanging_Delta_2))
        & ")";
   end Image;

   function Image (Item : in Indent_Pair) return String
   is begin
      return "(" & Image (Item.Code_Delta) &
        (if Item.Comment_Present
         then ", " & Image (Item.Comment_Delta)
         else "") & ")";
   end Image;

   procedure Indent_Action_0
     (Data    : in out Parse_Data_Type'Class;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Params  : in     Indent_Param_Array)
   is
      use all type SAL.Base_Peek_Type;

      function In_Line_Region (Node : in Syntax_Trees.Valid_Node_Access) return Boolean
      is
         Node_Region : constant Line_Region := Tree.Line_Region (Node, Trailing_Non_Grammar => True);
      begin
         return Node_Region.First > Node_Region.Last -- null region is always in active region
           or else
           (Contains (Data.Action_Region_Lines, Node_Region.First) or
              Contains (Data.Action_Region_Lines, Node_Region.Last) or
              (Node_Region.First < Data.Action_Region_Lines.First and
              Node_Region.Last > Data.Action_Region_Lines.Last));
      end In_Line_Region;

   begin
      if Trace_Action > Outline then
         Data.Trace.Put_Line
           ("indent_action_0: " & Tree.Image
              (Nonterm, RHS_Index => True, Node_Numbers => True, Augmented => True, Line_Numbers => True));
      end if;

      for I in 1 .. Tree.Child_Count (Nonterm) loop
         if not (Tree.SOI = Tree.Child (Nonterm, I) or Tree.EOI = Tree.Child (Nonterm, I)) and then
           --  We see these in a partial parse.

           (I in Params'Range and then
              --  In some translated EBNF, not every token has an indent param.
              In_Line_Region (Tree.Child (Nonterm, I)))
         then
            declare
               Child : constant Syntax_Trees.Valid_Node_Access := Tree.Child (Nonterm, I);

               Indenting : constant Wisi.Indenting := Compute_Indenting (Data, Tree, Child);

               Code_Delta : constant Delta_Type :=
                 (if Indenting.Code = Null_Line_Region
                  then Null_Delta
                  else Indent_Compute_Delta
                    (Data, Tree, Nonterm, Params (I).Code_Delta, Child, Indenting_Comment => False));

               Controlling_Token : Syntax_Trees.Node_Access;
               Comment_Param     : Indent_Param;
               Comment_Param_Set : Boolean := False;
               Comment_Delta     : Delta_Type;
            begin
               if Trace_Action > Detail then
                  Data.Trace.Put_Line
                    ("indent_action_0 code: " & Tree.Image (Child, Line_Numbers => True) & ": " &
                       Image (Params (I).Code_Delta));
               end if;

               if Code_Delta /= Null_Delta then
                  Indent_Token_1 (Data, Tree, Child, Code_Delta, Indenting_Comment => False);
               end if;

               if Indenting.Comment /= Null_Line_Region then
                  if Params (I).Comment_Present then
                     Comment_Param     := Params (I).Comment_Delta;
                     Controlling_Token := Child;
                     Comment_Param_Set := True;

                  elsif I < Tree.Child_Count (Nonterm) then
                     Comment_Param     := Params (I + 1).Code_Delta;
                     Controlling_Token := Tree.Child (Nonterm, I + 1);
                     Comment_Param_Set := True;

                  end if;

                  if Comment_Param_Set then
                     if Trace_Action > Detail then
                        Data.Trace.Put_Line
                          ("indent_action_0 comment: " & Tree.Image (Controlling_Token, Line_Numbers => True) & ": " &
                             Image (Comment_Param));
                     end if;

                     Comment_Delta := Indent_Compute_Delta
                       (Data, Tree, Nonterm, Comment_Param, Controlling_Token, Indenting_Comment => True);

                     if Comment_Delta /= Null_Delta then
                        Indent_Token_1 (Data, Tree, Child, Comment_Delta, Indenting_Comment => True);
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Indent_Action_0;

   function Indent_Hanging_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Token    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Label             : in     Hanging_Label)
     return Delta_Type
   is
      Indenting_Line_Region : constant WisiToken.Line_Region := Tree.Line_Region (Indenting_Token);

      Indenting : constant Wisi.Indenting := Compute_Indenting (Data, Tree, Indenting_Token);

      function Compute_Hanging_2 return Simple_Indent_Param
      is begin
         --  WORKAROUND: GNAT Commmunity 2020 gives a bogus compile error when
         --  we try to inline this with an if_expression.
         if Indenting_Line_Region.First = Indenting.Code.First then
            return Add_Simple_Indent_Param (Delta_1, Delta_2);
         else
            return Delta_2;
         end if;
      end Compute_Hanging_2;

   begin
      if Indenting_Comment then
         --  We assume a comment indent is never explicitly set to hanging, so
         --  we get here when the indent of the token following the comment is
         --  hanging; Indenting_Token must be first on a following line, so
         --  Delta_1 applies.
         return Indent_Compute_Delta
           (Data, Tree, Nonterm, (Simple, Delta_1), Indenting_Token, Indenting_Comment);
      else
         return Result : Delta_Type :=
           (Hanging,
            Hanging_First_Line  => Indenting_Line_Region.First,
            Hanging_Delta_1     => Indent_Compute_Delta
              (Data, Tree, Nonterm,
               (Simple,
                (if Indenting_Line_Region.First = Indenting.Code.First
                 then Delta_1
                 else (Label => None))),
               Indenting_Token, Indenting_Comment).Simple_Delta,
            Hanging_Delta_2     =>
              Indent_Compute_Delta
                (Data, Tree, Nonterm,
                 (Simple,
                  (case Label is
                   when Hanging_0 => Delta_2,
                   when Hanging_1 =>
                     (if Indenting_Line_Region.First = Indenting.Code.First
                      then Delta_2 else Delta_1),
                   when Hanging_2 =>
                      Compute_Hanging_2)),
                 Indenting_Token, Indenting_Comment)
                .Simple_Delta)
         do
            --  Controlling_Token_Line for Delta_2 is the first non-comment
            --  line indented by Delta_2.
            if Label = Hanging_1 and
              Indenting_Line_Region.First /= Indenting.Code.First
            then
               --  Only using Delta_1
               null;
            else
               for Line in Indenting.Code.First +
                 (if Indenting_Line_Region.First = Indenting.Code.First then 1 else 0)
                 .. Indenting.Code.Last
               loop
                  if Tree.Line_Begin_Token (Line) /= Syntax_Trees.Invalid_Node_Access then
                     Result.Hanging_Delta_2.Controlling_Token_Line := Line;
                     exit;
                  end if;
               end loop;
            end if;
         end return;
      end if;
   end Indent_Hanging_1;

   procedure Query_Tree
     (Data       : in Parse_Data_Type;
      Tree       : in WisiToken.Syntax_Trees.Tree;
      Label      : in Query_Label;
      Char_Point : in WisiToken.Buffer_Pos)
   is
      use Syntax_Trees;
   begin
      case Label is
      when Bounds =>
         declare
            Result_Chars : constant WisiToken.Buffer_Region := Tree.Char_Region (Tree.Root);
            Result_Lines : constant WisiToken.Line_Region   := Tree.Line_Region (Tree.Root);
         begin
            Ada.Text_IO.Put_Line
              ("[" & Query_Tree_Code &
                 Query_Label'Pos (Label)'Image &
                 Result_Chars.First'Image &
                 Result_Chars.Last'Image &
                 Result_Lines.First'Image &
                 Result_Lines.Last'Image & "]");
         end;

      when Containing_Statement =>
         declare
            Node : constant Node_Access := Tree.Find_Char_Pos
              (Char_Point, After => True, Trailing_Non_Grammar => True);
         begin
            if Node = Invalid_Node_Access then
               Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Label)'Image & " nil]");
            else
               declare
                  Statement : constant Node_Access := Tree.Find_Ancestor (Node, To_Array (Data.Statement_IDs));
                  Char_Region : constant Buffer_Region :=
                    (if Statement = Invalid_Node_Access
                     then Null_Buffer_Region
                     else Tree.Char_Region (Statement, Trailing_Non_Grammar => False));
               begin
                  if Statement = Invalid_Node_Access then
                     Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Label)'Image & " nil]");
                  else
                     Ada.Text_IO.Put_Line
                       ("[" & Query_Tree_Code &
                          Query_Label'Pos (Label)'Image &
                          Tree.ID (Statement)'Image & " " &
                          Char_Region.First'Image & " " &
                          Char_Region.Last'Image & "]");
                  end if;
               end;
            end if;
         end;
      when Nonterm =>
         declare
            Terminal : constant Node_Access := Tree.Find_Char_Pos
              (Char_Point, After => False, Trailing_Non_Grammar => False);
         begin
            if Terminal = Invalid_Node_Access then
               Ada.Text_IO.Put_Line ("[" & Query_Tree_Code & Query_Label'Pos (Label)'Image & " nil]");
            else
               Ada.Text_IO.Put_Line
                 ("[" & Query_Tree_Code &
                    Query_Label'Pos (Label)'Image &
                    Tree.ID (Tree.Parent (Terminal))'Image & "]");
            end if;
         end;

      when Virtuals =>
         declare
            Result_Nodes : constant WisiToken.Syntax_Trees.Valid_Node_Access_Array := Tree.Get_Virtuals (Tree.Root);
            Result_String : Ada.Strings.Unbounded.Unbounded_String;
         begin
            for N of Result_Nodes loop
               Ada.Strings.Unbounded.Append
                 (Result_String, "[" & Tree.ID (N)'Image & Tree.Char_Region (N).First'Image & "]");
            end loop;

            Ada.Text_IO.Put_Line
              ("[" & Query_Tree_Code & Query_Label'Pos (Label)'Image & "[" & (-Result_String) & "]]");
         end;

      when Print =>
         Tree.Print_Tree (Data.Trace.all, Line_Numbers => True, Non_Grammar => True);
      end case;
   end Query_Tree;

   procedure Put_Language_Action
     (Data    : in Parse_Data_Type;
      Content : in String)
   is
      pragma Unreferenced (Data);
   begin
      Ada.Text_IO.Put_Line ("[" & Language_Action_Code & Content & "]");
   end Put_Language_Action;

   procedure Put (Data : in out Parse_Data_Type; Parser : in Parse.Base_Parser'Class)
   is
      Tree : WisiToken.Syntax_Trees.Tree renames Parser.Tree;
   begin
      --  +1 to match Emacs region
      Ada.Text_IO.Put_Line ('[' & End_Code & Buffer_Pos'Image (Tree.Char_Region (Tree.EOI).Last + 1) & ']');

      case Data.Post_Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            if Contains (Data.Action_Region_Chars, Cache.Pos) then
               Put (Cache);
            end if;
         end loop;
         for Cache of Data.Name_Caches loop
            if Contains (Outer => Data.Action_Region_Chars, Inner => Cache) then
               Put (Cache);
            end if;
         end loop;

      when Face =>
         for Cache of Data.Face_Caches loop
            if Overlaps (Cache.Char_Region, Data.Action_Region_Chars) then
               Put (Cache);
            end if;
         end loop;

      when Indent =>
         Resolve_Anchors (Data, Tree);
         for Line in Data.Action_Region_Lines.First .. Data.Action_Region_Lines.Last loop
            Put (Tree, Line, Data.Indents (Line));
         end loop;
      end case;
   exception
   when E : others =>
      if Debug_Mode then
         Parser.Trace.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
         Parser.Trace.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
         Parser.Trace.New_Line;
      end if;
      raise;
   end Put;

   procedure Put (Lexer_Errors : in Lexer.Error_Lists.List)
   is begin
      for Item of Lexer_Errors loop
         Ada.Text_IO.Put_Line
           ('[' & Lexer_Error_Code & Buffer_Pos'Image (Item.Char_Pos) &
              " ""lexer error" &
              (if Item.Recover_Char (1) = ASCII.NUL
               then """"
               elsif Item.Recover_Char (1) = '"'
               then """ ?\"""
               else """ ?" & Item.Recover_Char (1)) &
              "]");
         if Item.Recover_Char (2) /= ASCII.NUL then
            raise SAL.Programmer_Error with "lexer error with non-ascii or multiple repair char";
         end if;
      end loop;
   end Put;

   procedure Put
     (Data         : in Parse_Data_Type;
      Lexer_Errors : in Lexer.Error_Lists.List;
      Parse_Errors : in Parse.LR.Parse_Error_Lists.List;
      Recover      : in Parse.LR.Recover_Op_Arrays.Vector;
      Tree         : in Syntax_Trees.Tree)
   is
      use Ada.Text_IO;
      use In_Parse_Actions;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      function Safe_Pos_Image (Token : in Syntax_Trees.Recover_Token) return String
      is
         Result : constant Base_Buffer_Pos := Tree.Name (Token).First;
      begin
         if Result = Invalid_Buffer_Pos then
            return " nil";
         end if;
         return Result'Image;
      end Safe_Pos_Image;

      function Safe_Pos (Token : in Syntax_Trees.Node_Access) return Buffer_Pos
      is begin
         if Token = Syntax_Trees.Invalid_Node_Access then
            return Buffer_Pos'First;
         else
            declare
               Result : constant Buffer_Region := Tree.Char_Region (Token);
            begin
               if Result = Null_Buffer_Region then
                  return Buffer_Pos'First;
               else
                  return Result.First;
               end if;
            end;
         end if;
      end Safe_Pos;
   begin
      Put (Lexer_Errors);

      for Item of Parse_Errors loop
         case Item.Label is
         when Parse.LR.LR_Parse_Action =>
            Put_Line
              ('[' & Parser_Error_Code & Base_Buffer_Pos'Image (Safe_Pos (Item.Error_Token.Node)) &
                 " ""syntax error: expecting " & Image (Item.Expecting, Descriptor) &
                 ", found '" & Image (Tree.ID (Item.Error_Token.Node), Descriptor) & "'""]");

         when Parse.LR.User_Parse_Action =>
            Put_Line
              ('[' & Check_Error_Code & Integer'Image
                 (In_Parse_Actions.Status_Label'Pos (Item.Status.Label)) &
                 (case Item.Status.Label is
                  when Ok => "",
                  when Error =>
                     Safe_Pos_Image (Item.Status.Begin_Name) &
                       Safe_Pos_Image (Item.Status.End_Name) & " """ &
                       (case Error'(Item.Status.Label) is
                        when Missing_Name_Error => "missing",
                        when Extra_Name_Error => "extra",
                        when Match_Names_Error => "match") &
                       " name error""]"));

         when Parse.LR.Message =>
            Put_Line
              ('[' & Parser_Error_Code & Buffer_Pos'Image (Buffer_Pos'First) &
                 " """ & (-Item.Msg) & """]");
         end case;
      end loop;

      Put (Recover, Data, Tree);
   end Put;

   procedure Put_Error
     (Tree        : in Syntax_Trees.Tree;
      Line_Number : in Line_Number_Type;
      Message     : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (Tree.Lexer.File_Name, Line_Number, 0, Message) & """)");
   end Put_Error;

   ----------
   --  Spec visible private subprograms, alphabetical

   function Compute_Indenting
     (Data : in Parse_Data_Type'Class;
      Tree : in Syntax_Trees.Tree;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Wisi.Indenting
   is
      Aug : constant Augmented_Access := Get_Augmented (Tree, Node);
   begin
      if Aug.Cache_Version = Data.Augmented_Cache_Version then
         return Aug.Indenting;
      end if;

      declare
         use all type Ada.Containers.Count_Type;
         use all type SAL.Base_Peek_Type;
         use Syntax_Trees;
         use Lexer;

         --  The precondition guarrantees Prev_Non_Grammar and Next_Non_Grammar exist.

         Prev_Non_Grammar  : constant Valid_Node_Access := Tree.Prev_Non_Grammar (Node);
         Next_Non_Grammar  : constant Valid_Node_Access := Tree.Next_Non_Grammar (Node);
         Prev_Terminal     : constant Valid_Node_Access := Tree.Prev_Terminal (Node);
         First_Non_Grammar : constant Node_Access       := Tree.First_Non_Grammar (Node);
         Last_Terminal     : constant Node_Access       := Tree.Last_Terminal (Node);

         function Has_New_Line (Node : in Valid_Node_Access) return Boolean
         is begin
            return (for some Token of Tree.Non_Grammar_Const (Node) =>
                      Contains_New_Line (Token.Line_Region));
         end Has_New_Line;

         function Get_Last (Node : in Valid_Node_Access) return Base_Line_Number_Type
         is
            Non_Grammar  : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Node);
         begin
            if Non_Grammar.Length = 0 then
               return Invalid_Line_Number;
            else
               return Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last;
            end if;
         end Get_Last;

         function Get_First (Node : in Valid_Node_Access) return Base_Line_Number_Type
         is
            Non_Grammar : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Node);
         begin
            if Non_Grammar.Length = 0 then
               return Invalid_Line_Number;
            else
               return Non_Grammar (Non_Grammar.First_Index).Line_Region.First;
            end if;
         end Get_First;

         First_Code_Line : constant Base_Line_Number_Type :=
           --  Correct even if not first in line.
           (if Prev_Non_Grammar = Prev_Terminal and Has_New_Line (Prev_Non_Grammar)
            then  --  First terminal in Node is first on a line
               Get_Last (Prev_Non_Grammar)
            elsif First_Non_Grammar = Invalid_Node_Access
            then Invalid_Line_Number
            elsif Last_Terminal = Invalid_Node_Access
            then Invalid_Line_Number -- No grammar terminals after first_non_grammar
            else Get_First (First_Non_Grammar));

         Last_Code_Line : constant Base_Line_Number_Type :=
           (if Last_Terminal = Invalid_Node_Access
            then Get_First (Next_Non_Grammar)
            elsif Get_First (Last_Terminal) = Invalid_Line_Number
            then Get_First (Next_Non_Grammar)
            else Get_First (Last_Terminal));

      begin
         return Result : Wisi.Indenting do
            if Last_Terminal = Invalid_Node_Access then
               --  Node is an empty nonterm
               Result.Code    := Null_Line_Region;
               Result.Comment := Null_Line_Region;
            else
               if First_Code_Line = Invalid_Line_Number or Last_Code_Line = Invalid_Line_Number then
                  Result.Code := Null_Line_Region;
               else
                  if Prev_Non_Grammar = Prev_Terminal then
                     --  First terminal in Node is first on line.
                     Result.Code := (First_Code_Line, Last_Code_Line);

                  elsif First_Code_Line = Last_Code_Line then
                     --  Not first on line, none on next line
                     Result.Code := Null_Line_Region;
                  else
                     --  Not first on line, some on next line
                     Result.Code :=
                       (First => First_Code_Line + 1,
                        Last  => Last_Code_Line);
                  end if;
               end if;

               if Last_Terminal = Invalid_Node_Access then
                  Result.Comment := Null_Line_Region;

               else
                  declare
                     Trailing_Non_Grammar : Token_Arrays.Vector renames Tree.Non_Grammar_Const (Last_Terminal);
                  begin
                     if Trailing_Non_Grammar.Length in 0 | 1 then
                        --  Single non_grammar either contains a single new_line or is a
                        --  non-new_line comment (ie placeholder); neither needs indenting.
                        --  FIXME: handle multi-line comments
                        Result.Comment := Null_Line_Region;

                     else
                        if Contains_New_Line (Trailing_Non_Grammar (Trailing_Non_Grammar.First_Index).Line_Region)
                        then
                           --  First non_grammar terminates code line.
                           Result.Comment.First := Trailing_Non_Grammar
                             (Trailing_Non_Grammar.First_Index).Line_Region.Last;
                        else
                           --  First non_grammar is a block comment (ie placeholder) on the code
                           --  line; find first blank or comment line, if any.
                           declare
                              First_Line : constant Line_Number_Type := Trailing_Non_Grammar
                                (Trailing_Non_Grammar.First_Index).Line_Region.Last;
                           begin
                              for I in Trailing_Non_Grammar.First_Index + 1 .. Trailing_Non_Grammar.Last_Index loop
                                 if Trailing_Non_Grammar (I).Line_Region.First /= First_Line then
                                    Result.Comment.First := Trailing_Non_Grammar (I).Line_Region.First;
                                    exit;
                                 end if;
                              end loop;
                           end;
                        end if;

                        Result.Comment.Last  :=
                          (if Contains_New_Line (Trailing_Non_Grammar (Trailing_Non_Grammar.Last_Index).Line_Region)
                           then
                              Trailing_Non_Grammar (Trailing_Non_Grammar.Last_Index).Line_Region.First
                           else
                              Trailing_Non_Grammar (Trailing_Non_Grammar.Last_Index).Line_Region.Last);
                     end if;
                  end;
               end if;
            end if;

            Aug.Cache_Version := Data.Augmented_Cache_Version;
            Aug.Indenting     := Result;
         end return;
      end;
   end Compute_Indenting;

   function Get_Augmented
     (Tree : in Syntax_Trees.Tree'Class;
      Node : in Syntax_Trees.Valid_Node_Access)
     return Augmented_Access
   is
      Aug : Augmented_Access := Augmented_Access (Tree.Augmented (Node));
   begin
      if Aug = null then
         Aug := new Augmented;
         Tree.Set_Augmented (Node, Syntax_Trees.Augmented_Class_Access (Aug));
      end if;
      return Aug;
   end Get_Augmented;

   function Image (Item : in Simple_Delta_Type) return String
   is begin
      return "(" & Trimmed_Image (Item.Controlling_Token_Line) & ": " & Simple_Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Int => Integer'Image (Item.Int_Delta),
         when Anchored => Item.Anchor_Line'Image & Item.Anchored_Delta'Image)
        & ")";
   end Image;

   function Image (Item : in Delta_Type) return String
   is begin
      return "(" & Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when Simple => " " & Image (Item.Simple_Delta),
         when Hanging => Line_Number_Type'Image (Item.Hanging_First_Line) &
           " " & Image (Item.Hanging_Delta_1) & " " & Image (Item.Hanging_Delta_2)) & ")";
   end Image;

   function Current_Indent_Offset
     (Tree         : in Syntax_Trees.Tree'Class;
      Anchor_Token : in Syntax_Trees.Valid_Node_Access;
      Offset       : in Integer)
     return Integer
   is
      Line_Begin_Token : constant Syntax_Trees.Node_Access := Tree.Line_Begin_Token
        (Tree.Line_Region (Anchor_Token).First);
   begin
      return Offset + Integer
        (Tree.Char_Region (Anchor_Token).First -
           (if Line_Begin_Token = WisiToken.Syntax_Trees.Invalid_Node_Access
            then 0
            else Tree.Char_Region (Line_Begin_Token).First));
   end Current_Indent_Offset;

   function Get_Text
     (Data       : in Parse_Data_Type;
      Tree       : in Syntax_Trees.Tree;
      Tree_Index : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return String
   is
      use all type Syntax_Trees.Node_Label;
   begin
      case Tree.Label (Tree_Index) is
      when Source_Terminal | Nonterm =>
         return Tree.Lexer.Buffer_Text (Tree.Byte_Region (Tree_Index));

      when Virtual_Terminal | Virtual_Identifier =>
         raise SAL.Programmer_Error;

      end case;
   end Get_Text;

   function Elisp_Escape_Quotes (Item : in String) return String
   is
      Result : String (Item'First .. Item'First + Item'Length * 2);
      Last   : Integer := Item'First - 1;
   begin
      for I in Item'Range loop
         if Item (I) = '"' then
            Last := Last + 1;
            Result (Last) := '\';
         end if;
         Last := Last + 1;
         Result (Last) := Item (I);
      end loop;
      return Result (Result'First .. Last);
   end Elisp_Escape_Quotes;

   function Indent_Anchored_2
     (Data              : in Parse_Data_Type'Class;
      Tree              : in Syntax_Trees.Tree;
      Anchor_Token      : in Syntax_Trees.Valid_Node_Access;
      Indenting_Token   : in Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in Boolean;
      Offset            : in Integer)
     return Delta_Type
   is
      Anchor_Line    : constant Line_Number_Type := Tree.Line_Region (Anchor_Token).First;
      Indenting_Line : constant Line_Number_Type :=
        (if Indenting_Comment
         then Compute_Indenting (Data, Tree, Indenting_Token).Comment.First
         else Tree.Line_Region (Indenting_Token).Last);
   begin
      if Anchor_Line = Indenting_Line then
         --  test/ada_mode-interactive_1.adb:
         --  E := (1 =>
         --          'A');
         --
         --  The expression is anchored to itself, which is needed for
         --  multi-line expressions (ada_annex_p.wy assoc_expression).
         return Null_Delta;
      else
         return
           (Simple,
            (Anchored,
             Controlling_Token_Line => Anchor_Line,
             Anchor_Line            => Anchor_Line,
             Anchored_Delta         => Offset));
      end if;
   end Indent_Anchored_2;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     Syntax_Trees.Valid_Node_Access;
      Param             : in     Indent_Param;
      Indenting_Token   : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean)
     return Delta_Type
   is begin
      --  Evaluate wisi-anchored*, wisi-hanging*.
      case Param.Label is
      when Simple =>
         case Param.Param.Label is
         when None =>
            return Null_Delta;

         when Block =>
            return (Simple, (Int, Invalid_Line_Number, Param.Param.Int_Delta));

         when Int =>
            return (Simple, (Int, Tree.Line_Region (Indenting_Token).First, Param.Param.Int_Delta));

         when Simple_Param_Anchored =>
            --  [2] wisi-anchored, wisi-anchored%
            declare
               Anchor_Token  : constant Syntax_Trees.Valid_Node_Access := Tree.Child
                 (Nonterm, Param.Param.Anchored_Index);
            begin
               return Indent_Anchored_2
                 (Data, Tree, Anchor_Token, Indenting_Token, Indenting_Comment,
                  Offset         =>
                    (case Simple_Param_Anchored'(Param.Param.Label) is
                     when Anchored_0 =>
                        --  test/ada_mode-interactive_2.adb 'if (A and B -- Comment 1'
                        Current_Indent_Offset (Tree, Anchor_Token, Param.Param.Anchored_Delta),
                     when Anchored_1 =>
                        Paren_In_Anchor_Line (Data, Tree, Anchor_Token, Param.Param.Anchored_Delta)));
            end;

         when Language =>
            return Param.Param.Function_Ptr
              (Data, Tree, Nonterm, Indenting_Token, Indenting_Comment, Param.Param.Args);
         end case;

      when Hanging_Label =>
         return Indent_Hanging_1
           (Data, Tree, Nonterm, Indenting_Token, Indenting_Comment, Param.Hanging_Delta_1,
            Param.Hanging_Delta_2, Param.Label);
      end case;
   end Indent_Compute_Delta;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Indenting_Token   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean)
   is
      Indenting : constant Wisi.Indenting := Compute_Indenting (Data, Tree, Indenting_Token);

      Line_Region : constant WisiToken.Line_Region :=
        (if Indenting_Comment
         then Indenting.Comment
         else Indenting.Code);

   begin
      if Trace_Action > Detail then
         Data.Trace.Put_Line
           ("indent_token_1:      " &
              Tree.Image (Indenting_Token, Node_Numbers => True) & " " & Image (Delta_Indent) &
              (if Indenting_Comment then " comment" else " code") &
              Image (Line_Region));
      end if;

      for Line in Line_Region.First .. Line_Region.Last loop
         if Data.Indent_Comment_Col_0 then
            declare
               use all type Ada.Text_IO.Count;
               Indent              : Boolean := True;
               Line_Begin_Char_Pos : Buffer_Pos;

               Containing : constant Syntax_Trees.Valid_Node_Access := Tree.Find_New_Line
                 (Line, Line_Begin_Char_Pos);
            begin
               for Tok of Tree.Non_Grammar_Const (Containing) loop
                  if Tok.Line_Region.First = Line and then
                    Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                    Lexer.Column (Tok, Line_Begin_Char_Pos) = 0
                  then
                     Indent := False;
                     exit;
                  end if;
               end loop;

               if Indent then
                  Indent_Line (Data, Line, Delta_Indent);
               else
                  Indent_Line (Data, Line, (Simple, (Int, Invalid_Line_Number, 0)));
               end if;
            end;
         else
            Indent_Line (Data, Line, Delta_Indent);
         end if;
      end loop;
   end Indent_Token_1;

   function Refactor_Parse  (Data : in Parse_Data_Type; Item : in String) return Refactor_Action
   is
      pragma Unreferenced (Item);
      pragma Unreferenced (Data);
   begin
      return Refactor_Action'Last;
   end Refactor_Parse;

end Wisi;
