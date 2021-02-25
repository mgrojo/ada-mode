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
with SAL;
with WisiToken.In_Parse_Actions;
package body Wisi is
   use WisiToken;

   Chars_Per_Int : constant Integer := Integer'Width;

   ----------
   --  body subprogram specs (as needed), alphabetical

   function Max_Anchor_ID
     (Data       : in out Parse_Data_Type;
      First_Line : in     Line_Number_Type;
      Last_Line  : in     Line_Number_Type)
     return Integer;

   ----------
   --  body subprograms bodies, alphabetical

   procedure Update_Indent_Lines
     (Tree          : in     Syntax_Trees.Tree'Class;
      Deleted_Tok   : in     WisiToken.Base_Token;
      Deleted_Aug   : in     Augmented_Var_Ref;
      Prev_Token    : in     Syntax_Trees.Node_Access;
      Next_Token    : in     Syntax_Trees.Node_Access;
      Prev_Modified : in out Boolean)
   is
      use all type WisiToken.Syntax_Trees.Node_Access;

      Next_Modified : Boolean := False;
   begin
      if (Prev_Token = Syntax_Trees.Invalid_Node_Access or else
           Tree.Line_Region (Prev_Token).First < Deleted_Tok.Line_Region.First) and
        (Next_Token = Syntax_Trees.Invalid_Node_Access or else
           Tree.Line_Region (Next_Token).First > Deleted_Tok.Line_Region.First)
      then
         --  Deleted_Tok.Line is now blank; add to previous token indent range
         if Prev_Token /= Syntax_Trees.Invalid_Node_Access then
            Prev_Modified := True;
            declare
               Prev_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Token);
            begin
               if Prev_Aug.First_Trailing_Comment_Line = Invalid_Line_Number then
                  Prev_Aug.First_Trailing_Comment_Line := Deleted_Tok.Line_Region.First;
                  Prev_Aug.Last_Trailing_Comment_Line  := Deleted_Tok.Line_Region.Last;
               else
                  if Prev_Aug.First_Trailing_Comment_Line > Deleted_Tok.Line_Region.First then
                     Prev_Aug.First_Trailing_Comment_Line := Deleted_Tok.Line_Region.First;
                  end if;
                  if Prev_Aug.Last_Trailing_Comment_Line < Deleted_Tok.Line_Region.Last then
                     Prev_Aug.Last_Trailing_Comment_Line := Deleted_Tok.Line_Region.Last;
                  end if;
               end if;
            end;
         end if;
      end if;

      if Prev_Modified then
         --  May need this for First in next block; test/ada_mode-recover_partial_01.adb
         Tree.Update_Ancestor_Cache (Prev_Token);
      end if;

      if Next_Token /= Syntax_Trees.Invalid_Node_Access
        and then Tree.Augmented (Next_Token) /= null
      then
         declare
            Next_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Next_Token);
         begin
            if First (Tree, Next_Token) then
               if Next_Aug.First_Indent_Line = Invalid_Line_Number then
                  Next_Aug.First_Indent_Line := Tree.Line_Region (Next_Token).First;
                  Next_Aug.Last_Indent_Line  := Tree.Line_Region (Next_Token).Last;
               else
                  Next_Aug.First_Indent_Line := Tree.Line_Region (Next_Token).First;
               end if;
            else
               Next_Aug.First_Indent_Line := Deleted_Aug.First_Indent_Line;
               Next_Aug.Last_Indent_Line  := Deleted_Aug.Last_Indent_Line;
            end if;
            Next_Modified := True;
         end;
      end if;

      if Next_Modified then
         Tree.Update_Ancestor_Cache (Next_Token);
      end if;
   end Update_Indent_Lines;

   procedure Edit
     (Data  : in out Parse_Data_Type;
      Edits : in     WisiToken.Parse.KMN_Lists.List)
   is
      use all type Ada.Containers.Count_Type;

      Old_Point : Buffer_Pos := Buffer_Pos'First; -- in original source text

      Navigate_Iterator : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;
      Name_Iterator     : constant Name_Cache_Trees.Iterator     := Data.Name_Caches.Iterate;
      Face_Iterator     : constant Face_Cache_Trees.Iterator     := Data.Face_Caches.Iterate;

      Navigate : Navigate_Cache_Trees.Cursor := Navigate_Iterator.First;
      Name     : Name_Cache_Trees.Cursor     := Name_Iterator.First;
      Face     : Face_Cache_Trees.Cursor     := Face_Iterator.First;

      Shift_Chars : Base_Buffer_Pos := 0;

      type Old_New is record
         Old_Char_First : Base_Buffer_Pos;
         Shift_Chars  : Base_Buffer_Pos;
      end record;

      Shifts : array (1 .. Edits.Length) of Old_New;
      Shifts_Next : Ada.Containers.Count_Type := Shifts'First;

      Navigate_Delete : WisiToken.Buffer_Pos_Lists.List;
      Name_Delete     : WisiToken.Buffer_Pos_Lists.List;
      Face_Delete     : WisiToken.Buffer_Pos_Lists.List;

      function Map (Old_Pos : in Base_Buffer_Pos) return Base_Buffer_Pos
      is begin
         for I in Shifts'Range loop
            if Old_Pos >= Shifts (I).Old_Char_First and
              (I = Shifts'Last or else Old_Pos < Shifts (I + 1).Old_Char_First)
            then
               return Old_Pos + Shifts (I).Shift_Chars;
            end if;
         end loop;
         raise SAL.Programmer_Error; -- can't get here.
      end Map;

   begin
      --  Elisp handles deleting text properties in edited regions; delete
      --  the same data here. Execute_Actions will computed new text
      --  properties for the edited regions.

      for Edit of Edits loop
         Shifts (Shifts_Next) := (Old_Point, Shift_Chars);
         Shifts_Next := @ + 1;

         Old_Point := @ + Edit.Stable_Chars;
         declare
            Old_Edit_Region_Chars : constant Buffer_Region := (Old_Point, Old_Point + Edit.Deleted_Chars);
         begin
            loop
               exit when not Navigate_Cache_Trees.Has_Element (Navigate);
               exit when Data.Navigate_Caches (Navigate).Pos >= Old_Point;

               --  Navigate positions are updated below using Map.

               Navigate := Navigate_Iterator.Next (Navigate);
            end loop;

            if Navigate_Cache_Trees.Has_Element (Navigate) and then
              Inside (Data.Navigate_Caches (Navigate).Pos, Old_Edit_Region_Chars)
            then
               Navigate_Delete.Append (Data.Navigate_Caches (Navigate).Pos);
               Navigate := Navigate_Iterator.Next (Navigate);
            end if;

            loop
               exit when not Name_Cache_Trees.Has_Element (Name);
               exit when Data.Name_Caches (Name).First >= Old_Point;

               Data.Name_Caches.Variable_Ref (Name).Element.all := @ + Shift_Chars;

               Name := Name_Iterator.Next (Name);
            end loop;

            if Name_Cache_Trees.Has_Element (Name) and then
              Overlaps (Data.Name_Caches (Name), Old_Edit_Region_Chars)
            then
                  Name_Delete.Append (Data.Name_Caches (Name).First);
                  Name := Name_Iterator.Next (Name);
            end if;

            loop
               exit when not Face_Cache_Trees.Has_Element (Face);
               exit when Data.Face_Caches (Face).Char_Region.First >= Old_Point;

               Data.Face_Caches (Face).Char_Region := @ + Shift_Chars;

               Face := Face_Iterator.Next (Face);
            end loop;

            if Face_Cache_Trees.Has_Element (Face) and then
              Overlaps (Data.Face_Caches (Face).Char_Region, Old_Edit_Region_Chars)
            then
               Face_Delete.Append (Data.Face_Caches (Face).Char_Region.First);
               Face := Face_Iterator.Next (Face);
            end if;
         end;

         Shift_Chars := @ - Edit.Deleted_Chars + Edit.Inserted_Chars;
      end loop;

      for Pos of Navigate_Delete loop
         Data.Navigate_Caches.Delete (Pos);
      end loop;

      for Pos of Name_Delete loop
         Data.Name_Caches.Delete (Pos);
      end loop;

      for Pos of Face_Delete loop
         begin
            Data.Face_Caches.Delete (Pos);
         exception
         when SAL.Not_Found =>
            --  Code above can enter Pos into Face_Delete more than once
            null;
         end;
      end loop;

      --  Now Map is valid; finish shifting Navigate
      Navigate := Navigate_Iterator.First;
      loop
         exit when not Navigate_Cache_Trees.Has_Element (Navigate);

         declare
            Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Navigate);
         begin
            Cache.Pos := Map (@);
            if Cache.Containing_Pos.Set then
               Cache.Containing_Pos := (True, Map (@.Item));
            end if;
            if Cache.Prev_Pos.Set then
               Cache.Prev_Pos := (True, Map (@.Item));
            end if;
            if Cache.Next_Pos.Set then
               Cache.Next_Pos := (True, Map (@.Item));
            end if;
            if Cache.End_Pos.Set then
               Cache.End_Pos := (True, Map (@.Item));
            end if;
         end;
         Navigate := Navigate_Iterator.Next (Navigate);
      end loop;
   end Edit;

   overriding
   function Image_Augmented (Aug : in Augmented) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      if Aug.First_Indent_Line /= Invalid_Line_Number then
         Result := @ & "indent code" & Aug.First_Indent_Line'Image & " .." & Aug.Last_Indent_Line'Image;
      end if;

      if Aug.First_Trailing_Comment_Line /= Invalid_Line_Number then
         Result := @ & (if Length (Result) > 1 then "," else "indent") &
           " comment" & Aug.First_Trailing_Comment_Line'Image & " .." &
           Aug.Last_Trailing_Comment_Line'Image;
      end if;

      if Length (Result) > 0 then
         Result := "(" & @ & ")";
      end if;
      return -Result;
   end Image_Augmented;

   overriding
   procedure Shift
     (Augmented   : in out Wisi.Augmented;
      Shift_Bytes : in     Base_Buffer_Pos;
      Shift_Chars : in     Base_Buffer_Pos;
      Shift_Line  : in     Base_Line_Number_Type)
   is begin
      if Augmented.First_Indent_Line /= Invalid_Line_Number then
         Augmented.First_Indent_Line := @ + Shift_Line;
         Augmented.Last_Indent_Line := @ + Shift_Line;
      end if;

      if Augmented.First_Trailing_Comment_Line /= Invalid_Line_Number then
         Augmented.First_Trailing_Comment_Line := @ + Shift_Line;
         Augmented.Last_Trailing_Comment_Line := @ + Shift_Line;
      end if;
   end Shift;

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
   is
      Prefix : constant String := "(" & Trimmed_Image (Indent.Controlling_Token_Line) & ": " &
        Indent_Label'Image (Indent.Label);
   begin
      case Indent.Label is
      when Not_Set =>
         return Prefix & ")";

      when Int =>
         return Prefix & Integer'Image (Indent.Int_Indent) & ")";

      when Anchor_Nil =>
         return Prefix & ", " & Image (Indent.Anchor_Nil_IDs) & ", nil)";

      when Anchor_Int =>
         return Prefix & ", " & Image (Indent.Anchor_Int_IDs) & ", " & Integer'Image
           (Indent.Anchor_Int_Indent) & ")";

      when Anchored =>
         return Prefix & ", " & Integer'Image (Indent.Anchored_ID) & ", " &
           Integer'Image (Indent.Anchored_Delta) & ")";

      when Anchor_Anchored =>
         return Prefix & ", " & Image (Indent.Anchor_Anchored_IDs) & Integer'Image
           (Indent.Anchor_Anchored_ID) & ", " & Integer'Image (Indent.Anchor_Anchored_Delta) & ")";
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
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchored_ID, Delta_Indent.Anchored_Delta);

      when Int =>
         Indent :=
           (Anchored, Delta_Indent.Controlling_Token_Line, Delta_Indent.Anchored_ID,
            Delta_Indent.Anchored_Delta + Indent.Int_Indent);

      when Anchor_Nil =>
         Indent :=
           (Anchor_Anchored,
            Delta_Indent.Controlling_Token_Line,
            Indent.Anchor_Nil_IDs,
            Delta_Indent.Anchored_ID,
            Delta_Indent.Anchored_Delta);

      when Anchor_Int =>
         Indent :=
           (Anchor_Anchored,
            Delta_Indent.Controlling_Token_Line,
            Indent.Anchor_Int_IDs,
            Delta_Indent.Anchored_ID,
            Delta_Indent.Anchored_Delta + Indent.Anchor_Int_Indent);

      when Anchored | Anchor_Anchored =>
         --  Already anchored, as in nested parens.
         null;
      end case;
   end Indent_Apply_Anchored;

   procedure Indent_Apply_Int
     (Indent                 : in out Indent_Type;
      Offset                 : in     Integer;
      Controlling_Token_Line : in     Line_Number_Type)
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

      when Anchor_Nil              =>
         Indent :=
           (Label                  => Anchor_Int,
            Controlling_Token_Line => Controlling_Token_Line,
            Anchor_Int_IDs         => Indent.Anchor_Nil_IDs,
            Anchor_Int_Indent      => Offset);

      when Anchor_Int =>
         if Controlling_Token_Line = Invalid_Line_Number or
           Indent.Controlling_Token_Line = Invalid_Line_Number or
           Controlling_Token_Line /= Indent.Controlling_Token_Line
         then
            Indent.Controlling_Token_Line := Controlling_Token_Line;
            Indent.Anchor_Int_Indent := Indent.Anchor_Int_Indent + Offset;
         end if;

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
            when Anchor_Nil =>
               Result := Integer'Max (Result, Indent.Anchor_Nil_IDs (Indent.Anchor_Nil_IDs.First_Index));
            when Anchor_Int =>
               Result := Integer'Max (Result, Indent.Anchor_Int_IDs (Indent.Anchor_Int_IDs.First_Index));
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
     (Data         : in out Parse_Data_Type'Class;
      Tree         : in     WisiToken.Syntax_Trees.Tree;
      Anchor_Token : in     Syntax_Trees.Valid_Node_Access;
      Offset       : in     Integer)
     return Integer
   is
      Left_Paren_ID  : Token_ID renames Data.Left_Paren_ID;
      Right_Paren_ID : Token_ID renames Data.Right_Paren_ID;

      I : Syntax_Trees.Node_Access := Tree.First_Terminal (Anchor_Token);

      Paren_Count    : Integer    := 0;
      Paren_Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      Text_Begin_Pos : Buffer_Pos := Invalid_Buffer_Pos;
   begin
      loop
         declare
            Tok : constant Augmented_Token := Get_Augmented_Token (Tree, I);
         begin
            if Tok.Base.ID = Left_Paren_ID then
               Paren_Count := Paren_Count + 1;
               if Paren_Count = 1 then
                  Paren_Char_Pos := Tok.Base.Char_Region.First;
               end if;

            elsif Tok.Base.ID = Right_Paren_ID then
               Paren_Count := Paren_Count - 1;

            end if;

            if Tok.Base.Line_Region.First = Tok.Aug.First_Indent_Line then
               Text_Begin_Pos := Tok.Base.Char_Region.First;
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

   procedure Put (Line_Number : in Line_Number_Type; Item : in Indent_Type)
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
         begin
            if Debug_Mode and Ind > 100 then
               --  This is better than hanging Emacs by returning a huge bogus indent.
               raise SAL.Programmer_Error with "indent > 100";
            end if;
            Ada.Text_IO.Put_Line
              ('[' & Indent_Code & Line_Number_Type'Image (Line_Number) & Integer'Image (Ind) & ']');
         end;

      when Anchor_Nil | Anchor_Int | Anchored | Anchor_Anchored =>
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
        (Delete, Invalid_Buffer_Pos, Invalid_Token_ID, Syntax_Trees.Invalid_Node_Index,
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

            Edit_Node : constant Node_Access :=
              --  Can be Invalid_Node_Access when recover fails.
              (case Op.Op is
               when Insert =>
                 (if Op.Ins_Node = Invalid_Node_Access
                  then Invalid_Node_Access
                  else Tree.Next_Shared_Terminal (Op.Ins_Node)),

               when Delete => Op.Del_Node);
            Edit_Pos : constant Buffer_Pos :=
              (if Edit_Node = Invalid_Node_Access
               then Invalid_Buffer_Pos
               else Tree.Base_Token (Edit_Node).Char_Region.First);
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
               Deleted_Region := Deleted_Region and Tree.Base_Token (Op.Del_Node).Char_Region;
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
                           Tok_1 : constant WisiToken.Base_Token := Tree.Base_Token (Last_Deleted.Del_Node);
                           Tok_2 : constant WisiToken.Base_Token := Tree.Base_Token (Op.Del_Node);
                        begin
                           if Tok_1.Char_Region.Last + 1 = Tok_2.Char_Region.First then
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

   procedure Resolve_Anchors (Data : in out Parse_Data_Type)
   is
      Begin_Indent  : Integer renames Data.Begin_Indent;
      Anchor_Indent : array (First_Anchor_ID .. Data.Max_Anchor_ID) of Integer;
   begin
      if Trace_Action > Outline then
         Data.Trace.New_Line;
         Data.Trace.Put_Line ("Begin_Indent: " & Integer'Image (Data.Begin_Indent));
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Data.Trace.Put_Line ("" & Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
         Data.Trace.Put_Line ("resolve anchors");
      end if;

      for Line in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         declare
            Indent : constant Indent_Type := Data.Indents (Line);
         begin
            case Indent.Label is
            when Not_Set =>
               --  Indent not computed, therefore not output.
               null;

            when Int =>
               Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Indent.Int_Indent + Begin_Indent));

            when Anchor_Nil =>
               for I of Indent.Anchor_Nil_IDs loop
                  Anchor_Indent (I) := Begin_Indent;
                  if Trace_Action > Extra then
                     Data.Trace.Put_Line
                       ("anchor line" & Line'Image & " id" & I'Image & " indent" & Anchor_Indent (I)'Image);
                  end if;
               end loop;
               Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Begin_Indent));

            when Anchor_Int =>
               for I of Indent.Anchor_Int_IDs loop
                  Anchor_Indent (I) := Indent.Anchor_Int_Indent + Begin_Indent;
                  if Trace_Action > Extra then
                     Data.Trace.Put_Line
                       ("anchor line" & Line'Image & " id" & I'Image & " indent" & Anchor_Indent (I)'Image);
                  end if;
               end loop;
               Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Indent.Anchor_Int_Indent + Begin_Indent));

            when Anchored =>
               Data.Indents.Replace_Element
                 (Line, (Int, Invalid_Line_Number, Anchor_Indent (Indent.Anchored_ID) + Indent.Anchored_Delta));

            when Anchor_Anchored =>
               declare
                  Temp : constant Integer :=
                    Anchor_Indent (Indent.Anchor_Anchored_ID) + Indent.Anchor_Anchored_Delta;
               begin
                  for I of Indent.Anchor_Anchored_IDs loop
                     Anchor_Indent (I) := Temp;
                     if Trace_Action > Extra then
                        Data.Trace.Put_Line
                          ("anchor line" & Line'Image & " id" & I'Image & " indent" & Anchor_Indent (I)'Image);
                     end if;
                  end loop;
                  Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Temp));
               end;
            end case;
         end;
      end loop;
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
   begin
      Last := Index
        (Source  => Source,
         Pattern => """",
         From    => First + 1);

      if First = 0 or Last = 0 then
         raise Protocol_Error with "at" & Last'Image & ": no '""' found for string";
      end if;

      return Source (First + 1 .. Last - 1);
   end Get_String;

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
     (Command_Line          : in     String;
      Last                  : in out Integer;
      Handle_String_Escapes : in     Boolean)
     return Change_Lists.List
   is
      function Substitute_Escapes (Item : in String) return String
      is begin
         if not Handle_String_Escapes then
            return Item;
         elsif Item'Length = 0 then
            return Item;
         else
            declare
               I : Integer := Item'First;
               J : Integer := Item'First;
               Result : String (Item'Range);
            begin
               loop
                  if Item (I) = '\' and
                    (I < Item'Last and then Item (I + 1) = 'n')
                  then
                     Result (J) := ASCII.LF;
                     I := @ + 2;
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

      procedure Insert_KMN (Change : in Wisi.Change)
      is
         use Parse.KMN_Lists;
         Cur : Cursor := KMN_List.First;

         KMN_Last_Byte : Base_Buffer_Pos := 0; --  Last byte of prev KMN.
         KMN_Last_Char : Base_Buffer_Pos := 0; --  Last char of prev KMN.

         function Max_Byte_Pos (Item : in Wisi.Change) return Buffer_Pos
         is begin
            return Buffer_Pos'Max
              (Item.Begin_Byte_Pos + Base_Buffer_Pos (Item.Deleted_Bytes),
               Item.Inserted_End_Byte_Pos);
         end Max_Byte_Pos;

         function To_KMN (Item : in Wisi.Change) return Parse.KMN
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
            begin
               pragma Assert (KMN_Last_Byte < Change.Begin_Byte_Pos);

               if Max_Byte_Pos (Change) < KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change is entirely within Cur_KMN.Stable_Bytes
                  declare
                     KMN : constant Parse.KMN := To_KMN (Change);
                  begin
                     Cur_KMN.Stable_Bytes := @ - (KMN.Stable_Bytes + KMN.Deleted_Bytes);
                     Cur_KMN.Stable_Chars := @ - (KMN.Stable_Chars + KMN.Deleted_Chars);

                     KMN_List.Insert (Before => Cur, Element => KMN);
                  end;

                  Edit_Text (Change);
                  return;

               elsif Change.Begin_Byte_Pos < KMN_Last_Byte + Cur_KMN.Stable_Bytes + 1 then
                  --  Change starts in Cur_KMN.Stable_Bytes, ends in or after Cur_KMN.Insert.
                  declare
                     KMN : constant Parse.KMN := To_KMN (Change);
                  begin
                     Cur_KMN.Stable_Bytes := @ - (KMN.Stable_Bytes + KMN.Deleted_Bytes);
                     Cur_KMN.Stable_Chars := @ - (KMN.Stable_Chars + KMN.Deleted_Chars);

                     KMN_List.Insert (Before => Cur, Element => KMN);
                  end;

                  Edit_Text (Change);
                  return;

               elsif Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes + 1 or
                 Change.Begin_Byte_Pos <= KMN_Last_Byte + Cur_KMN.Stable_Bytes + Cur_KMN.Deleted_Bytes + 1
               then
                  --  Change starts in or immediately after Cur_KMN inserted or deleted text; merge
                  --  them.

                  if Change.Deleted_Bytes > 0 then
                     declare
                        Next_KMN : Parse.KMN renames KMN_List (Next (Cur));
                        --  FIXME: fails if Change inserts text at end of Source

                        --  Change.Deleted first reduces Cur_KMN.Inserted, then reduces
                        --  Next_KMN.Stable and increases Cur_KMN.Deleted
                     begin
                        if Base_Buffer_Pos (Change.Deleted_Bytes) <= Cur_KMN.Inserted_Bytes then
                           Cur_KMN.Inserted_Bytes := @ - Base_Buffer_Pos (Change.Deleted_Bytes) +
                             (Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos);
                           Cur_KMN.Inserted_Chars := @ - Base_Buffer_Pos (Change.Deleted_Chars) +
                             (Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos);

                        else
                           declare
                              Remaining_Deleted_Bytes : constant Base_Buffer_Pos :=
                                Base_Buffer_Pos (Change.Deleted_Bytes) - Cur_KMN.Inserted_Bytes;
                              Remaining_Deleted_Chars : constant Base_Buffer_Pos :=
                                Base_Buffer_Pos (Change.Deleted_Chars) - Cur_KMN.Inserted_Chars;
                           begin
                              Next_KMN.Stable_Bytes := @ - Remaining_Deleted_Bytes;
                              Next_KMN.Stable_Chars := @ - Remaining_Deleted_Chars;

                              Cur_KMN.Inserted_Bytes := 0 + Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos;
                              Cur_KMN.Inserted_Chars := 0 + Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos;

                              Cur_KMN.Deleted_Bytes := @ + Remaining_Deleted_Bytes;
                              Cur_KMN.Deleted_Chars := @ + Remaining_Deleted_Chars;
                           end;
                        end if;
                     end;
                  else
                     Cur_KMN.Inserted_Bytes := @ + Change.Inserted_End_Byte_Pos - Change.Begin_Byte_Pos;
                     Cur_KMN.Inserted_Chars := @ + Change.Inserted_End_Char_Pos - Change.Begin_Char_Pos;
                  end if;

                  Edit_Text (Change);
                  return;

               else
                  --  Change is entirely after Cur_KMN
                  KMN_Last_Byte := @ + Cur_KMN.Stable_Bytes + Cur_KMN.Inserted_Bytes;
                  KMN_Last_Char := @ + Cur_KMN.Stable_Chars + Cur_KMN.Inserted_Chars;

                  Cur := Next (Cur);

                  if not Has_Element (Cur) then
                     --  Since KMN_List starts with one KMN covering all of Source, we
                     --  should never get here. FIXME: not true if insert text at end of Source!
                     raise SAL.Programmer_Error;
                  end if;
               end if;
            end;
         end loop;
      end Insert_KMN;

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

      --  Start with one KMN with stable region = entire source. Insert_KMN
      --  edits this, leaving it to cover the final stable region or insert
      --  after Source end.
      KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Source_Byte_Last),
          Stable_Chars   => Base_Buffer_Pos (Source_Char_Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      for Change of Changes loop
         Insert_KMN (Change);

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

   procedure Initialize_Partial_Parse
     (Data              : in out Parse_Data_Type;
      Trace             : in     WisiToken.Trace_Access;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     Line_Number_Type;
      End_Line          : in     Line_Number_Type)
   is begin
      Data.Line_Paren_State.Set_First_Last
        (First   => Begin_Line,
         Last    => End_Line);

      Data.Post_Parse_Action := Post_Parse_Action;
      Data.Trace             := Trace;

      case Post_Parse_Action is
      when Navigate | Face =>
         null;
      when Indent =>
         Data.Indents.Set_First_Last
           (First   => Begin_Line,
            Last    => End_Line);
      end case;

      Data.Reset;
   exception
   when E : others =>
      raise SAL.Programmer_Error with "wisi.initialize: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E);
   end Initialize_Partial_Parse;

   procedure Initialize_Full_Parse
     (Data     : in out Parse_Data_Type;
      Trace    : in     WisiToken.Trace_Access;
      End_Line : in     WisiToken.Line_Number_Type)
   is begin
      Data.Trace := Trace;

      Data.Line_Paren_State.Set_First_Last
        (First   => WisiToken.Line_Number_Type'First,
         Last    => End_Line);

      Data.Indents.Set_First_Last
        (First   => WisiToken.Line_Number_Type'First,
         Last    => End_Line);

      Data.Reset;
   end Initialize_Full_Parse;

   procedure Reset_Post_Parse
     (Data                : in out Parse_Data_Type;
      Post_Parse_Action   : in     Post_Parse_Action_Type;
      Action_Region_Bytes : in     WisiToken.Buffer_Region;
      Action_Region_Chars : in     WisiToken.Buffer_Region;
      End_Line            : in     WisiToken.Line_Number_Type;
      Begin_Indent        : in     Integer)
   is begin
      Data.Post_Parse_Action   := Post_Parse_Action;
      Data.Action_Region_Bytes := Action_Region_Bytes;
      Data.Action_Region_Chars := Action_Region_Chars;
      Data.Begin_Indent := Begin_Indent;

      Data.Line_Paren_State.Set_First_Last
        (First   => WisiToken.Line_Number_Type'First,
         Last    => End_Line);

      Data.Indents.Set_First_Last
        (First   => WisiToken.Line_Number_Type'First,
         Last    => End_Line);
   end Reset_Post_Parse;

   overriding procedure Reset (Data : in out Parse_Data_Type)
   is begin
      for S of Data.Line_Paren_State loop
         S := 0;
      end loop;
      Data.Current_Paren_State := 0;

      Data.Navigate_Caches.Finalize;
      Data.Navigate_Caches.Initialize;

      Data.Name_Caches.Finalize;
      Data.Name_Caches.Initialize;

      Data.End_Positions.Clear;

      Data.Face_Caches.Finalize;
      Data.Face_Caches.Initialize;

      for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
         Data.Indents.Replace_Element (I, (Not_Set, Invalid_Line_Number));
      end loop;
      Data.Max_Anchor_ID := First_Anchor_ID - 1;
   end Reset;

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type
   is begin
      return Data.Post_Parse_Action;
   end Post_Parse_Action;

   function Action_Region_Bytes (Data : in Parse_Data_Type) return WisiToken.Buffer_Region
   is begin
      return Data.Action_Region_Bytes;
   end Action_Region_Bytes;

   overriding
   procedure Lexer_To_Augmented
     (Data          : in out Parse_Data_Type;
      Tree          : in out Syntax_Trees.Tree'Class;
      Token         : in     Base_Token;
      Grammar_Token : in     Syntax_Trees.Node_Access)
   is
      use Syntax_Trees;
      use all type Ada.Containers.Count_Type;
      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;
   begin
      if Grammar_Token /= Invalid_Node_Access and then Tree.Augmented (Grammar_Token) /= null then
         pragma Assert
           (Tree.Label (Grammar_Token) = Source_Terminal,
            "FIXME: wisi.adb support multi-line token");

         --  If Token is a non-grammar token following a multi-line grammar
         --  token, Prev_Grammar.Non_Grammar.length is 1, for Token. If Token
         --  is a grammar token, Prev_Grammar.Non_Grammar.length is 0.

         --  FIXME: check for actual multi-line grammar token! (not in Ada; in wisitoken-grammar)
         null;
         --  if (Token.ID < Descriptor.First_Terminal and Tree.Non_Grammar_Const (Grammar_Token).Length = 1)
         --    or
         --    (Token.ID >= Descriptor.First_Terminal and Tree.Non_Grammar_Const (Grammar_Token).Length = 0)
         --  then
         --     declare
         --        Prev_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Grammar_Token);
         --     begin
         --        Prev_Aug.Last_Indent_Line := Token.Line; --  FIXME: set first_indent_line if invalid
         --     end;
         --  end if;
      end if;

      if Token.ID < Descriptor.First_Terminal then
         --  Non-grammar token

         if Grammar_Token /= Invalid_Node_Access then
            declare
               Containing_Non_Grammar : Base_Token_Array_Const_Ref renames Tree.Non_Grammar_Const (Grammar_Token);
               Containing_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Grammar_Token);

               Trailing_Blank : constant Boolean :=
                 Token.ID = Descriptor.New_Line_ID and
                 (Containing_Non_Grammar.Length > 0 and then
                    Containing_Non_Grammar
                      (Containing_Non_Grammar.Last_Index).ID = Descriptor.New_Line_ID);
            begin
               if Tree.Lexer.First and
                 (Token.ID in Data.First_Comment_ID .. Data.Last_Comment_ID or
                    Trailing_Blank)
               then
                  if Containing_Aug.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Containing_Aug.First_Trailing_Comment_Line := Token.Line_Region.First;
                  end if;
                  Containing_Aug.Last_Trailing_Comment_Line  := Token.Line_Region.Last;
               end if;
            end;
         end if;

      else
         --  grammar token
         declare
            Temp : constant Augmented_Access := new Augmented'
              (Deleted                     => False,
               Paren_State                 => 0,
               First_Indent_Line           =>
                 (if Tree.Lexer.First then Token.Line_Region.First else Invalid_Line_Number),
               Last_Indent_Line            =>
                 (if Tree.Lexer.First then Token.Line_Region.Last else Invalid_Line_Number),
               First_Trailing_Comment_Line => Invalid_Line_Number, -- Set by following non_grammar tokens
               Last_Trailing_Comment_Line  => Invalid_Line_Number,
               Inserted_After              => False);
         begin
            --  Data.Line_Paren_State is computed in Initialize_Actions after
            --  parse is finished and error recover insert/delete applied to the
            --  parse stream.

            Tree.Set_Augmented (Grammar_Token, Syntax_Trees.Augmented_Class_Access (Temp));
         end;
      end if;
   end Lexer_To_Augmented;

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
      I         : Node_Access      := Tree.First_Terminal (Tree.Root);
      Last_Line : Line_Number_Type := Invalid_Line_Number;
   begin
      --  Parsing is complete, with error recover insert/delete tokens in
      --  the parse tree. Insert_Token, Delete_Token have been called;
      --  Reduce has _not_ been called. Compute User_Data components that
      --  depend on the corrected parse tree terminal sequence.

      Data.Current_Paren_State := 0;

      loop
         exit when I = Invalid_Node_Access;

         declare
            Token : constant WisiToken.Base_Token := Tree.Base_Token (I);
            Aug   : Augmented_Var_Ref renames To_Augmented_Var_Ref (Tree.Augmented (I));
         begin
            if Last_Line /= Token.Line_Region.First then
               Data.Line_Paren_State (Token.Line_Region.First) := Data.Current_Paren_State;
               Last_Line := Token.Line_Region.First;
            end if;

            Aug.Paren_State := Data.Current_Paren_State;

            if Token.ID = Data.Left_Paren_ID then
               Data.Current_Paren_State := @ + 1;
            elsif Token.ID = Data.Right_Paren_ID then
               Data.Current_Paren_State := @ - 1;
            end if;
         end;

         I := Tree.Next_Terminal (I);
      end loop;

      if Trace_Action > Detail then
         Data.Trace.Put_Line ("Tree.Root.line first, last:" & Image (Tree.Line_Region (Tree.Root)));
         Data.Trace.Put_Line ("Tree.EOI.line:" & Tree.Line_Region (Tree.EOI).First'Image);
         Data.Trace.Put_Line
           ("Data.Line_Paren_State first, last" & Data.Line_Paren_State.First_Index'Image &
              Data.Line_Paren_State.Last_Index'Image);
         Data.Trace.Put_Line ("Line_Paren_State: " & Image (Data.Line_Paren_State));
         --  We print the tree after all actions have executed, to include the
         --  effects of Reduce on augmented in nonterms.

         Data.Trace.Put_Line ("Action_Region_Bytes: " & Image (Data.Action_Region_Bytes));
      end if;
   end Initialize_Actions;

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Syntax_Trees.Node_Access;

      Descriptor : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      Inserted_Before : constant Syntax_Trees.Node_Access := Tree.Next_Shared_Terminal (Inserted_Token);
      --  Invalid_Node_Access when Inserted_Token is inserted before
      --  Wisi_EOI, which is not in the parse stream.

      Before_Token : constant WisiToken.Base_Token :=
        (if Inserted_Before = Syntax_Trees.Invalid_Node_Access
         then Invalid_Token
         else Tree.Base_Token (Inserted_Before));

      --  Set data that allows using Inserted_Token when computing indent.

      Indent_Line : constant Line_Number_Type :=
        (if Inserted_Before = Syntax_Trees.Invalid_Node_Access
         then Invalid_Line_Number
         elsif First (Tree, Inserted_Token)
         then Before_Token.Line_Region.First
         else Invalid_Line_Number);

      --  Initially assume Insert_After = False; see below for True.
      New_Aug : constant Augmented_Access := new Augmented'
        (Deleted                     => False,
         Paren_State                 =>
           (if Inserted_Before = Syntax_Trees.Invalid_Node_Access
            then 0
            else Get_Augmented_Const (Tree, Inserted_Before).Paren_State),
         First_Indent_Line           => Indent_Line,
         Last_Indent_Line            => Indent_Line,
         First_Trailing_Comment_Line => Invalid_Line_Number,
         Last_Trailing_Comment_Line  => Invalid_Line_Number,
         Inserted_After              => False);

      Prev_Terminal : constant Syntax_Trees.Node_Access := Tree.Prev_Terminal (Inserted_Token);
      --  Invalid_Node_Index if Inserted_Token is inserted before first grammar token

      Insert_After : Boolean := False;
   begin
      Tree.Set_Augmented (Inserted_Token, Syntax_Trees.Augmented_Class_Access (New_Aug));

      if Prev_Terminal /= Syntax_Trees.Invalid_Node_Access and
        Inserted_Before /= Syntax_Trees.Invalid_Node_Access and
        First (Tree, Inserted_Token)
      then
         declare
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

            Prev_Token        : constant WisiToken.Base_Token := Tree.Base_Token (Prev_Terminal);
            Prev_Aug          : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Terminal);
            Prev_Non_Grammar  : Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Prev_Terminal);
            Token_Non_Grammar : Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Inserted_Token);

            --  Prev_Non_Grammar must have at least one New_Line, since First
            --  (Inserted_Token) is True. The whitespace after the New_Line is not
            --  given a token, but comments are.
            --
            --  If the first two tokens in Prev_Non_Grammar are both New_Lines,
            --  there is a blank line after the code line (and before any
            --  comments); assume that is the edit point; see
            --  test/ada_mode-interactive_2.adb "A := B \n+C;"
            Blank_Line       : Line_Number_Type   := Invalid_Line_Number;
            Blank_Line_Index : SAL.Base_Peek_Type := 0;
            Comment_Present  : Boolean            := False;

            procedure Check_Non_Grammar
            --  Set Blank_Line, Blank_Line_Index if there is a blank line
            --  immediately after Prev_Terminal.
            is
               I : SAL.Base_Peek_Type := Prev_Non_Grammar.First_Index;
            begin
               loop
                  exit when I > Prev_Non_Grammar.Last_Index;

                  if Prev_Non_Grammar (I).ID /= Descriptor.New_Line_ID then
                     Comment_Present := True;
                     return;
                  end if;

                  if I < Prev_Non_Grammar.Last_Index and then
                    Prev_Non_Grammar (I + 1).ID = Descriptor.New_Line_ID
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

            if Blank_Line /= Invalid_Line_Number then
               --  Insert on blank line; no need to call Insert_After.
               --
               --  test/ada_mode-interactive_2.adb Function_Access_2 and many similar
               --  indent for new code line extending previous code line
               declare
                  New_Non_Grammar : Base_Token_Arrays.Vector;
               begin
                  pragma Assert (Blank_Line_Index = Prev_Non_Grammar.First_Index + 1);

                  for I in Blank_Line_Index .. Prev_Non_Grammar.Last_Index loop
                     New_Non_Grammar.Append (Prev_Non_Grammar (I));
                  end loop;
                  New_Non_Grammar.Append (Token_Non_Grammar);
                  Token_Non_Grammar := New_Non_Grammar;

                  pragma Assert (Tree.Byte_Region (Inserted_Token) = Null_Buffer_Region);

                  New_Aug.Inserted_After := True; --  FIXME: need tri-state?

                  Tree.Update
                    (Inserted_Token,
                     Byte_Region => Null_Buffer_Region,
                     Char_Region => (First | Last => Prev_Non_Grammar (Blank_Line_Index).Char_Region.First + 1),
                     Line_Region => (First | Last => Blank_Line));
                  Tree.Set_Line_Last (Inserted_Token, Blank_Line);

                  Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Prev_Non_Grammar.First_Index);
                  Tree.Set_Line_Last (Prev_Terminal, Blank_Line - 1);

                  New_Aug.First_Indent_Line := Blank_Line;
                  New_Aug.Last_Indent_Line  := Blank_Line;

                  if Prev_Aug.First_Trailing_Comment_Line <= Prev_Aug.Last_Trailing_Comment_Line then
                     New_Aug.First_Trailing_Comment_Line := Prev_Aug.First_Trailing_Comment_Line + 1;
                     New_Aug.Last_Trailing_Comment_Line  := Prev_Aug.Last_Trailing_Comment_Line;
                  end if;

                  Prev_Aug.First_Trailing_Comment_Line := Invalid_Line_Number;
                  Prev_Aug.Last_Trailing_Comment_Line  := Invalid_Line_Number;

                  if Trace_Action > WisiToken.Detail then
                     Data.Trace.Put_Line
                       ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
                          " after " & Tree.Image (Prev_Terminal, Node_Numbers => True) &
                          " on blank line" & Blank_Line'Image);
                  end if;
               end;

            else
               Insert_After := Parse_Data_Type'Class (Data).Insert_After
                 (Tree, Inserted_Token, Inserted_Before,
                  Comment_Present      => Comment_Present,
                  Insert_On_Blank_Line => False); --  FIXME: delete arg

               if Insert_After then
                  New_Aug.Inserted_After := True;

                  if Trace_Action > WisiToken.Detail then
                     Data.Trace.Put_Line
                       ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
                          " after " & Tree.Image (Prev_Terminal, Node_Numbers => True));
                  end if;

                  if Length (Prev_Token.Char_Region) = 0 then
                     raise SAL.Programmer_Error with "Prev_Token char_region = 0: " & Tree.Image (Prev_Terminal);

                  else
                     Tree.Update
                       (Inserted_Token,
                        Byte_Region => Null_Buffer_Region,
                        Char_Region => (First | Last => Prev_Token.Char_Region.Last),
                        Line_Region => (First | Last => Prev_Token.Line_Region.First));

                     Token_Non_Grammar := Prev_Non_Grammar;

                     New_Aug.First_Indent_Line := Invalid_Line_Number;
                     New_Aug.Last_Indent_Line  := Invalid_Line_Number;

                     Prev_Non_Grammar := WisiToken.Base_Token_Arrays.Empty_Vector;

                     New_Aug.First_Trailing_Comment_Line := Prev_Aug.First_Trailing_Comment_Line;
                     New_Aug.Last_Trailing_Comment_Line  := Prev_Aug.Last_Trailing_Comment_Line;

                     Prev_Aug.First_Trailing_Comment_Line := Invalid_Line_Number;
                     Prev_Aug.Last_Trailing_Comment_Line  := Invalid_Line_Number;
                  end if;
               end if;
            end if;
         end;
      end if;

      if not Insert_After and Trace_Action > WisiToken.Detail then
         Data.Trace.Put_Line
           ("insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
              " before " & Tree.Image (Inserted_Before, Node_Numbers => True));
      end if;

      if First (Tree, Inserted_Token) and
        (Inserted_Before /= Syntax_Trees.Invalid_Node_Access and then
           Tree.Line_Region (Inserted_Token).First = Before_Token.Line_Region.First)
      then
         declare
            Before_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Inserted_Before);
         begin
            Before_Aug.First_Indent_Line := Invalid_Line_Number;
            Before_Aug.Last_Indent_Line  := Invalid_Line_Number;
         end;
      end if;
   end Insert_Token;

   overriding
   procedure Delete_Token
     (Data          : in out Parse_Data_Type;
      Tree          : in     Syntax_Trees.Tree'Class;
      Deleted_Token : in     Syntax_Trees.Valid_Node_Access;
      Prev_Token    : in     Syntax_Trees.Node_Access)
   is
      use all type Syntax_Trees.Node_Access;
      use all type Ada.Containers.Count_Type;

      Deleted_Tok         : constant WisiToken.Base_Token := Tree.Base_Token (Deleted_Token);
      Deleted_Aug         : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Deleted_Token);
      Deleted_Non_Grammar : WisiToken.Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Deleted_Token);

      Next_Token : constant Syntax_Trees.Node_Access :=
        (if Prev_Token = Syntax_Trees.Invalid_Node_Access
         then Tree.First_Terminal (Tree.Root)
         else Tree.Next_Terminal (Prev_Token));

      Prev_Modified : Boolean := False;
   begin
      if Deleted_Aug.Deleted then
         --  This can happen if error recovery screws up.
         if Trace_Action > WisiToken.Detail then
            Data.Trace.Put_Line ("delete token again; ignored " & Tree.Image (Deleted_Token, Node_Numbers => True));
         end if;
         return;
      end if;
      if Trace_Action > WisiToken.Detail then
         Data.Trace.Put_Line ("delete token " & Tree.Image (Deleted_Token, Node_Numbers => True));
      end if;

      Deleted_Aug.Deleted := True;

      if Deleted_Non_Grammar.Length > 0 then
         --  Move Non_Grammar to previous non-deleted token

         if Prev_Token /= Syntax_Trees.Invalid_Node_Access then
            declare
               Prev_Non_Grammar : WisiToken.Base_Token_Array_Var_Ref renames Tree.Non_Grammar_Var (Prev_Token);
               Aug : constant Syntax_Trees.Augmented_Class_Access := Tree.Augmented (Prev_Token);
            begin
               if Aug = null then
                  raise SAL.Programmer_Error with "null augmented: " & Tree.Image
                    (Prev_Token, Node_Numbers => True);
               end if;
               declare
                  Prev_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Token);
               begin
                  Prev_Non_Grammar.Append (Deleted_Non_Grammar);
                  Prev_Modified := True;

                  if Deleted_Aug.First_Trailing_Comment_Line /= Invalid_Line_Number then
                     if Prev_Aug.First_Trailing_Comment_Line = Invalid_Line_Number then
                        Prev_Aug.First_Trailing_Comment_Line := Deleted_Aug.First_Trailing_Comment_Line;
                     end if;
                     Prev_Aug.Last_Trailing_Comment_Line  := Deleted_Aug.Last_Trailing_Comment_Line;
                  end if;
               end;
            end;
            --  FIXME: else move to Tree.Leading_Non_Grammar
         end if;
      end if;

      Update_Indent_Lines (Tree, Deleted_Tok, Deleted_Aug, Prev_Token, Next_Token, Prev_Modified);
   end Delete_Token;

   overriding
   procedure Delete_Token
     (Data      : in out Parse_Data_Type;
      Tree      : in     Syntax_Trees.Tree'Class;
      To_Delete : in     Syntax_Trees.Terminal_Ref)
   is
      pragma Unreferenced (Data);

      Deleted_Tok : constant WisiToken.Base_Token     := Tree.Base_Token (To_Delete.Node);
      Deleted_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, To_Delete.Node);
      Prev_Token  : constant Syntax_Trees.Node_Access := Tree.Prev_Terminal (To_Delete).Node;
      Next_Token  : constant Syntax_Trees.Node_Access := Tree.Next_Terminal (To_Delete).Node;

      Prev_Modified : Boolean := False;
   begin
      --  Edit_Tree is about to delete To_Delete. To_Delete.Non_Grammar will be scanned.

      --  We don't set Aug.Deleted True; that's for error recovery.

      Update_Indent_Lines (Tree, Deleted_Tok, Deleted_Aug, Prev_Token, Next_Token, Prev_Modified);
   end Delete_Token;

   overriding
   procedure Reduce
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree'Class;
      Nonterm : in     Syntax_Trees.Valid_Node_Access)
   is
      Nonterm_Aug : Augmented_Access := Augmented_Access (Tree.Augmented (Nonterm));

      Trailing_Comment_Done : Boolean := False;
   begin
      if Nonterm_Aug = null then
         Nonterm_Aug := new Augmented'(others => <>);
         Tree.Set_Augmented (Nonterm, Syntax_Trees.Augmented_Class_Access (Nonterm_Aug));
      else
         Nonterm_Aug.all := (others => <>);
      end if;

      for Child of reverse Tree.Children (Nonterm) loop
         --  'reverse' to find token containing trailing comments; last
         --  non-empty token.

         if Tree.Augmented (Child) /= null then
            --  Augmented is invalid if Child is outside Action_Region
            declare
               Token : constant Augmented_Token := Get_Augmented_Token (Tree, Child);
            begin
               if Data.Post_Parse_Action = Indent then
                  if Token.Aug.First_Indent_Line /= Invalid_Line_Number then
                     Nonterm_Aug.First_Indent_Line := Token.Aug.First_Indent_Line;
                  elsif Trailing_Comment_Done and Token.Aug.First_Trailing_Comment_Line /= Invalid_Line_Number then
                     Nonterm_Aug.First_Indent_Line := Token.Aug.First_Trailing_Comment_Line;
                  end if;

                  if Nonterm_Aug.Last_Indent_Line = Invalid_Line_Number then
                     if Trailing_Comment_Done and Token.Aug.Last_Trailing_Comment_Line /= Invalid_Line_Number then
                        Nonterm_Aug.Last_Indent_Line := Token.Aug.Last_Trailing_Comment_Line;
                     elsif Token.Aug.Last_Indent_Line /= Invalid_Line_Number then
                        Nonterm_Aug.Last_Indent_Line := Token.Aug.Last_Indent_Line;
                     end if;
                  end if;

                  if not Trailing_Comment_Done then
                     Nonterm_Aug.First_Trailing_Comment_Line := Token.Aug.First_Trailing_Comment_Line;
                     Nonterm_Aug.Last_Trailing_Comment_Line  := Token.Aug.Last_Trailing_Comment_Line;
                     Trailing_Comment_Done := True;
                  end if;
               end if;

               Nonterm_Aug.Paren_State := Token.Aug.Paren_State;
            end;
         end if;
      end loop;
   end Reduce;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Statement_Param_Array)
   is
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
         if not (Pair.Index in Tokens'Range) then
            raise Fatal_Error with Tree.Error_Message
              (Nonterm,
               "wisi-statement-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) &
                 " token index" & SAL.Peek_Type'Image (Pair.Index) &
                 " not in tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
                 SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");

         elsif Overlaps (Tree.Char_Region (Tokens (Pair.Index)), Data.Action_Region_Chars) then
            declare
               use all type Syntax_Trees.Node_Label;
               Token  : constant Base_Token := Tree.Base_Token
                 ((if Pair.Class = Statement_End and then
                     Tree.Label (Tokens (Pair.Index)) = Syntax_Trees.Nonterm
                   then Tree.Last_Terminal (Tokens (Pair.Index))
                   else Tokens (Pair.Index)));

               Cache_Pos : constant Buffer_Pos         := Token.Char_Region.First;
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
                      ID             => Token.ID,
                      Length         => Length (Token.Char_Region),
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
                     Containing_Pos     := (True, Token.Char_Region.First);

                     --  Set containing on all contained caches
                     declare
                        use Navigate_Cache_Trees;
                        Iterator : constant Navigate_Cache_Trees.Iterator := Data.Navigate_Caches.Iterate;

                        Nonterm_Tok : constant Base_Token := Tree.Base_Token (Nonterm);

                        Cursor : Navigate_Cache_Trees.Cursor :=
                          (if Length (Nonterm_Tok.Char_Region) = 0
                           then No_Element
                           else Find_In_Range
                             (Iterator, Ascending, Nonterm_Tok.Char_Region.First + 1, -- don't set containing on start
                              Nonterm_Tok.Char_Region.Last));
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
                              exit when Nonterm_Tok.Char_Region.Last < Cache.Pos + 1;
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
            --  Token.Byte_Region is null
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
      Tokens  : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array;
      Name    : in     WisiToken.Positive_Index_Type)
   is
      use all type Syntax_Trees.Node_Label;
   begin
      if not (Name in Tokens'Range) then
         raise Grammar_Error with Tree.Error_Message
           (Tokens (Tokens'First),
            "wisi-name-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) & " name (" &
              Trimmed_Image (Name) & ") not in Tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
              SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");
      end if;

      if Tree.Is_Virtual (Tokens (Name)) then
         --  Virtual tokens don't appear in the actual buffer, so we can't set
         --  a text property on them.
         return;
      elsif not Overlaps (Tree.Char_Region (Tokens (Name)), Data.Action_Region_Chars) then
         return;
      end if;

      pragma Assert (Tree.Label (Tokens (Name)) in Source_Terminal | Syntax_Trees.Nonterm);

      declare
         use Name_Cache_Trees;
         Name_Token : constant Base_Token := Tree.Base_Token (Tokens (Name));
         Cursor     : constant Name_Cache_Trees.Cursor := Find
           (Data.Name_Caches.Iterate, Name_Token.Char_Region.First,
            Direction => Name_Cache_Trees.Unknown);
      begin
         if Has_Element (Cursor) then
            raise Fatal_Error with Tree.Error_Message
              (Tokens (Name), Tree.Image
                 (Node         => Tokens (Name),
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
                       (Tokens (Name),
                        Node_Numbers => Trace_Action > Extra,
                        RHS_Index    => Trace_Action > Extra));
            end if;

            if Name_Token.Char_Region /= Null_Buffer_Region then
               Data.Name_Caches.Insert (Name_Token.Char_Region);
            end if;
         end if;
      end;
   end Name_Action;

   procedure Motion_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
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
         if Overlaps (Tree.Char_Region (Tokens (Param.Index)), Data.Action_Region_Chars) then
            declare
               use all type Syntax_Trees.Node_Label;
               Token     : constant Base_Token    := Tree.Base_Token (Tokens (Param.Index));
               Region    : constant Buffer_Region := Token.Char_Region;
               Cache_Cur : Cursor;
               Skip      : Boolean;
               Done      : Boolean                := False;
            begin
               loop
                  Skip := False;

                  case Tree.Label (Tokens (Param.Index)) is
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
                          (Tokens (Param.Index),
                           Message   => "wisi-motion-action: token " &
                             WisiToken.Image (Token.ID, Descriptor) &
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
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      use Face_Cache_Trees;

      Iter       : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur  : Cursor;
      Suffix_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tokens (Param.Index)), Data.Action_Region_Chars) then
            if Trace_Action > Outline then
               Data.Trace.Put_Line
                 ("face_apply_action: " & Image (Tree.Char_Region (Tokens (Param.Index))) &
                    " " & Param.Prefix_Face'Image & " " & Param.Suffix_Face'Image);
            end if;

            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (Param.Index));
            begin
               Cache_Cur := Find (Iter, Token.Char_Region.First, Direction => Ascending);
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
                                Inside (Suf_Cache.Char_Region.First, Token.Char_Region)
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
            end;
         end if;
      end loop;
   end Face_Apply_Action;

   procedure Face_Apply_List_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Apply_Param_Array)
   is
      pragma Unreferenced (Nonterm);
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tokens (Param.Index)), Data.Action_Region_Chars) then
            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (Param.Index));
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token.Char_Region.Last;
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
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Mark_Param_Array)
   is
      pragma Unreferenced (Nonterm);

      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for Param of Params loop
         if Overlaps (Tree.Char_Region (Tokens (Param.Index)), Data.Action_Region_Chars) then
            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (Param.Index));
            begin
               Cache_Cur := Find (Iter, Token.Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Char_Region.Last + 1, Token.Char_Region.Last);
                     To_Delete : Buffer_Pos_Lists.List;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Data.Face_Caches (Other_Cur).Char_Region.First > Token.Char_Region.Last;
                        To_Delete.Append (Data.Face_Caches (Other_Cur).Char_Region.First);
                        Other_Cur := Next (Iter, Other_Cur);
                     end loop;

                     Cache.Class            := Param.Class;
                     Cache.Char_Region.Last := Token.Char_Region.Last;

                     for Face of To_Delete loop
                        Data.Face_Caches.Delete (Face);
                     end loop;
                  end;
               else
                  Data.Face_Caches.Insert ((Token.Char_Region, Param.Class, (Set => False)));
               end if;
            end;
         end if;
      end loop;
   end Face_Mark_Action;

   procedure Face_Remove_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Face_Remove_Param_Array)
   is
      pragma Unreferenced (Nonterm);
      use Face_Cache_Trees;

      Iter      : constant Iterator := Data.Face_Caches.Iterate;
      Cache_Cur : Cursor;
   begin
      for I of Params loop
         if Overlaps (Tree.Char_Region (Tokens (I)), Data.Action_Region_Chars) then
            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (I));
               To_Delete : Buffer_Pos_Lists.List;
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token.Char_Region.Last;
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
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Indent_Param_Array)
   is begin
      if Trace_Action > Outline then
         Data.Trace.Put_Line
           ("indent_action_0: " & Tree.Image (Nonterm, RHS_Index => True, Augmented => True, Line_Numbers => True));
      end if;

      for I in Tokens'Range loop
         if Overlaps (Tree.Char_Region (Tokens (I)), Data.Action_Region_Chars) and
           I in Params'Range -- in some translated EBNF, not every token has an indent param
         then
            declare
               use all type SAL.Base_Peek_Type;
               Tree_Token        : constant Syntax_Trees.Valid_Node_Access := Tokens (I);
               Token             : constant Augmented_Token                := Get_Augmented_Token (Tree, Tree_Token);
               Pair              : Indent_Pair renames Params (I);
               Code_Delta        : Delta_Type;
               Controlling_Token : Syntax_Trees.Node_Access;
               Comment_Param     : Indent_Param;
               Comment_Param_Set : Boolean                                 := False;
               Comment_Delta     : Delta_Type;
            begin
               if Trace_Action > Detail then
                  Data.Trace.Put_Line
                    ("indent_action_0 code: " & Tree.Image (Tree_Token, Line_Numbers => True) & ": " &
                       Image (Pair.Code_Delta));
               end if;

               if Token.Aug.First_Indent_Line /= Invalid_Line_Number then
                  Code_Delta := Indent_Compute_Delta
                    (Data, Tree, Nonterm, Tokens, Pair.Code_Delta, Tree_Token, Indenting_Comment => False);

                  Indent_Token_1 (Data, Tree, Token, Code_Delta, Indenting_Comment => False);
               end if;

               if Token.Aug.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  if Pair.Comment_Present then
                     Comment_Param     := Pair.Comment_Delta;
                     Controlling_Token := Tokens (I);
                     Comment_Param_Set := True;

                  elsif I < Tokens'Last then
                     Comment_Param     := Params (I + 1).Code_Delta;
                     Controlling_Token := Tokens (I + 1);
                     Comment_Param_Set := True;

                  end if;

                  if Comment_Param_Set then
                     if Trace_Action > Detail then
                        Data.Trace.Put_Line
                          ("indent_action_0 comment: " & Tree.Image (Controlling_Token, Line_Numbers => True) & ": " &
                             Image (Comment_Param));
                     end if;

                     Comment_Delta := Indent_Compute_Delta
                       (Data, Tree, Nonterm, Tokens, Comment_Param, Controlling_Token, Indenting_Comment => True);

                     Indent_Token_1 (Data, Tree, Token, Comment_Delta, Indenting_Comment => True);
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
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean;
      Delta_1           : in     Simple_Indent_Param;
      Delta_2           : in     Simple_Indent_Param;
      Label             : in     Hanging_Label)
     return Delta_Type
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      Indenting_Token : constant Augmented_Token := Get_Augmented_Token (Tree, Tree_Indenting);

      function Compute_Hanging_2 return Simple_Indent_Param
      is begin
         --  WORKAROUND: GNAT Commmunity 2020 gives a bogus compile error when
         --  we try to inline this with an if_expression.
         if Indenting_Token.Base.Line_Region.First = Indenting_Token.Aug.First_Indent_Line
         then
            return Add_Simple_Indent_Param (Delta_1, Delta_2);
         else
            return Delta_2;
         end if;
      end Compute_Hanging_2;

   begin
      if Indenting_Comment then
         --  Indenting the comment before Tree_Indenting, which has hanging
         --  indent. Tree_Indenting must be first on a following line, so
         --  Delta_1 applies.
         return Indent_Compute_Delta
           (Data, Tree, Nonterm, Tokens, (Simple, Delta_1), Tree_Indenting, Indenting_Comment);
      else
         return Result : Delta_Type :=
           (Hanging,
            Hanging_First_Line  => Indenting_Token.Base.Line_Region.First,
            Hanging_Delta_1     => Indent_Compute_Delta
              (Data, Tree, Nonterm, Tokens,
               (Simple,
                (if Indenting_Token.Base.Line_Region.First = Indenting_Token.Aug.First_Indent_Line
                 then Delta_1
                 else (Label => None))),
               Tree_Indenting, Indenting_Comment).Simple_Delta,
            Hanging_Delta_2     =>
              Indent_Compute_Delta
                (Data, Tree, Nonterm, Tokens,
                 (Simple,
                  (case Label is
                   when Hanging_0 => Delta_2,
                   when Hanging_1 =>
                     (if Indenting_Token.Base.Line_Region.First = Indenting_Token.Aug.First_Indent_Line
                      then Delta_2 else Delta_1),
                   when Hanging_2 =>
                      Compute_Hanging_2)),
                 Tree_Indenting, Indenting_Comment).Simple_Delta)
         do
            --  Controlling_Token_Line for Delta_2 is the first non-comment
            --  line indented by Delta_2.
            if Label = Hanging_1 and
              Indenting_Token.Base.Line_Region.First /= Indenting_Token.Aug.First_Indent_Line
            then
               --  Only using Delta_1
               null;
            else
               for Line in Indenting_Token.Aug.First_Indent_Line +
                 (if Indenting_Token.Base.Line_Region.First = Indenting_Token.Aug.First_Indent_Line then 1 else 0)
                 .. Indenting_Token.Aug.Last_Indent_Line
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
     (Tree       : in WisiToken.Syntax_Trees.Tree;
      Label      : in Query_Label;
      Char_Point : in WisiToken.Buffer_Pos)
   is
      use Syntax_Trees;
   begin
      case Label is
      when Nonterm =>
         declare
            Terminal : constant Node_Access := Tree.Find_Terminal (Char_Point);
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
      use all type Ada.Containers.Count_Type;
      use all type WisiToken.Syntax_Trees.Node_Access;

      Tree : WisiToken.Syntax_Trees.Tree renames Parser.Tree;

      Last_Char_Pos : constant Buffer_Pos       := Tree.Char_Region (Tree.EOI).Last;
      Last_Line     : constant Line_Number_Type := Tree.Line_Region (Tree.EOI).First;
   begin
      if Trace_Action > Outline then
         if Trace_Action > Extra then
            Parser.Trace.Put_Line
              (Parser.Tree.Image
                 (Children     => True,
                  Non_Grammar  => True,
                  Augmented    => True,
                  Line_Numbers => True));
            Parser.Trace.New_Line;
         end if;
         Parser.Trace.Put_Line
           ("last_char_pos:" & Buffer_Pos'Image (Last_Char_Pos + 1) &
              " last_line:" & Line_Number_Type'Image (Last_Line));
      end if;

      --  +1 to match Emacs region
      Ada.Text_IO.Put_Line ('[' & End_Code & Buffer_Pos'Image (Last_Char_Pos + 1) & ']');

      case Data.Post_Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            if Inside (Cache.Pos, Data.Action_Region_Chars) then
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
         Resolve_Anchors (Data);

         if Trace_Action > Outline then
            Parser.Trace.Put_Line ("indent leading non_grammar");
         end if;
         declare
            Non_Grammar : WisiToken.Base_Token_Arrays.Vector renames Parser.Tree.Leading_Non_Grammar;
         begin
            if Non_Grammar.Length > 0 then
               for Line in Non_Grammar (Non_Grammar.First_Index).Line_Region.First ..
                 Non_Grammar (Non_Grammar.Last_Index).Line_Region.Last
               loop
                  if Inside (Tree.Line_Begin_Char_Pos (Line), Data.Action_Region_Chars)
                  then
                     Put (Line, (Int, Invalid_Line_Number, Data.Begin_Indent));
                  end if;
               end loop;
            end if;
         end;

         --  It may be that not all lines in Data.Indents were parsed.
         if Trace_Action > Outline then
            Parser.Trace.Put_Line ("indent grammar");
         end if;
         for Line in Data.Indents.First_Index .. Line_Number_Type'Min (Data.Indents.Last_Index, Last_Line) loop
            if Inside (Tree.Line_Begin_Char_Pos (Line), Data.Action_Region_Chars) then
               Put (Line, Data.Indents (Line));
            end if;
         end loop;
      end case;
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
      use all type Syntax_Trees.Node_Access;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      function Safe_Pos (Token : in Syntax_Trees.Recover_Token) return Base_Buffer_Pos
      is
         Result : Base_Buffer_Pos := Syntax_Trees.Name (Token).First;
      begin
         if Result = Invalid_Buffer_Pos then
            Result := 0;
         end if;
         return Result;
      end Safe_Pos;

      function Safe_Pos (Token : in Syntax_Trees.Node_Access) return Base_Buffer_Pos
      is begin
         if Token = Syntax_Trees.Invalid_Node_Access then
            return 0; --  elisp interprets this as invalid buffer pos, and it takes fewer bytes to transmit.
         else
            return Result : Base_Buffer_Pos := Tree.Char_Region (Token).First do
               if Result = Invalid_Buffer_Pos then
                  Result := 0;
               end if;
            end return;
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
                     Base_Buffer_Pos'Image (Safe_Pos (Item.Status.Begin_Name)) &
                       Base_Buffer_Pos'Image (Safe_Pos (Item.Status.End_Name)) & " """ &
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

   function Image (Item : in Simple_Delta_Type) return String
   is begin
      return "(" & Trimmed_Image (Item.Controlling_Token_Line) & ": " & Simple_Delta_Labels'Image (Item.Label) &
        (case Item.Label is
         when None => "",
         when Int => Integer'Image (Item.Int_Delta),
         when Anchored => Integer'Image (Item.Anchored_ID) & Integer'Image (Item.Anchored_Delta))
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
      Anchor_Token : in Base_Token;
      Offset       : in Integer)
     return Integer
   is
      use all type Syntax_Trees.Node_Access;
      Line_Begin_Token : constant Syntax_Trees.Node_Access := Tree.Line_Begin_Token (Anchor_Token.Line_Region.First);
   begin
      return Offset + Integer
        (Anchor_Token.Char_Region.First -
           (if Line_Begin_Token = WisiToken.Syntax_Trees.Invalid_Node_Access
            then 0
            else Tree.Char_Region (Line_Begin_Token).First));
   end Current_Indent_Offset;

   function First
     (Tree  : in Syntax_Trees.Tree'Class;
      Token : in Syntax_Trees.Node_Access)
     return Boolean
   is
      use all type Syntax_Trees.Node_Access;
   begin
      if Token = Syntax_Trees.Invalid_Node_Access then
         return False;
      else
         declare
            First_Token : constant Syntax_Trees.Node_Access :=
              (if Tree.Root = Syntax_Trees.Invalid_Node_Access
               then Tree.Line_Begin_Token (Tree.Line_Region (Token).First, Tree.Shared_Stream)
               else Tree.Line_Begin_Token (Tree.Line_Region (Token).First));
         begin
            return First_Token = Token;
         end;
      end if;
   end First;

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.Aug.First_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Base.Line_Region.First
            else Token.Aug.First_Trailing_Comment_Line)
         else
           (if Token.Aug.First_Indent_Line = Invalid_Line_Number
            then Token.Base.Line_Region.First
            else Token.Aug.First_Indent_Line));
   end First_Line;

   function Get_Augmented_Const
     (Tree  : in Syntax_Trees.Tree'Class;
      Token : in WisiToken.Syntax_Trees.Valid_Node_Access)
     return Augmented_Const_Ref
   is begin
      return To_Augmented_Const_Ref (Tree.Augmented (Token));
   end Get_Augmented_Const;

   function Get_Augmented_Var
     (Tree  : in Syntax_Trees.Tree'Class;
      Token : in Syntax_Trees.Valid_Node_Access)
     return Augmented_Var_Ref
   is begin
      return To_Augmented_Var_Ref (Tree.Augmented (Token));
   end Get_Augmented_Var;

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
     (Data           : in out Parse_Data_Type;
      Anchor_Line    : in     Line_Number_Type;
      Indenting_Line : in     Line_Number_Type;
      Offset         : in     Integer)
     return Delta_Type
   is
      use Anchor_ID_Vectors;
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent    : Indent_Type      := Data.Indents (Anchor_Line);

      --  IMPROVEME: if Anchor_Line already has an ID, use it.
      Anchor_ID : constant Integer := 1 + Max_Anchor_ID (Data, Anchor_Line, Indenting_Line);
   begin
      Data.Max_Anchor_ID := Integer'Max (Data.Max_Anchor_ID, Anchor_ID);

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchor_Nil, Invalid_Line_Number, To_Vector (Anchor_ID, 1));

         if Trace_Action > Extra then
            Data.Trace.Put_Line
              ("indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
         end if;

      when Int =>
         Indent := (Anchor_Int, Invalid_Line_Number, To_Vector (Anchor_ID, 1), Indent.Int_Indent);

         if Trace_Action > Extra then
            Data.Trace.Put_Line
              ("indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
         end if;

      when Anchor_Nil =>
         Indent.Anchor_Nil_IDs := Anchor_ID & Indent.Anchor_Nil_IDs;

      when Anchor_Int =>
         Indent.Anchor_Int_IDs := Anchor_ID & Indent.Anchor_Int_IDs;

      when Anchored =>
         Indent :=
           (Anchor_Anchored, Indent.Controlling_Token_Line, To_Vector (Anchor_ID, 1), Indent.Anchored_ID,
            Indent.Anchored_Delta);

      when Anchor_Anchored =>
         Indent.Anchor_Anchored_IDs := Anchor_ID & Indent.Anchor_Anchored_IDs;
      end case;

      Data.Indents.Replace_Element (Anchor_Line, Indent);

      return (Simple, (Anchored, Anchor_Line, Anchor_ID, Offset));
   end Indent_Anchored_2;

   function Indent_Compute_Delta
     (Data              : in out Parse_Data_Type'Class;
      Tree              : in     Syntax_Trees.Tree;
      Nonterm           : in     Syntax_Trees.Valid_Node_Access;
      Tokens            : in     Syntax_Trees.Valid_Node_Access_Array;
      Param             : in     Indent_Param;
      Tree_Indenting    : in     Syntax_Trees.Valid_Node_Access;
      Indenting_Comment : in     Boolean)
     return Delta_Type
   is
      Indenting_Token : constant Augmented_Token := Get_Augmented_Token (Tree, Tree_Indenting);
   begin
      --  Evaluate wisi-anchored*, wisi-hanging*.
      case Param.Label is
      when Simple =>
         case Param.Param.Label is
         when None =>
            return Null_Delta;

         when Block =>
            return (Simple, (Int, Invalid_Line_Number, Param.Param.Int_Delta));

         when Int =>
            return (Simple, (Int, Indenting_Token.Base.Line_Region.First, Param.Param.Int_Delta));

         when Simple_Param_Anchored =>
            --  [2] wisi-anchored, wisi-anchored%
            declare
               Anchor_Node  : constant Syntax_Trees.Valid_Node_Access := Tokens (Param.Param.Anchored_Index);
               Anchor_Token : constant Augmented_Token := Get_Augmented_Token (Tree, Anchor_Node);
            begin
               return Indent_Anchored_2
                 (Data,
                  Anchor_Line =>
                    (if Indenting_Comment then
                        --  FIXME: why use aug.last_indent_line for comment?
                       (if Anchor_Token.Aug.Last_Indent_Line = Invalid_Line_Number
                        then Anchor_Token.Base.Line_Region.First
                        else Anchor_Token.Aug.Last_Indent_Line)
                     else Anchor_Token.Base.Line_Region.First),
                  Indenting_Line => Tree.Line_Region (Tree_Indenting).Last,
                  Offset         =>
                    (case Simple_Param_Anchored'(Param.Param.Label) is
                     when Anchored_0 =>
                        --  test/ada_mode-interactive_2.adb 'if (A and B -- Comment 1'
                        Current_Indent_Offset (Tree, Anchor_Token.Base, Param.Param.Anchored_Delta),
                     when Anchored_1 =>
                        Paren_In_Anchor_Line (Data, Tree, Anchor_Node, Param.Param.Anchored_Delta)));
            end;

         when Language =>
            return Param.Param.Function_Ptr
              (Data, Tree, Nonterm, Tokens, Tree_Indenting, Indenting_Comment, Param.Param.Args);
         end case;

      when Hanging_Label =>
         return Indent_Hanging_1
           (Data, Tree, Nonterm, Tokens, Tree_Indenting, Indenting_Comment, Param.Hanging_Delta_1,
            Param.Hanging_Delta_2, Param.Label);
      end case;
   end Indent_Compute_Delta;

   procedure Indent_Token_1
     (Data              : in out Parse_Data_Type;
      Tree              : in     Syntax_Trees.Tree;
      Indenting_Token   : in     Augmented_Token;
      Delta_Indent      : in     Delta_Type;
      Indenting_Comment : in     Boolean)
   --  Apply Delta_Indent to Indenting_Token
   is
      use all type WisiToken.Syntax_Trees.Node_Access;
      Descriptor  : WisiToken.Descriptor renames Tree.Lexer.Descriptor.all;

      First_Line : constant Line_Number_Type := Wisi.First_Line (Indenting_Token, Indenting_Comment);
      Last_Line  : constant Line_Number_Type := Wisi.Last_Line (Indenting_Token, Indenting_Comment);
   begin
      if Trace_Action > Detail then
         Data.Trace.Put_Line
           ("indent_token_1:      " &
              Image (Indenting_Token.Base, Descriptor) & " " & Image (Delta_Indent) &
              Line_Number_Type'Image (First_Line) & " .." & Line_Number_Type'Image (Last_Line) &
              (if Indenting_Comment then " comment" else ""));
      end if;

      for Line in First_Line .. Last_Line loop
         if Data.Indent_Comment_Col_0 then
            declare
               use all type Ada.Containers.Count_Type;
               use all type Ada.Text_IO.Count;
               Indent : Boolean := True;
            begin
               if Tree.Leading_Non_Grammar.Length > 0 and then
                 Line < Tree.Leading_Non_Grammar (Tree.Leading_Non_Grammar.Last_Index).Line_Region.First
               then
                  --  Line is before the first grammar token. We may be doing a partial
                  --  parse where the initial indent is non-zero, so we still have to
                  --  check for column 0.
                  for Tok of Tree.Leading_Non_Grammar loop
                     if Tok.Line_Region.First = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       WisiToken.Column (Tok, Tree.Line_Begin_Char_Pos (Line)) = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;

               else
                  declare
                     Containing : constant Syntax_Trees.Node_Access := Tree.Find_New_Line (Line);
                  begin
                     if Containing /= Syntax_Trees.Invalid_Node_Access then
                        for Tok of Tree.Non_Grammar_Const (Containing) loop
                           if Tok.Line_Region.First = Line and then
                             Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                             WisiToken.Column (Tok, Tree.Line_Begin_Char_Pos (Line)) = 0
                           then
                              Indent := False;
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;

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

   function Last_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.Aug.Last_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Base.Line_Region.First
            else Token.Aug.Last_Trailing_Comment_Line)
         else
           (if Token.Aug.Last_Indent_Line = Invalid_Line_Number
            then Token.Base.Line_Region.First
            else Token.Aug.Last_Indent_Line));
   end Last_Line;

end Wisi;
