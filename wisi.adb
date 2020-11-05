--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL;
with WisiToken.Semantic_Checks;
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

   function Image_Augmented (Aug : in Syntax_Trees.Augmented_Class_Access_Constant) return String
   is
      pragma Unreferenced (Aug);
   begin
      return "augmented"; --  FIXME: delete or improve
   end Image_Augmented;

   function Image_Action (Action : in Syntax_Trees.Semantic_Action) return String
   is
      pragma Unreferenced (Action);
   begin
      return "action";
   end Image_Action;

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
         Ada.Text_IO.Put_Line (";; indent_line: " & Line_Number_Type'Image (Line) & " => " & Image (Indent));
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

            if Tok.Base.Line = Tok.Aug.First_Indent_Line then
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
        (Delete, Invalid_Buffer_Pos, Invalid_Token_ID, Syntax_Trees.Invalid_Stream_Index,
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
         Ada.Text_IO.Put_Line (";; " & Parse.LR.Image (Item, Tree));
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
                       ((Last_Deleted.Del_ID = Data.Descriptor.String_1_ID and
                           Op.Del_ID = Data.Descriptor.String_1_ID) or
                          (Last_Deleted.Del_ID = Data.Descriptor.String_2_ID and
                             Op.Del_ID = Data.Descriptor.String_2_ID))
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
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (";; Begin_Indent: " & Integer'Image (Data.Begin_Indent));
         for I in Data.Indents.First_Index .. Data.Indents.Last_Index loop
            Ada.Text_IO.Put_Line (";; " & Line_Number_Type'Image (I) & ", " & Image (Data.Indents (I)));
         end loop;
         Ada.Text_IO.Put_Line (";; resolve anchors");
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
                     Ada.Text_IO.Put_Line
                       ("anchor line" & Line'Image & " id" & I'Image & " indent" & Anchor_Indent (I)'Image);
                  end if;
               end loop;
               Data.Indents.Replace_Element (Line, (Int, Invalid_Line_Number, Begin_Indent));

            when Anchor_Int =>
               for I of Indent.Anchor_Int_IDs loop
                  Anchor_Indent (I) := Indent.Anchor_Int_Indent + Begin_Indent;
                  if Trace_Action > Extra then
                     Ada.Text_IO.Put_Line
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
                        Ada.Text_IO.Put_Line
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
               if WisiToken.Trace_Action > Detail then
                  Ada.Text_IO.Put_Line ("   " & Cache.Pos'Image & " end to " & Cache.End_Pos.Item'Image);
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

   procedure Initialize
     (Data              : in out Parse_Data_Type;
      Lexer             : in     WisiToken.Lexer.Handle;
      Descriptor        : access constant WisiToken.Descriptor;
      Post_Parse_Action : in     Post_Parse_Action_Type;
      Begin_Line        : in     Line_Number_Type;
      End_Line          : in     Line_Number_Type;
      Begin_Indent      : in     Integer;
      Params            : in     String)
   is
      pragma Unreferenced (Params);
   begin
      Data.Line_Begin_Char_Pos.Set_First_Last
        (First   => Begin_Line,
         Last    => End_Line);

      for Pos of Data.Line_Begin_Char_Pos loop
         Pos := Invalid_Buffer_Pos;
      end loop;

      --  + 1 for data on line following last line; see Lexer_To_Augmented.
      Data.Line_Paren_State.Set_First_Last
        (First   => Begin_Line,
         Last    => End_Line + 1);

      Data.Lexer             := Lexer;
      Data.Descriptor        := Descriptor;
      Data.Post_Parse_Action := Post_Parse_Action;

      case Post_Parse_Action is
      when Navigate | Face =>
         null;
      when Indent =>
         Data.Indents.Set_First_Last
           (First   => Begin_Line,
            Last    => End_Line);

         Data.Begin_Indent := Begin_Indent;
      end case;

      Data.Reset;
   exception
   when E : others =>
      raise SAL.Programmer_Error with "wisi.initialize: " & Ada.Exceptions.Exception_Name (E) & ": " &
        Ada.Exceptions.Exception_Message (E);
   end Initialize;

   overriding procedure Reset (Data : in out Parse_Data_Type)
   is begin
      --  Data.Line_Begin_Char_Pos  set in Initialize, overwritten in Lexer_To_Augmented

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

   function Source_File_Name (Data : in Parse_Data_Type) return String
   is begin
      return Data.Lexer.File_Name;
   end Source_File_Name;

   function Post_Parse_Action (Data : in Parse_Data_Type) return Post_Parse_Action_Type
   is begin
      return Data.Post_Parse_Action;
   end Post_Parse_Action;

   overriding
   procedure Lexer_To_Augmented
     (Data               : in out Parse_Data_Type;
      Tree               : in out Syntax_Trees.Tree'Class;
      Token              : in     Base_Token;
      Prev_Grammar_Token : in     Syntax_Trees.Node_Access)
   is
      use Syntax_Trees;
      use all type Ada.Containers.Count_Type;
   begin
      if Data.Lexer.First then
         Data.Line_Begin_Char_Pos (Token.Line) := Token.Char_Region.First;
      end if;

      declare
         First_Set_Line : Line_Number_Type;
         Last_Line      : constant Line_Number_Type :=
           (if Token.Line <= Data.Line_Begin_Char_Pos.First_Index
            then Invalid_Line_Number
            elsif Data.Lexer.First
            then Token.Line - 1
            else Token.Line);
      begin
         if Token.Line > Data.Line_Begin_Char_Pos.First_Index and then
           Data.Line_Begin_Char_Pos (Last_Line) = Invalid_Buffer_Pos
         then
            --  Previous token contains multiple lines; ie %code in wisitoken_grammar.wy
            for Line in reverse Data.Line_Begin_Char_Pos.First_Index .. Last_Line loop
               if Data.Line_Begin_Char_Pos (Line) /= Invalid_Buffer_Pos then
                  First_Set_Line := Line;
                  exit;
               end if;
            end loop;
            for Line in First_Set_Line + 1 .. Last_Line loop
               Data.Line_Begin_Char_Pos (Line) := Data.Line_Begin_Char_Pos (First_Set_Line); -- good enough
            end loop;

            if Prev_Grammar_Token /= Invalid_Node_Access then
               --  If Token is a non-grammar token following a multi-line grammar
               --  token, Prev_Grammar.Non_Grammar.length is 1, for Token. If Token
               --  is a grammar token, Prev_Grammar.Non_Grammar.length is 0.
               if (Token.ID < Data.Descriptor.First_Terminal and Tree.Non_Grammar_Const (Prev_Grammar_Token).Length = 1)
                 or
                 (Token.ID >= Data.Descriptor.First_Terminal and Tree.Non_Grammar_Const (Prev_Grammar_Token).Length = 0)
               then
                  declare
                     Prev_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Grammar_Token);
                  begin
                     Prev_Aug.Last_Indent_Line := Token.Line;
                  end;
               end if;
            end if;
         end if;
      end;

      if Token.ID < Data.Descriptor.First_Terminal then
         --  Non-grammar token

         if Prev_Grammar_Token /= Invalid_Node_Access then
            declare
               Containing_Non_Grammar : Base_Token_Array_Const_Ref renames Tree.Non_Grammar_Const (Prev_Grammar_Token);
               Containing_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Grammar_Token);

               Trailing_Blank : constant Boolean :=
                 Token.ID = Data.Descriptor.New_Line_ID and
                 (Containing_Non_Grammar.Length > 0 and then
                    Containing_Non_Grammar
                      (Containing_Non_Grammar.Last_Index).ID = Data.Descriptor.New_Line_ID);
            begin
               if Data.Lexer.First and
                 (Token.ID in Data.First_Comment_ID .. Data.Last_Comment_ID or
                    Trailing_Blank)
               then
                  if Containing_Aug.First_Trailing_Comment_Line = Invalid_Line_Number then
                     Containing_Aug.First_Trailing_Comment_Line := Token.Line;
                  end if;
                  Containing_Aug.Last_Trailing_Comment_Line  := Token.Line;
               end if;
            end;
         end if;

      else
         --  grammar token
         declare
            Temp : constant Augmented_Access := new Augmented'
              (Deleted                     => False,
               Paren_State                 => 0,
               First_Indent_Line           => (if Data.Lexer.First then Token.Line else Invalid_Line_Number),
               Last_Indent_Line            => (if Data.Lexer.First then Token.Line else Invalid_Line_Number),
               First_Trailing_Comment_Line => Invalid_Line_Number, -- Set by Reduce
               Last_Trailing_Comment_Line  => Invalid_Line_Number);
         begin
            --  Data.Line_Paren_State, Non_Grammar.Paren_State are computed in
            --  Initialize_Actions after parse is finished and error recover
            --  insert/delete applied to the parse stream.

            Tree.Set_Augmented (Prev_Grammar_Token, Syntax_Trees.Augmented_Class_Access (Temp));
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
      --  the parse tree. Insert_Token, Delete_Token not yet called;
      --  Non_Grammar on deleted tokens have been moved to previous
      --  terminal. Compute User_Data components that depend on the parse
      --  tree terminal sequence.

      --  Data.Line_Begin_Char_Pos does not change here; it is not affected
      --  by inserted or deleted tokens.

      Data.Current_Paren_State := 0;

      loop
         declare
            Token : constant WisiToken.Base_Token := Tree.Base_Token (I);
            Aug   : Augmented_Var_Ref renames To_Augmented_Var_Ref (Tree.Augmented (I));
         begin
            if Last_Line /= Token.Line then
               Data.Line_Paren_State (Token.Line) := Data.Current_Paren_State;
               Last_Line := Token.Line;
            end if;

            Aug.Paren_State := Data.Current_Paren_State;

            if Token.ID = Data.Left_Paren_ID then
               Data.Current_Paren_State := @ + 1;
            elsif Token.ID = Data.Right_Paren_ID then
               Data.Current_Paren_State := @ - 1;
            end if;
         end;

         I := Tree.Next_Terminal (I);

         exit when I = Invalid_Node_Access;
      end loop;

      if Trace_Action > Detail then
         Ada.Text_IO.Put_Line (";; Line_Paren_State: " & Image (Data.Line_Paren_State));
      end if;
   end Initialize_Actions;

   overriding
   procedure Insert_Token
     (Data           : in out Parse_Data_Type;
      Tree           : in out Syntax_Trees.Tree'Class;
      Inserted_Token : in     Syntax_Trees.Valid_Node_Access)
   is
      use all type Syntax_Trees.Node_Access;

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
         elsif First (Data, Before_Token)
         then Before_Token.Line
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
         Last_Trailing_Comment_Line  => Invalid_Line_Number);

      Prev_Terminal : constant Syntax_Trees.Node_Access := Tree.Prev_Terminal (Inserted_Token);
      --  Invalid_Node_Index if Inserted_Token is inserted before first grammar token

      Insert_After : Boolean := False;
   begin
      Tree.Set_Augmented (Inserted_Token, Syntax_Trees.Augmented_Class_Access (New_Aug));

      if (Prev_Terminal /= Syntax_Trees.Invalid_Node_Access and Inserted_Before /= Syntax_Trees.Invalid_Node_Access)
        and then First (Data, Before_Token)
      then
         declare
            use all type SAL.Base_Peek_Type;
            use all type Ada.Containers.Count_Type;
            use all type Ada.Text_IO.Count;

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
            --  (Before_Token) is True. The whitespace after the New_Line is not
            --  given a token.
            --
            --  If the first two tokens in Prev_Non_Grammar are both New_Lines,
            --  there is a blank line after the code line (and before any
            --  comments); assume that is the edit point.
            Insert_On_Blank_Line : constant Boolean := Prev_Non_Grammar.Length >= 2 and then
              (Prev_Non_Grammar (Prev_Non_Grammar.First_Index).ID = Data.Descriptor.New_Line_ID and
                 Prev_Non_Grammar (Prev_Non_Grammar.First_Index + 1).ID = Data.Descriptor.New_Line_ID);

            --  In Ada, 'end' is Insert_After except when Insert_On_Blank_Line is
            --  True (see test/ada_mode-interactive_2.adb Record_1), so Insert_After
            --  needs Insert_On_Blank_Line.
         begin
            Insert_After := Parse_Data_Type'Class (Data).Insert_After
              (Tree, Inserted_Token, Inserted_Before, Insert_On_Blank_Line);

            if WisiToken.Trace_Action > WisiToken.Detail then
               Ada.Text_IO.Put_Line
                 (";; insert token " & Tree.Image (Inserted_Token, Node_Numbers => True) &
                    (if Insert_After
                     then " after " & Tree.Image (Prev_Terminal, Node_Numbers => True)
                     else " before " & Tree.Image (Inserted_Before, Node_Numbers => True)));
            end if;

            if Insert_After then
               if Insert_On_Blank_Line then
                  declare
                     Prev_Non_Grammar_Tok : constant WisiToken.Base_Token :=
                       Prev_Non_Grammar (Prev_Non_Grammar.First_Index + 1);
                     --  The newline nominally after the inserted token.
                  begin
                     Tree.Update
                       (Inserted_Token,
                        Byte_Region => (First | Last => Prev_Non_Grammar_Tok.Byte_Region.Last - 1),
                        Char_Region => (First | Last => Prev_Non_Grammar_Tok.Char_Region.Last - 1),
                        Line        => Prev_Non_Grammar_Tok.Line,
                        Column      => Prev_Non_Grammar_Tok.Column +
                          Ada.Text_IO.Count (Length (Before_Token.Char_Region)) - 1);

                     New_Aug.First_Indent_Line := Prev_Non_Grammar_Tok.Line;
                     New_Aug.Last_Indent_Line  := Prev_Non_Grammar_Tok.Line;

                     for I in Prev_Non_Grammar.First_Index + 1 .. Prev_Non_Grammar.Last_Index loop
                        Token_Non_Grammar.Append (Prev_Non_Grammar (I));
                     end loop;

                     Prev_Non_Grammar.Set_First_Last (Prev_Non_Grammar.First_Index, Prev_Non_Grammar.First_Index);
                  end;
               elsif Length (Prev_Token.Char_Region) = 0 then
                  raise SAL.Programmer_Error with "Prev_Token char_region = 0: " & Tree.Image (Prev_Terminal);
               else

                  Tree.Update
                    (Inserted_Token,
                     Byte_Region => (First | Last => Prev_Token.Byte_Region.Last),
                     Char_Region => (First | Last => Prev_Token.Char_Region.Last),
                     Line        => Prev_Token.Line,
                     Column      => Prev_Token.Column + Ada.Text_IO.Count (Length (Prev_Token.Char_Region)) - 1);

                  Token_Non_Grammar := Prev_Non_Grammar;

                  New_Aug.First_Indent_Line := Invalid_Line_Number;
                  New_Aug.Last_Indent_Line  := Invalid_Line_Number;

                  Prev_Non_Grammar := WisiToken.Base_Token_Arrays.Empty_Vector;
               end if;

               New_Aug.First_Trailing_Comment_Line := Prev_Aug.First_Trailing_Comment_Line;
               New_Aug.Last_Trailing_Comment_Line  := Prev_Aug.Last_Trailing_Comment_Line;

               Prev_Aug.First_Trailing_Comment_Line := Invalid_Line_Number;
               Prev_Aug.Last_Trailing_Comment_Line  := Invalid_Line_Number;
            end if;
         end;
      end if;

      declare
         Tok : constant WisiToken.Base_Token := Tree.Base_Token (Inserted_Token);
      begin
         if First (Data, Tok) and not Insert_After and
           Inserted_Before /= Syntax_Trees.Invalid_Node_Access
         then
            declare
               Before_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Inserted_Before);
            begin
               Before_Aug.First_Indent_Line := Invalid_Line_Number;
               Before_Aug.Last_Indent_Line  := Invalid_Line_Number;
            end;
         end if;
      end;
   end Insert_Token;

   overriding
   procedure Delete_Token
     (Data          : in out Parse_Data_Type;
      Tree          : in out Syntax_Trees.Tree'Class;
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
   begin
      if Deleted_Aug.Deleted then
         --  This can happen if error recovery screws up.
         if WisiToken.Trace_Action > WisiToken.Detail then
            Ada.Text_IO.Put_Line (";; delete token again; ignored " & Tree.Image (Deleted_Token, Node_Numbers => True));
         end if;
         return;
      end if;
      if WisiToken.Trace_Action > WisiToken.Detail then
         Ada.Text_IO.Put_Line (";; delete token " & Tree.Image (Deleted_Token, Node_Numbers => True));
      end if;

      Deleted_Aug.Deleted := True;

      if Deleted_Non_Grammar.Length > 0 then
         --  Move Non_Grammar to previous non-deleted token

         if Prev_Token /= Syntax_Trees.Invalid_Node_Access then
            declare
               use all type Syntax_Trees.Augmented_Class_Access;
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

      if First (Data, Deleted_Tok) and
        (Next_Token = Syntax_Trees.Invalid_Node_Access or else
           Tree.Base_Token (Next_Token).Line > Deleted_Tok.Line)
      then
         --  Deleted_Tok.Line is now blank; add to previous token non
         --  grammar.
         if Prev_Token /= Syntax_Trees.Invalid_Node_Access then
            declare
               Prev_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Prev_Token);
            begin
               if Prev_Aug.First_Trailing_Comment_Line = Invalid_Line_Number then
                  Prev_Aug.First_Trailing_Comment_Line := Deleted_Tok.Line;
                  Prev_Aug.Last_Trailing_Comment_Line  := Deleted_Tok.Line;
               else
                  if Prev_Aug.First_Trailing_Comment_Line > Deleted_Tok.Line then
                     Prev_Aug.First_Trailing_Comment_Line := Deleted_Tok.Line;
                  end if;
                  if Prev_Aug.Last_Trailing_Comment_Line < Deleted_Tok.Line then
                     Prev_Aug.Last_Trailing_Comment_Line := Deleted_Tok.Line;
                  end if;
               end if;
            end;
         end if;
      end if;

      if First (Data, Deleted_Tok) and Next_Token /= Syntax_Trees.Invalid_Node_Access then
         if not First (Data, Tree.Base_Token (Next_Token)) then
            declare
               Next_Aug : Augmented_Var_Ref renames Get_Augmented_Var (Tree, Next_Token);
            begin
               Next_Aug.First_Indent_Line := Deleted_Aug.First_Indent_Line;
               Next_Aug.Last_Indent_Line  := Deleted_Aug.Last_Indent_Line;
            end;
         end if;
      end if;
   end Delete_Token;

   overriding
   procedure Reduce
     (Data    : in out Parse_Data_Type;
      Tree    : in out Syntax_Trees.Tree'Class;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Node_Access_Array)
   is
      Nonterm_Aug : constant Augmented_Access := new Augmented'(others => <>);

      Trailing_Comment_Done : Boolean := False;
   begin
      Tree.Set_Augmented (Nonterm, Syntax_Trees.Augmented_Class_Access (Nonterm_Aug));

      for I in reverse Tokens'Range loop
         --  'reverse' to find token containing trailing comments; last
         --  non-empty token.
         declare
            Token : constant Augmented_Token := Get_Augmented_Token (Tree, Tokens (I));
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
      end loop;
   end Reduce;

   procedure Statement_Action
     (Data    : in out Parse_Data_Type;
      Tree    : in     Syntax_Trees.Tree;
      Nonterm : in     Syntax_Trees.Valid_Node_Access;
      Tokens  : in     Syntax_Trees.Valid_Node_Access_Array;
      Params  : in     Statement_Param_Array)
   is
      Nonterm_Tok : constant Base_Token := Tree.Base_Token (Nonterm);

      First_Item         : Boolean        := True;
      Start_Set          : Boolean        := False;
      Override_Start_Set : Boolean        := False;
      Containing_Pos     : Nil_Buffer_Pos := Nil;
   begin
      if WisiToken.Trace_Action > Outline then
         Ada.Text_IO.Put_Line ("Statement_Action " & Tree.Image (Nonterm, Children => True));
      end if;

      for Pair of Params loop
         if not (Pair.Index in Tokens'Range) then
            raise Fatal_Error with Error_Message
              (File_Name => Data.Lexer.File_Name,
               Line      => Nonterm_Tok.Line,
               Column    => Nonterm_Tok.Column,
               Message   => "wisi-statement-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) &
                 " token index" & SAL.Peek_Type'Image (Pair.Index) &
                 " not in tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
                 SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");

         elsif Tree.Byte_Region (Tokens (Pair.Index)) /= Null_Buffer_Region then
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
                     if WisiToken.Trace_Action > Detail then
                        Ada.Text_IO.Put_Line
                          ("   " & Cache.Pos'Image & " nonterm to " & Image (Cache.Statement_ID, Data.Descriptor.all) &
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

                  if WisiToken.Trace_Action > Detail then
                     declare
                        Cache : Navigate_Cache_Type renames Data.Navigate_Caches.Constant_Ref (Cursor);
                     begin
                        Ada.Text_IO.Put_Line
                          ("   " & Cache.Pos'Image & " create " & Image (Cache.ID, Data.Descriptor.all) &
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

                        Cursor : Navigate_Cache_Trees.Cursor := Find_In_Range
                          (Iterator, Ascending, Nonterm_Tok.Char_Region.First + 1, -- don't set containing on start
                           Nonterm_Tok.Char_Region.Last);
                     begin
                        loop
                           exit when not Has_Element (Cursor);
                           declare
                              Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Cursor);
                           begin
                              if not Cache.Containing_Pos.Set then
                                 Cache.Containing_Pos := Containing_Pos;
                                 if WisiToken.Trace_Action > Detail then
                                    Ada.Text_IO.Put_Line
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
         declare
            Token : constant Base_Token := Tree.Base_Token (Tokens (Tokens'First));
         begin
            raise Grammar_Error with Error_Message
              (File_Name => Data.Lexer.File_Name,
               Line      => Token.Line,
               Column    => Token.Column,
               Message   => "wisi-name-action: " & Trimmed_Image (Tree.Production_ID (Nonterm)) & " name (" &
                 Trimmed_Image (Name) & ") not in Tokens range (" & SAL.Peek_Type'Image (Tokens'First) & " .." &
                 SAL.Peek_Type'Image (Tokens'Last) & "); bad grammar action.");
         end;
      end if;

      if Tree.Is_Virtual (Tokens (Name)) then
         --  Virtual tokens don't appear in the actual buffer, so we can't set
         --  a text property on them.
         return;
      end if;

      pragma Assert (Tree.Label (Tokens (Name)) in Shared_Terminal | Syntax_Trees.Nonterm);

      declare
         use Name_Cache_Trees;
         Name_Token : constant Base_Token := Tree.Base_Token (Tokens (Name));
         Cursor     : constant Name_Cache_Trees.Cursor := Find
           (Data.Name_Caches.Iterate, Name_Token.Char_Region.First,
            Direction => Name_Cache_Trees.Unknown);
      begin
         if Has_Element (Cursor) then
            raise Fatal_Error with Error_Message
              (File_Name       => Data.Lexer.File_Name,
               Line            => Name_Token.Line,
               Column          => Name_Token.Column,
               Message         => Tree.Image
                 (Node         => Tokens (Name),
                  Node_Numbers => WisiToken.Trace_Action > Extra,
                  RHS_Index    => WisiToken.Trace_Action > Extra)
                 & ": wisi-name-action: name set twice.");
         else
            if Trace_Action > Detail then
               Ada.Text_IO.Put_Line
                 ("Name_Action " & Tree.Image
                    (Nonterm,
                     Node_Numbers    => WisiToken.Trace_Action > Extra,
                     RHS_Index       => WisiToken.Trace_Action > Extra) & " " & Tree.Image
                       (Tokens (Name),
                        Node_Numbers => WisiToken.Trace_Action > Extra,
                        RHS_Index    => WisiToken.Trace_Action > Extra));
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

      Iter           : constant Iterator := Data.Navigate_Caches.Iterate;
      Prev_Cache_Cur : Cursor;
   begin
      if WisiToken.Trace_Action > Outline then
         Ada.Text_IO.Put_Line
           ("Motion_Action " & Image (Tree.ID (Nonterm), Data.Descriptor.all) & " " &
              Image (Tree.Byte_Region (Nonterm)));
      end if;
      for Param of Params loop
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
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
                  when Shared_Terminal =>
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
                        raise Fatal_Error with Error_Message
                          (File_Name => Data.Lexer.File_Name,
                           Line      => Token.Line,
                           Column    => Token.Column,
                           Message   => "wisi-motion-action: token " &
                             WisiToken.Image (Token.ID, Data.Descriptor.all) &
                             " has no cache; add to statement-action for " &
                             Trimmed_Image (Tree.Production_ID (Nonterm)) & ".");
                     end if;

                     if Has_Element (Prev_Cache_Cur) then
                        declare
                           Cache      : Navigate_Cache_Type renames Data.Navigate_Caches (Cache_Cur);
                           Prev_Cache : Navigate_Cache_Type renames Data.Navigate_Caches (Prev_Cache_Cur);
                        begin
                           if Cache.Prev_Pos.Set then
                              if WisiToken.Trace_Action > Detail then
                                 Ada.Text_IO.Put_Line
                                   ("   " & Cache.Pos'Image & " prev already at " & Cache.Prev_Pos.Item'Image);
                              end if;
                           else
                              Cache.Prev_Pos := (True, Prev_Cache.Pos);
                              if WisiToken.Trace_Action > Detail then
                                 Ada.Text_IO.Put_Line
                                   ("   " & Cache.Pos'Image & " prev to " & Cache.Prev_Pos.Item'Image);
                              end if;
                           end if;

                           if Prev_Cache.Next_Pos.Set then
                              if WisiToken.Trace_Action > Detail then
                                 Ada.Text_IO.Put_Line
                                   ("   " & Prev_Cache.Pos'Image & " next already at " &
                                      Prev_Cache.Next_Pos.Item'Image);
                              end if;
                           else
                              Prev_Cache.Next_Pos := (True, Cache.Pos);
                              if WisiToken.Trace_Action > Detail then
                                 Ada.Text_IO.Put_Line
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
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            if Trace_Action > Outline then
               Ada.Text_IO.Put_Line
                 (";; face_apply_action: " & Image (Tree.Byte_Region (Tokens (Param.Index))) &
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
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
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
         if Tree.Byte_Region (Tokens (Param.Index)) /= Null_Buffer_Region then
            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (Param.Index));
            begin
               Cache_Cur := Find (Iter, Token.Char_Region.First, Direction => Ascending);
               if Has_Element (Cache_Cur) then
                  declare
                     Cache : Face_Cache_Type renames Data.Face_Caches (Cache_Cur);
                     Other_Cur : Cursor := Find_In_Range
                       (Iter, Ascending, Cache.Char_Region.Last + 1, Token.Char_Region.Last);
                     Temp : Cursor;
                  begin
                     loop
                        exit when not Has_Element (Other_Cur) or else
                          Data.Face_Caches (Other_Cur).Char_Region.First > Token.Char_Region.Last;
                        Temp := Other_Cur;
                        Other_Cur := Next (Iter, Other_Cur);
                        Delete (Data.Face_Caches, Temp);
                     end loop;

                     Cache.Class            := Param.Class;
                     Cache.Char_Region.Last := Token.Char_Region.Last;
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
      Temp      : Cursor;
   begin
      for I of Params loop
         if Tree.Byte_Region (Tokens (I)) /= Null_Buffer_Region then
            declare
               Token : constant Base_Token := Tree.Base_Token (Tokens (I));
            begin
               Cache_Cur := Find_In_Range (Iter, Ascending, Token.Char_Region.First, Token.Char_Region.Last);
               loop
                  exit when not Has_Element (Cache_Cur) or else
                    Data.Face_Caches (Cache_Cur).Char_Region.First > Token.Char_Region.Last;
                  Temp := Cache_Cur;
                  Cache_Cur := Next (Iter, Cache_Cur);
                  Delete (Data.Face_Caches, Temp);
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
            Image (Item.Hanging_Delta_1) & ", "  & Image (Item.Hanging_Delta_2) & ")");
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
         Ada.Text_IO.Put_Line (";; indent_action_0: " & Tree.Image (Nonterm, RHS_Index => True));
      end if;

      for I in Tokens'Range loop
         if (Tree.Is_Virtual_Terminal (Tokens (I)) or
               Tree.Byte_Region (Tokens (I)) /= Null_Buffer_Region) and
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
                  Ada.Text_IO.Put_Line
                    (";; indent_action_0 code: " & Tree.Image (Tree_Token) & ": " & Image (Pair.Code_Delta));
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
                        Ada.Text_IO.Put_Line
                          (";; indent_action_0 comment: " & Tree.Image (Controlling_Token) & ": " &
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
         if Indenting_Token.Base.Line = Indenting_Token.Aug.First_Indent_Line
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
            Hanging_First_Line  => Indenting_Token.Base.Line,
            Hanging_Paren_State => Indenting_Token.Aug.Paren_State,
            Hanging_Delta_1     => Indent_Compute_Delta
              (Data, Tree, Nonterm, Tokens,
               (Simple,
                (if Indenting_Token.Base.Line = Indenting_Token.Aug.First_Indent_Line
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
                     (if Indenting_Token.Base.Line = Indenting_Token.Aug.First_Indent_Line
                      then Delta_2 else Delta_1),
                   when Hanging_2 =>
                      Compute_Hanging_2)),
                 Tree_Indenting, Indenting_Comment).Simple_Delta)
         do
            --  Controlling_Token_Line for Delta_2 is the first non-comment
            --  line indented by Delta_2.
            if Label = Hanging_1 and
              Indenting_Token.Base.Line /= Indenting_Token.Aug.First_Indent_Line
            then
               --  Only using Delta_1
               null;
            else
               for Line in Indenting_Token.Aug.First_Indent_Line +
                 (if Indenting_Token.Base.Line = Indenting_Token.Aug.First_Indent_Line then 1 else 0)
                 .. Indenting_Token.Aug.Last_Indent_Line
               loop
                  if Data.Line_Begin_Token.all (Line).Node /= Syntax_Trees.Invalid_Node_Access then
                     Result.Hanging_Delta_2.Controlling_Token_Line := Line;
                     exit;
                  end if;
               end loop;
            end if;
         end return;
      end if;
   end Indent_Hanging_1;

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

      Last_Term : constant Syntax_Trees.Node_Access := Parser.Tree.Last_Terminal (Parser.Tree.Root);

      function Get_Last_Char_Pos return Buffer_Pos
      is begin

         if Last_Term = Syntax_Trees.Invalid_Node_Access then
            --  All comments, or empty
            if Parser.Tree.Leading_Non_Grammar_Const.Length > 0 then
               return Parser.Tree.Leading_Non_Grammar_Const.Element.all
                 (Parser.Tree.Leading_Non_Grammar_Const.Last_Index).Char_Region.Last;
            else
               return Buffer_Pos'First;
            end if;
         else
            declare
               Non_Grammar : Base_Token_Array_Const_Ref renames Parser.Tree.Non_Grammar_Const (Last_Term);
            begin
               if Non_Grammar.Length = 0 then
                  return Parser.Tree.Base_Token (Last_Term).Char_Region.Last;
               else
                  return Non_Grammar (Non_Grammar.Last_Index).Char_Region.Last;
               end if;
            end;
         end if;
      end Get_Last_Char_Pos;

      Last_Char_Pos : constant Buffer_Pos := Get_Last_Char_Pos;

      function Get_Last_Line return Line_Number_Type
      is begin
         for I in Data.Line_Begin_Char_Pos.First_Index .. Data.Line_Begin_Char_Pos.Last_Index loop
            if Data.Line_Begin_Char_Pos (I) = Invalid_Buffer_Pos then
               raise SAL.Programmer_Error with "line_begin_pos" & Line_Number_Type'Image (I) & " invalid";
            end if;
            if Data.Line_Begin_Char_Pos (I) > Last_Char_Pos then
               if I > Line_Number_Type'First then
                  return I - 1;
               else
                  return I;
               end if;
            end if;
         end loop;
         return Data.Line_Begin_Char_Pos.Last_Index;
      end Get_Last_Line;

   begin
      if Trace_Action > Outline then
         Ada.Text_IO.Put_Line
           (";; last_char_pos:" & Buffer_Pos'Image (Last_Char_Pos + 1) &
              " last_line:" & Line_Number_Type'Image (Get_Last_Line));
      end if;

      --  +1 to match Emacs region
      Ada.Text_IO.Put_Line ('[' & End_Code & Buffer_Pos'Image (Last_Char_Pos + 1) & ']');

      case Data.Post_Parse_Action is
      when Navigate =>
         for Cache of Data.Navigate_Caches loop
            Put (Cache);
         end loop;
         for Cache of Data.Name_Caches loop
            Put (Cache);
         end loop;

      when Face =>
         for Cache of Data.Face_Caches loop
            Put (Cache);
         end loop;

      when Indent =>
         Resolve_Anchors (Data);

         if Trace_Action > Outline then
            Ada.Text_IO.Put_Line (";; indent leading non_grammar");
         end if;
         for Token of Parser.Tree.Leading_Non_Grammar_Const loop
            if First (Data, Token) then
               Put (Token.Line, (Int, Invalid_Line_Number, Data.Begin_Indent));
            end if;
         end loop;

         --  It may be that not all lines in Data.Indents were parsed.
         if Trace_Action > Outline then
            Ada.Text_IO.Put_Line (";; indent grammar");
         end if;
         for I in Data.Indents.First_Index .. Get_Last_Line loop
            Put (I, Data.Indents (I));
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
      use Semantic_Checks;

      function Safe_Pos (Node : in Syntax_Trees.Valid_Node_Access) return Buffer_Pos
      is
         --  Return a reasonable position for the error at Node.
         --
         --  In a successful parse with error recovery, Node is a
         --  shared_terminal, so it's Char_Region is the first choice.
         --
         --  If this is an error due to a bad recovery, Node may be a virtual
         --  token, with no position information, so we try to get information
         --  from its parent.
         use Syntax_Trees;

         N : Node_Access := Node;
      begin
         loop
            if Tree.Label (N) /= Virtual_Terminal then
               declare
                  Token : constant WisiToken.Base_Token := Tree.Base_Token (N);
               begin
                  if Token.Char_Region /= Null_Buffer_Region then
                     return Token.Char_Region.First;
                  end if;

               end;
            end if;
            if Tree.Parents_Set then
               N := Tree.Parent (N);
               exit when N = Invalid_Node_Access;
            else
               exit;
            end if;
         end loop;
         return Buffer_Pos'First;
      end Safe_Pos;

      function Safe_Pos (Token : in Syntax_Trees.Recover_Token) return Buffer_Pos
      is begin
         if Token.Name /= Null_Buffer_Region then
            return Token.Name.First;

         elsif Token.Byte_Region = Null_Buffer_Region then
            return Buffer_Pos'First;

         else
            return Token.Byte_Region.First;
         end if;
      end Safe_Pos;

   begin
      Put (Lexer_Errors);

      for Item of Parse_Errors loop
         case Item.Label is
         when Parse.LR.Action =>
            Put_Line
              ('[' & Parser_Error_Code & Buffer_Pos'Image (Safe_Pos (Item.Error_Token)) &
                 " ""syntax error: expecting " & Image (Item.Expecting, Data.Descriptor.all) &
                 ", found '" & Image (Tree.ID (Item.Error_Token), Data.Descriptor.all) & "'""]");

         when Parse.LR.Check =>
            Put_Line
              ('[' & Check_Error_Code & Integer'Image
                 (Semantic_Checks.Check_Status_Label'Pos (Item.Check_Status.Label)) &
                 (case Item.Check_Status.Label is
                  when Ok => "",
                  when Error =>
                     Buffer_Pos'Image (Safe_Pos (Item.Check_Status.Begin_Name)) &
                       Buffer_Pos'Image (Safe_Pos (Item.Check_Status.End_Name)) &
                       " ""block name error""]"));

         when Parse.LR.Message =>
            Put_Line
              ('[' & Parser_Error_Code & Buffer_Pos'Image (Buffer_Pos'First) &
                 " """ & (-Item.Msg) & """]");
         end case;
      end loop;

      Put (Recover, Data, Tree);
   end Put;

   procedure Put_Error (Data : in Parse_Data_Type; Line_Number : in Line_Number_Type; Message : in String)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("(error """ & Error_Message (Data.Lexer.File_Name, Line_Number, 0, Message) & """)");
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
         when Hanging => Line_Number_Type'Image (Item.Hanging_First_Line) & Integer'Image (Item.Hanging_Paren_State) &
              " " & Image (Item.Hanging_Delta_1) & " " & Image (Item.Hanging_Delta_2)) & ")";
   end Image;

   function Current_Indent_Offset
     (Data         : in Parse_Data_Type'Class;
      Anchor_Token : in Base_Token;
      Offset       : in Integer)
     return Integer
   is begin
      return Offset + Integer (Anchor_Token.Char_Region.First - Data.Line_Begin_Char_Pos (Anchor_Token.Line));
   end Current_Indent_Offset;

   function First_Line
     (Token             : in Augmented_Token;
      Indenting_Comment : in Boolean)
     return Line_Number_Type
   is begin
      return
        (if Indenting_Comment then
           (if Token.Aug.First_Trailing_Comment_Line = Invalid_Line_Number
            then Token.Base.Line
            else Token.Aug.First_Trailing_Comment_Line)
         else
           (if Token.Aug.First_Indent_Line = Invalid_Line_Number
            then Token.Base.Line
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
      when Shared_Terminal | Nonterm =>
         return Data.Lexer.Buffer_Text (Tree.Byte_Region (Tree_Index));

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
     (Data        : in out Parse_Data_Type;
      Anchor_Line : in     Line_Number_Type;
      Last_Line   : in     Line_Number_Type;
      Offset      : in     Integer)
     return Delta_Type
   --  Create an anchor, return an anchored delta using it.
   is
      use Anchor_ID_Vectors;
      --  We can't use a Reference here, because the Element in reference
      --  types is constrained (as are all allocated objects of access
      --  types; AARM 4.8 (6/3)), and we may need to change the Label.
      Indent    : Indent_Type      := Data.Indents (Anchor_Line);
      Anchor_ID : constant Integer := 1 + Max_Anchor_ID (Data, Anchor_Line, Last_Line);
   begin
      Data.Max_Anchor_ID := Integer'Max (Data.Max_Anchor_ID, Anchor_ID);

      case Indent.Label is
      when Not_Set =>
         Indent := (Anchor_Nil, Invalid_Line_Number, To_Vector (Anchor_ID, 1));

         if Trace_Action > Extra then
            Ada.Text_IO.Put_Line
              (";; indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
         end if;

      when Int =>
         Indent := (Anchor_Int, Invalid_Line_Number, To_Vector (Anchor_ID, 1), Indent.Int_Indent);

         if Trace_Action > Extra then
            Ada.Text_IO.Put_Line
              (";; indent_anchored: " & Line_Number_Type'Image (Anchor_Line) & " => " & Image (Indent));
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
            return (Simple, (Int, Indenting_Token.Base.Line, Param.Param.Int_Delta));

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
                       (if Anchor_Token.Aug.Last_Indent_Line = Invalid_Line_Number
                        then Anchor_Token.Base.Line
                        else Anchor_Token.Aug.Last_Indent_Line)
                     else Anchor_Token.Base.Line),
                  Last_Line   => Last_Line (Indenting_Token, Indenting_Comment),
                  Offset      =>
                    (case Simple_Param_Anchored'(Param.Param.Label) is
                     when Anchored_0 =>
                       (if Indenting_Comment
                        --  FIXME: need test case for comment
                        then Param.Param.Anchored_Delta
                        else Current_Indent_Offset (Data, Anchor_Token.Base, Param.Param.Anchored_Delta)),
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

      First_Line : constant Line_Number_Type := Wisi.First_Line (Indenting_Token, Indenting_Comment);
      Last_Line  : constant Line_Number_Type := Wisi.Last_Line (Indenting_Token, Indenting_Comment);
   begin
      if Trace_Action > Detail then
         Ada.Text_IO.Put_Line
           (";; indent_token_1:      " &
              Image (Indenting_Token.Base, Data.Descriptor.all) & " " & Image (Delta_Indent) &
              Line_Number_Type'Image (First_Line) & " .." & Line_Number_Type'Image (Last_Line) &
              (if Indenting_Comment then " comment" else ""));
      end if;

      for Line in First_Line .. Last_Line loop
         if Data.Indent_Comment_Col_0 then
            declare
               use all type Ada.Text_IO.Count;

               function Containing_Token return Syntax_Trees.Node_Access
               --  Return terminal containing leading comment on Line;
               --  Invalid_Node_Access if none.
               is
                  I : Line_Number_Type := Line;
               begin
                  if Line < Data.Line_Begin_Token.First_Index then
                     --  Line is before first grammar token; Leading_Non_Grammar checked
                     --  below.
                     return Syntax_Trees.Invalid_Node_Access;
                  end if;

                  loop
                     --  The grammar token containing the comment on Line I is the last
                     --  grammar token on the previous line. So find the first grammar
                     --  token on line I or a subsequent line, then find the previous
                     --  terminal. Note that we are supporting C style comments here, where
                     --  code can follow a comment on a line.
                     exit when Data.Line_Begin_Token.all (I).Node /= Syntax_Trees.Invalid_Node_Access;
                     I := I + 1;

                     if I > Data.Line_Begin_Token.Last_Index then
                        --  We should always find Wisi_EOI
                        raise SAL.Programmer_Error;
                     end if;
                  end loop;

                  declare
                     J   : constant Syntax_Trees.Node_Access := Tree.Prev_Terminal (Data.Line_Begin_Token.all (I).Node);
                  begin
                     if J = Syntax_Trees.Invalid_Node_Access then
                        return J;
                     end if;

                     declare
                        Aug : Augmented_Const_Ref renames Get_Augmented_Const (Tree, J);
                     begin
                        if Line in Aug.First_Trailing_Comment_Line .. Aug.Last_Trailing_Comment_Line then
                           return J;
                        else
                           return Syntax_Trees.Invalid_Node_Access;
                        end if;
                     end;
                  end;
               end Containing_Token;

               Indent : Boolean := True;

               Containing : constant Syntax_Trees.Node_Access := Containing_Token;
            begin
               if Line < Data.Line_Begin_Token.First_Index then
                  --  Line is before the first grammar token. We may be doing a partial
                  --  parse where the initial indent is non-zero, so we still have to
                  --  check for column 0.
                  for Tok of Tree.Leading_Non_Grammar_Const loop
                     if Tok.Line = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       Tok.Column = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;

               elsif Containing /= Syntax_Trees.Invalid_Node_Access then
                  for Tok of Tree.Non_Grammar_Const (Containing) loop
                     if Tok.Line = Line and then
                       Tok.ID in Data.First_Comment_ID .. Data.Last_Comment_ID and then
                       Tok.Column = 0
                     then
                        Indent := False;
                        exit;
                     end if;
                  end loop;
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
            then Token.Base.Line
            else Token.Aug.Last_Trailing_Comment_Line)
         else
           (if Token.Aug.Last_Indent_Line = Invalid_Line_Number
            then Token.Base.Line
            else Token.Aug.Last_Indent_Line));
   end Last_Line;

end Wisi;
