--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020, 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Directories;
with Ada.Finalization;
with Ada.Text_IO;
with GNAT.OS_Lib;
with SAL.Gen_Unbounded_Definite_Red_Black_Trees;
package body Wisi.Parse_Context is

   function Source_File_Name (Item : in Parse_Context_Access) return String
   is (Ada.Strings.Unbounded.To_String (Item.File_Name));

   package File_Parse_Context_Maps is new SAL.Gen_Unbounded_Definite_Red_Black_Trees
     (Element_Type => Parse_Context_Access,
      Key_Type     => String,
      Key          => Source_File_Name,
      Key_Compare  => SAL.String_Compare);

   Map : File_Parse_Context_Maps.Tree;

   function Create_No_File
     (Language : in Wisi.Parse_Context.Language;
      Trace    : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is
      use WisiToken;
   begin
      return Result : constant Parse_Context_Access :=
        (new Parse_Context'
           (File_Name                         => +"",
            Text_Buffer                       => null,
            Text_Buffer_Byte_Last             => 0,
            Text_Buffer_Char_Last             => 0,
            Parser                            => WisiToken.Parse.LR.Parser.Parser'
              (Ada.Finalization.Limited_Controlled with
               Trace                          => Trace,
               User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
               Table                          => Language.Table,
               Language_Fixes                 => Language.Fixes,
               Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
               Language_String_ID_Set         => Language.String_ID_Set,
               Partial_Parse_Active           => Language.Partial_Parse_Active,
               Partial_Parse_Byte_Goal        => Language.Partial_Parse_Byte_Goal,
               others                         => <>),
            Root_Save_Edited_Name             => <>,
            Save_Edited_Count                 => <>))
      do
         Result.Parser.Tree.Lexer := Language.Lexer;
      end return;
   end Create_No_File;

   procedure Set_File (File_Name : in String; Parse_Context : in Parse_Context_Access)
   is
      use File_Parse_Context_Maps;
      use WisiToken;
      use Ada.Strings.Unbounded;
   begin
      if Length (Parse_Context.File_Name) > 0 then
         raise Protocol_Error;
      end if;

      Parse_Context.File_Name := +File_Name;
      Map.Insert (Parse_Context);
   end Set_File;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Trace_Incremental_Parse > Outline then
                  Trace.Put_Line ("parse_context found");
               end if;
            end return;
         end if;

         return Result : constant Parse_Context_Access :=
           (new Parse_Context'
              (File_Name                         => +File_Name,
               Text_Buffer                       => null,
               Text_Buffer_Byte_Last             => 0,
               Text_Buffer_Char_Last             => 0,
               Parser                            => WisiToken.Parse.LR.Parser.Parser'
                 (Ada.Finalization.Limited_Controlled with
                  Trace                          => Trace,
                  User_Data                      => Wisi.New_User_Data (Language.Parse_Data_Template.all),
                  Table                          => Language.Table,
                  Language_Fixes                 => Language.Fixes,
                  Language_Matching_Begin_Tokens => Language.Matching_Begin_Tokens,
                  Language_String_ID_Set         => Language.String_ID_Set,
                  Partial_Parse_Active           => Language.Partial_Parse_Active,
                  Partial_Parse_Byte_Goal        => Language.Partial_Parse_Byte_Goal,
                  others                         => <>),
               Root_Save_Edited_Name             => <>,
               Save_Edited_Count                 => <>))
         do
            Result.Parser.Tree.Lexer := Language.Lexer;
            Map.Insert (Result);
            if Trace_Incremental_Parse > Outline then
               Trace.Put_Line ("parse_context created");
            end if;
         end return;
      end;
   end Find_Create;

   function Find
     (File_Name : in String;
      Language  : in Wisi.Parse_Context.Language)
     return Parse_Context_Access
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;
         use WisiToken;
         use all type WisiToken.Descriptor_Access_Constant;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if Has_Element (Found) then
            return Result : constant Parse_Context_Access := Element (Found) do
               if Language.Descriptor /= Result.Parser.Tree.Lexer.Descriptor then
                  raise WisiToken.User_Error with "language does not match for buffer '" & File_Name & "'";
               end if;
               if Trace_Incremental_Parse > Outline then
                  Result.Parser.Trace.Put_Line ("parse_context found");
               end if;
            end return;
         else
            if Trace_Incremental_Parse > Outline then
               Ada.Text_IO.Put_Line ("parse_context not found");
            end if;
            raise Not_Found;
         end if;
      end;
   end Find;

   procedure Kill (File_Name : in String)
   is begin
      if File_Name'Length = 0 then
         raise Wisi.Protocol_Error with "no file name given";
      end if;

      declare
         use File_Parse_Context_Maps;

         Found : constant Cursor := Map.Find (File_Name);
      begin
         if not Has_Element (Found) then
            --  already killed, or never opened
            null;
         else
            Map.Delete (File_Name);
         end if;
      end;
   end Kill;

   procedure Clear
   is begin
      Map.Clear;
   end Clear;

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

   procedure Save_Text
     (Context       : in Parse_Context;
      File_Name     : in String;
      Emacs_Message : in Boolean)
   is
      use GNAT.OS_Lib;
      File : File_Descriptor;
      Written : Integer;
      pragma Unreferenced (Written);
   begin
      if Ada.Directories.Exists (File_Name) then
         Ada.Directories.Delete_File (File_Name);
      end if;
      File := Create_New_File (File_Name, Fmode => Binary);
      Written := Write (File, Context.Text_Buffer (Context.Text_Buffer'First)'Address,
             N => Context.Text_Buffer_Byte_Last - Context.Text_Buffer'First + 1);
      --  Written /= N on disk full; we don't check for that, because there's
      --  nothing to do.
      Close (File);

      if Emacs_Message then
         Ada.Text_IO.Put_Line ("(message ""text saved to '" & File_Name & "'"")");
      else
         Ada.Text_IO.Put_Line ("text saved to '" & File_Name & "'");
      end if;
   end Save_Text;

   procedure Save_Text_Auto
     (Context       : in out Parse_Context;
      Emacs_Message : in     Boolean)
   is begin
      Context.Save_Edited_Count := @ + 1;

      declare
         Save_File_Name : constant String :=
           Ada.Strings.Unbounded.To_String (Context.Root_Save_Edited_Name) & "_" &
           Wisi.Integer_Filled_Image (Item => Context.Save_Edited_Count, Width => 3);
      begin
         Save_Text (Context, Save_File_Name, Emacs_Message);
      end;
   end Save_Text_Auto;

end Wisi.Parse_Context;
