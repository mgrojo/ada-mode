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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Wisi;
with WisiToken.Parse.AUnit;
with WisiToken.Text_IO_Trace;
package body Test_Edit_Source is

   Trace : WisiToken.Text_IO_Trace.Trace;

   procedure Check_Valid_KMN
     (KMN_List        : in WisiToken.Parse.KMN_Lists.List;
      Initial_Source  : in String;
      Expected_Source : in String)
   is
      use WisiToken;
      Source_First          : constant Base_Buffer_Pos := Base_Buffer_Pos (Initial_Source'First);
      Source_Last           : constant Base_Buffer_Pos := Base_Buffer_Pos (Initial_Source'Last);
      Expected_Source_First : constant Base_Buffer_Pos := Base_Buffer_Pos (Expected_Source'First);
      Expected_Source_Last  : constant Base_Buffer_Pos := Base_Buffer_Pos (Expected_Source'Last);
   begin
      WisiToken.Parse.Validate_KMN
        (KMN_List,
         Stable_Byte_First        => Source_First,
         Stable_Char_First        => Source_First,
         Initial_Text_Byte_Region => (Source_First, Source_Last),
         Initial_Text_Char_Region => (Source_First, Source_Last),
         Edited_Text_Byte_Region  => (Expected_Source_First, Expected_Source_Last),
         Edited_Text_Char_Region  => (Expected_Source_First, Expected_Source_Last));
   exception
   when E : WisiToken.User_Error =>
      AUnit.Assertions.Assert (False, Ada.Exceptions.Exception_Message (E));
   end Check_Valid_KMN;

   procedure Test
     (Label                     : in String;
      Initial_Source            : in String;
      Initial_Source_Char_Last  : in Integer;
      Changes                   : in Wisi.Change_Lists.List;
      Expected_Source           : in String;
      Expected_Source_Char_Last : in Integer;
      Expected_KMN              : in WisiToken.Parse.KMN_Lists.List)
   is
      use AUnit.Checks;
      use WisiToken.Parse.AUnit.KMN_Lists_AUnit;


      Computed_Source  : Ada.Strings.Unbounded.String_Access := new String'(Initial_Source);

      Computed_Source_Byte_Last : Integer := Initial_Source'Last;
      Computed_Source_Char_Last : Integer := Initial_Source_Char_Last;
      Computed_KMN              : WisiToken.Parse.KMN_Lists.List;
   begin
      Wisi.Edit_Source
        (Trace, Computed_Source, Computed_Source_Byte_Last, Computed_Source_Char_Last, Changes, Computed_KMN);

      if WisiToken.Trace_Tests > WisiToken.Detail then
         Ada.Text_IO.Put_Line (Label & ".edited source:");
         Ada.Text_IO.Put_Line (Computed_Source (1 .. Computed_Source_Byte_Last));

         Ada.Text_IO.Put_Line (Label & ".computed KMN:");
         for KMN of Computed_KMN loop
            Ada.Text_IO.Put_Line (WisiToken.Parse.Image (KMN));
         end loop;
      end if;

      Check (Label & ".source", Computed_Source (1 .. Computed_Source_Byte_Last), Expected_Source);

      Check (Label & ".char_last", Computed_Source_Char_Last, Expected_Source_Char_Last);

      Check_Valid_KMN (Computed_KMN, Initial_Source, Expected_Source);
      Check (Label & ".kmn", Computed_KMN, Expected_KMN);

      Ada.Strings.Unbounded.Free (Computed_Source);
   exception
   when AUnit.Assertions.Assertion_Error =>
      Ada.Strings.Unbounded.Free (Computed_Source);
      raise;

   when E : others =>
      Ada.Strings.Unbounded.Free (Computed_Source);
      raise AUnit.Assertions.Assertion_Error with Ada.Exceptions.Exception_Message (E);
   end Test;

   ----------
   --  Test procedures

   procedure No_Change (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken;

      Initial_Source  : constant String := "An_Integer := Another_Integer;";
      Expected_Source : constant String := Initial_Source;

      Changes  : Wisi.Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      Expected_KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Initial_Source'Last),
          Stable_Chars   => Base_Buffer_Pos (Initial_Source'Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end No_Change;

   procedure Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  Modeled on de-indent in test/ada_mode-incremental_parse.adb
      Initial_Source  : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --        |10       |20       |30       |40
        "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
        --     |50       |60       |70       |80       |90
        "     is (Float (A));" & ASCII.LF &
        --      |100      |110
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;
        --     |120      |130      |140

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float" & ASCII.LF &
        "    is (Float (A));" & ASCII.LF &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

      Changes  : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "   function"));
         Line_3_Start : Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "     is (Float"));
      begin
         --  De-indent line 2
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Line_2_Start - 1,
             Deleted_Bytes | Deleted_Chars   => 3,
             Inserted_Bytes | Inserted_Chars => 2));

         --  De-indent line 3
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Line_3_Start - (Line_2_Start + 3),
             Deleted_Bytes | Deleted_Chars   => 5,
             Inserted_Bytes | Inserted_Chars => 4));

         --  Rest of code
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Initial_Source'Last) + 1 - (Line_3_Start + 5),
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         --  (indent-rigidly begin end -1) inserts one less space, then deletes
         --  all leading space. We do that in the opposite order here, to test
         --  another case in Edit_Source.

         --  De-indent line 2
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 3));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"  ",
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  That moves line_3_start
         Line_3_Start := @ - 1;

         --  De-indent line 3
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 5));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 4,
             Inserted_Text                                 => +"    ",
             Deleted_Bytes | Deleted_Chars                 => 0));
      end;

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Deindent;

   procedure Complex_Noop (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      Line_3 : constant String := "     is (Float (A));" & ASCII.LF;
      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
        Line_3 &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

      Initial_Source  : constant String := Expected_Source;

      Changes  : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  Modeled on test/ada_mode-incremental_parse.adb; parse after
      --  delete/insert 'is (float(A))'; tests merge delete into KMN insert.

      declare
         use Ada.Strings.Fixed;
         Line_3_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "     is (Float"));
         Line_4_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "end"));
      begin
         --  Delete line 3, insert ';', delete ';', insert line_3. Actually a
         --  noop, but the code doesn't realize the inserted text = the deleted
         --  text. Insert/delete ';' cancels out, so the KMN just has the
         --  line_3 text.
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Line_3_Start - 1,
             Deleted_Bytes | Deleted_Chars   => Line_3'Length,
             Inserted_Bytes | Inserted_Chars => Line_3'Length));

         --  Rest of code
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Initial_Source'Last) + 1 - Line_4_Start,
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         --  Delete line 3
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => Line_3'Length));

         --  insert ';'
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 1,
             Inserted_Text                                 => +";",
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  delete ';'
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 1));

         --  insert line_3.
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + Line_3'Length,
             Inserted_Text                                 => +Line_3,
             Deleted_Bytes | Deleted_Chars                 => 0));
      end;

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Complex_Noop;

   procedure Complex_Noop_Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  Complex_Noop followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  off. It encounters another case in Insert_KMN.

      Changes  : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;

      Line_3 : constant String := "     is (Float (A));" & ASCII.LF;
      Initial_Source  : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
        Line_3 &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float" & ASCII.LF &
        Line_3 (2 .. Line_3'Last) &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

   begin
      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "   function"));
         Line_3_Start :          Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "     is (Float"));
         Line_4_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "end Ada"));
      begin
         --  De-indent line 2
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Line_2_Start - 1,
             Deleted_Bytes | Deleted_Chars   => 3,
             Inserted_Bytes | Inserted_Chars => 2));

         --  delete/insert line 3, de-indent line 3
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Line_3_Start - (Line_2_Start + 3),
             Deleted_Bytes | Deleted_Chars   => Line_3'Length,
             Inserted_Bytes | Inserted_Chars => Line_3'Length - 1));

         --  Rest of code
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Initial_Source'Last) + 1 - Line_4_Start,
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         --  Delete line 3
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => Line_3'Length));

         --  insert ';'
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 1,
             Inserted_Text                                 => +";",
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  delete ';'
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 1));

         --  insert line_3.
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + Line_3'Length,
             Inserted_Text                                 => +Line_3,
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  (indent-rigidly begin end -1) deletes all leading space, then inserts one less
         --  De-indent line 2
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 3));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"  ",
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  That moves line_3_start
         Line_3_Start := @ - 1;

         --  De-indent line 3; the complex noop starts in this delete region
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 5));
         Changes.Append -- ; the complex noop starts in this insert region
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 4,
             Inserted_Text                                 => +"    ",
             Deleted_Bytes | Deleted_Chars                 => 0));
      end;

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Complex_Noop_Deindent;

   procedure Insert_Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  Insert followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  on. It encounters another case in Insert_KMN.

      Initial_Source  : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --  |4    |10       |20       |30       |40
        "   function Func_1 (A : in Integer) return Float;" & ASCII.LF &
        --     |50       |60       |70       |80       |90
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;
      --       |100      |110      |120

      Changes  : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --  |4    |10       |20       |30       |40
        "  function Func_1 (A : in Integer) return Float" & ASCII.LF &
        --     |50       |60       |70       |80       |90
        "    is (Float (A));" & ASCII.LF &
        --       |100      |110
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;
        --       |120      |130      |140

   begin
      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "   function"));
         Line_3_Start :          Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "end Ada"));

         Insert : constant String := ASCII.LF & "     is (Float (A));";
      begin
         --  De-indent line 2
         --  stable_bytes = last_stable_byte - first_stable_byte + 1
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => (Line_2_Start - 1) - 1 + 1,
             Inserted_Bytes | Inserted_Chars => 0,
             Deleted_Bytes | Deleted_Chars   => 1));

         --  Delete ';', insert Insert, deindent new line 3
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => (Line_3_Start - 3) - (Line_2_Start + 1) + 1,
             Inserted_Bytes | Inserted_Chars => Insert'Length - 1,
             Deleted_Bytes | Deleted_Chars   => 1));

         --  Rest of code
         Expected_KMN_List.Append
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Initial_Source'Last) - (Line_3_Start - 1) + 1,
             Inserted_Bytes | Inserted_Chars => 0,
             Deleted_Bytes | Deleted_Chars   => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         --  Delete ';'
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start - 2,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start - 2,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 1));

         --  That moves line_3_start
         Line_3_Start := @ - 1;

         --  insert Insert
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start - 1,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start - 1 + Insert'Length,
             Inserted_Text                                 => +Insert,
             Deleted_Bytes | Deleted_Chars                 => 0));

         --  (indent-rigidly begin end -1) inserts spaces, then deletes spaces (weird!)
         --  De-indent line 2
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"  ",
             Deleted_Bytes | Deleted_Chars                 => 0));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start + 2,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 3));

         --  That moves line_3_start
         Line_3_Start := @ - 1;

         --  De-indent line 3
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 4,
             Inserted_Text                                 => +"    ",
             Deleted_Bytes | Deleted_Chars                 => 0));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start + 4,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 4,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 5));
      end;

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Insert_Deindent;

   procedure Preceding_Comments (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  Insert followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  on. It encounters another case in Insert_KMN.

      Initial_Source  : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --  |4    |10       |20       |30       |40
        "   -- preceding comment 1 " & ASCII.LF &
        "   function Func_1 (A : in Integer) return Float;" & ASCII.LF &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

      Changes  : Change_Lists.List;

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --  |4    |10       |20       |30       |40
        "  -- preceding comment 1 " & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float;" & ASCII.LF &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos
           (Index (Initial_Source, "   -- preceding comment 1"));

         Line_3_Start :  Base_Buffer_Pos := Base_Buffer_Pos (Index (Initial_Source, "   function"));
      begin
         --  (indent-rigidly begin end -1) inserts spaces, then deletes spaces (weird!)
         --  De-indent line 2
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"  ",
             Deleted_Bytes | Deleted_Chars                 => 0));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_2_Start + 2,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_2_Start + 2,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 3));

         --  That moves line_3_start
         Line_3_Start := @ - 1;

         --  De-indent line 3
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 2,
             Inserted_Text                                 => +"  ",
             Deleted_Bytes | Deleted_Chars                 => 0));
         Changes.Append
           ((Begin_Byte_Pos | Begin_Char_Pos               => Line_3_Start + 2,
             Inserted_End_Byte_Pos | Inserted_End_Char_Pos => Line_3_Start + 2,
             Inserted_Text                                 => +"",
             Deleted_Bytes | Deleted_Chars                 => 3));
      end;

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Preceding_Comments;

   procedure Undo_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  From test/ada_mode-interactive_03.adb

      Initial_Source : constant String :=
        "-- Used to get ""error during resume"" with incremental parse." & ASCII.LF &
        "--" & ASCII.LF &
        "-- This is extracted from ada_mode-interactive_2.adb, then expanded." & ASCII.LF &
        "procedure Ada_Mode.Interactive_03" & ASCII.LF &
        "is" & ASCII.LF &
        ASCII.LF &
        "   --EMACSCMD:(progn (end-of-line 7)(delete-char -2)(newline-and-indent))" & ASCII.LF &
        "   --EMACSCMD:(progn (forward-line 5)(execute-kbd-macro ""is begin\nnull;\Nend" &
        " Function_Access_1;\n"")(current-indentation))" & ASCII.LF &
        "   --EMACSRESULT:3" & ASCII.LF &
        "   function Function_Access_1" & ASCII.LF &
        "     (A_Param : in Float)" & ASCII.LF &
        "     return Standard.Float" & ASCII.LF &
        "is begin" & ASCII.LF &
        "      null;" & ASCII.LF &
        "      end Function_Access_1;" & ASCII.LF &
        "   " & ASCII.LF &
        "   --EMACSCMD:(let ((i 1)) (while (< i 9) (setq i (1+ i)) (undo)(wisi-indent-statement)))" & ASCII.LF &
        ASCII.LF &
        "begin" & ASCII.LF &
        "   null;" & ASCII.LF &
        "end Ada_Mode.Interactive_03;" & ASCII.LF;


      Expected_Source : constant String :=
        "-- Used to get ""error during resume"" with incremental parse." & ASCII.LF &
        "--" & ASCII.LF &
        "-- This is extracted from ada_mode-interactive_2.adb, then expanded." & ASCII.LF &
        "procedure Ada_Mode.Interactive_03" & ASCII.LF &
        "is" & ASCII.LF &
        ASCII.LF &
        "   --EMACSCMD:(progn (end-of-line 7)(delete-char -2)(newline-and-indent))" & ASCII.LF &
        "   --EMACSCMD:(progn (forward-line 5)(execute-kbd-macro ""is begin\nnull;\Nend" &
        " Function_Access_1;\n"")(current-indentation))" & ASCII.LF &
        "   --EMACSRESULT:3" & ASCII.LF &
        "   function Function_Access_1" & ASCII.LF &
        "     (A_Param : in Float)" & ASCII.LF &
        "     return Standard.Float" & ASCII.LF &
        "is begin" & ASCII.LF &
        "      null;" & ASCII.LF &
        "      end Function_Access_" & ASCII.LF &
        "   --EMACSCMD:(let ((i 1)) (while (< i 9) (setq i (1+ i)) (undo)(wisi-indent-statement)))" & ASCII.LF &
        ASCII.LF &
        "begin" & ASCII.LF &
        "   null;" & ASCII.LF &
        "end Ada_Mode.Interactive_03;" & ASCII.LF;

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      Changes.Append ((518, 518, 518, 518, +"", 6, 6));
      Changes.Append ((518, 518, 519, 519, +"1", 0, 0));
      Changes.Append ((511, 511, 511, 511, +"", 1, 1));
      Changes.Append ((511, 511, 512, 512, +"a", 0, 0));
      Changes.Append ((502, 502, 502, 502, +"", 1, 1));
      Changes.Append ((502, 502, 503, 503, +"f", 0, 0));
      Changes.Append ((502, 502, 502, 502, +"", 17, 17));
      Changes.Append ((502, 502, 519, 519, +"Function_Access_1", 0, 0));
      Changes.Append ((518, 518, 518, 518, +"", 1, 1));

      Expected_KMN_List.Append ((493, 493, 1, 1, 1, 1));
      Expected_KMN_List.Append ((7, 7, 16, 16, 17, 17));
      Expected_KMN_List.Append ((6, 6, 1, 1, 6, 6));
      Expected_KMN_List.Append ((136, 136, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Undo_01;

   procedure Edit_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  First of systematic tests of all relations of Change to existing KMN.
      --
      --  Change entirely in 1st KMN stable.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String := "Ask never what you must do for my country.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "not" -> "never"
      Changes.Append ((5, 5, 10, 10, +"never", 3, 3));

      Expected_KMN_List.Append ((4, 4, 5, 5, 3, 3));
      Expected_KMN_List.Append ((10, 10, 4, 4, 3, 3));
      Expected_KMN_List.Append ((8, 8, 2, 2, 4, 4));
      Expected_KMN_List.Append ((9, 9, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_01;

   procedure Edit_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  New change inserted length does not affect Edit_Source logic.
      --
      --  New change delete ends at 1st KMN inserted; no need to merge them.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String :=
        "Ask not what y'all_must do for my country.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "you " -> "y'all_"
      Changes.Append ((14, 14, 20, 20, +"y'all_", 4, 4));

      Expected_KMN_List.Append ((13, 13, 6, 6, 4, 4));
      Expected_KMN_List.Append ((0, 0, 4, 4, 3, 3));
      Expected_KMN_List.Append ((8, 8, 2, 2, 4, 4));
      Expected_KMN_List.Append ((9, 9, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_02;

   procedure Edit_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  New change delete ends in 1st KMN inserted; merge them.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String :=
        "Ask not what y'all_ust do for my country.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "you m" -> "y'all_"
      Changes.Append ((14, 14, 20, 20, +"y'all_", 5, 5));

      Expected_KMN_List.Append ((13, 13, 9, 9, 7, 7));
      Expected_KMN_List.Append ((8, 8, 2, 2, 4, 4));
      Expected_KMN_List.Append ((9, 9, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_03;

   procedure Edit_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  New change delete ends after 1st KMN inserted, in 2nd KMN stable;
      --  merge change into 1st KMN.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String :=
        "Ask not what y'all_do for my country.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "you must " -> "y'all_"
      Changes.Append ((14, 14, 20, 20, +"y'all_", 9, 9));

      Expected_KMN_List.Append ((13, 13, 6, 6, 8, 8));
      Expected_KMN_List.Append ((7, 7, 2, 2, 4, 4));
      Expected_KMN_List.Append ((9, 9, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_04;

   procedure Edit_05 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  New change delete ends in 2nd KMN inserted; delete 2nd KMN, merge
      --  change and 2nd into 1st KMN.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String :=
        "Ask not what y'all_doy country.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "you must do for m" -> "y'all_do"
      Changes.Append ((14, 14, 22, 22, +"y'all_do", 17, 17));

      Expected_KMN_List.Append ((13, 13, 9, 9, 19, 19));
      Expected_KMN_List.Append ((9, 9, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_05;

   procedure Edit_06 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;

      --  New change delete ends in final stable; delete 2nd KMN, merge
      --  change and 2nd into 1st KMN, adjust final KMN.

      Initial_Source : constant String :=
        "Ask not what you can do for your country.";
      --          |10       |20       |30

      Expected_Source : constant String :=
        "Ask not what y'all_ountry.";

      Changes : Change_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      --  "can" -> "must"
      Changes.Append ((18, 18, 22, 22, +"must", 3, 3));

      --  "your" -> "my"
      Changes.Append ((30, 30, 32, 32, +"my", 4, 4));

      --  new change "you must do for my c" -> "y'all_"
      Changes.Append ((14, 14, 20, 20, +"y'all_", 20, 20));

      Expected_KMN_List.Append ((13, 13, 6, 6, 21, 21));
      Expected_KMN_List.Append ((7, 7, 0, 0, 0, 0));

      Test
        ("1", Initial_Source, Initial_Source'Last, Changes, Expected_Source, Expected_Source'Last, Expected_KMN_List);
   end Edit_06;

   --  FIXME: systematic coverage of remaining relations between change, cur_kmn, next_kmn

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Deindent'Access, "Deindent");
      Register_Routine (T, Complex_Noop'Access, "Complex_Noop");
      Register_Routine (T, Complex_Noop_Deindent'Access, "Complex_Noop_Deindent");
      Register_Routine (T, Insert_Deindent'Access, "Insert_Deindent");
      Register_Routine (T, Preceding_Comments'Access, "Preceding_Comments");
      Register_Routine (T, Undo_01'Access, "Undo_01");
      Register_Routine (T, Edit_01'Access, "Edit_01");
      Register_Routine (T, Edit_02'Access, "Edit_02");
      Register_Routine (T, Edit_03'Access, "Edit_03");
      Register_Routine (T, Edit_04'Access, "Edit_04");
      Register_Routine (T, Edit_05'Access, "Edit_05");
      Register_Routine (T, Edit_06'Access, "Edit_06");
      --  Register_Routine (T, Edit_07'Access, "Edit_07");
      --  Register_Routine (T, Edit_08'Access, "Edit_08");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_edit_source.adb");
   end Name;

end Test_Edit_Source;
