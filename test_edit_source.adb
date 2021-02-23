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

with AUnit.Checks;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit;
with System.Assertions;
with Wisi;
with WisiToken.Parse;
with WisiToken.Text_IO_Trace;
package body Test_Edit_Source is

   Trace : WisiToken.Text_IO_Trace.Trace;

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (WisiToken.Base_Buffer_Pos);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Parse.KMN;
      Expected : in WisiToken.Parse.KMN)
   is
   begin
      Check (Label & ".stable_bytes", Computed.Stable_Bytes, Expected.Stable_Bytes);
      Check (Label & ".stable_chars", Computed.Stable_Chars, Expected.Stable_Chars);
      Check (Label & ".deleted_bytes", Computed.Deleted_Bytes, Expected.Deleted_Bytes);
      Check (Label & ".deleted_chars", Computed.Deleted_Chars, Expected.Deleted_Chars);
      Check (Label & ".inserted_bytes", Computed.Inserted_Bytes, Expected.Inserted_Bytes);
      Check (Label & ".inserted_chars", Computed.Inserted_Chars, Expected.Inserted_Chars);
   end Check;

   package KMN_Lists_AUnit is new WisiToken.Parse.KMN_Lists.Gen_AUnit (Check);

   Source           : Ada.Strings.Unbounded.String_Access;
   Source_Byte_Last : Integer;
   Source_Char_Last : Integer;

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
   end Check_Valid_KMN;

   ----------
   --  Test procedures

   procedure No_Change (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken;
      use KMN_Lists_AUnit;

      Changes  : Wisi.Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;
   begin
      Source := new String'("An_Integer := Another_Integer;");
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      Wisi.Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

      Expected_KMN_List.Append
        ((Stable_Bytes   => Base_Buffer_Pos (Source'Last),
          Stable_Chars   => Base_Buffer_Pos (Source'Last),
          Deleted_Bytes  => 0,
          Deleted_Chars  => 0,
          Inserted_Bytes => 0,
          Inserted_Chars => 0));

      Check ("1", KMN_List, Expected_KMN_List);
   end No_Change;

   procedure Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;
      use AUnit.Checks;
      use KMN_Lists_AUnit;

      Changes  : Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float" & ASCII.LF &
        "    is (Float (A));" & ASCII.LF &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;
   begin
      --  Modeled on de-indent in test/ada_mode-incremental_parse.adb
      Source := new String'
        ("package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
         --        |10       |20       |30       |40
         "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
         --     |50       |60       |70       |80       |90
         "     is (Float (A));" & ASCII.LF &
         --      |100      |110
         "end Ada_Mode.Incremental_Parse;" & ASCII.LF);
         --     |120      |130      |140
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "   function"));
         Line_3_Start : Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "     is (Float"));
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
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Source'Last) + 1 - (Line_3_Start + 5),
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         Check_Valid_KMN (Expected_KMN_List, Source.all, Expected_Source);

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

      Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

      Check ("1", KMN_List, Expected_KMN_List);

      Check ("2", Source_Byte_Last, Expected_Source'Last);
      Check ("3", Source_Char_Last, Expected_Source'Last);
      Check ("4", Source (1 .. Source_Byte_Last), Expected_Source);
   end Deindent;

   procedure Complex_Noop (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;
      use AUnit.Checks;
      use KMN_Lists_AUnit;

      Changes  : Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;

      Line_3 : constant String := "     is (Float (A));" & ASCII.LF;
      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
        Line_3 &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

   begin
      --  Modeled on test/ada_mode-incremental_parse.adb; parse after
      --  delete/insert 'is (float(A))'; tests merge delete into KMN insert.

      Source := new String'(Expected_Source);
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      declare
         use Ada.Strings.Fixed;
         Line_3_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "     is (Float"));
         Line_4_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "end"));
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
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Source'Last) + 1 - Line_4_Start,
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         Check_Valid_KMN (Expected_KMN_List, Source.all, Expected_Source);

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

      Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

      Check ("1", KMN_List, Expected_KMN_List);

      Check ("2", Source_Byte_Last, Expected_Source'Last);
      Check ("3", Source_Char_Last, Expected_Source'Last);
      Check ("4", Source (1 .. Source_Byte_Last), Expected_Source);
   end Complex_Noop;

   procedure Complex_Noop_Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;
      use AUnit.Checks;
      use KMN_Lists_AUnit;

      Changes  : Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_KMN_List : WisiToken.Parse.KMN_Lists.List;

      Line_3 : constant String := "     is (Float (A));" & ASCII.LF;
      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float" & ASCII.LF &
        Line_3 (2 .. Line_3'Last) &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

   begin
      --  Complex_Noop followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  off. It encounters another case in Insert_KMN.

      Source := new String'
        ("package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
         "   function Func_1 (A : in Integer) return Float" & ASCII.LF &
         Line_3 &
         "end Ada_Mode.Incremental_Parse;" & ASCII.LF);
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "   function"));
         Line_3_Start :          Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "     is (Float"));
         Line_4_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "end Ada"));
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
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Source'Last) + 1 - Line_4_Start,
             Deleted_Bytes | Deleted_Chars   => 0,
             Inserted_Bytes | Inserted_Chars => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         Check_Valid_KMN (Expected_KMN_List, Source.all, Expected_Source);

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

      Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

      Check ("1", KMN_List, Expected_KMN_List);

      Check ("2", Source_Byte_Last, Expected_Source'Last);
      Check ("3", Source_Char_Last, Expected_Source'Last);
      Check ("4", Source (1 .. Source_Byte_Last), Expected_Source);
   end Complex_Noop_Deindent;

   procedure Insert_Deindent (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;
      use AUnit.Checks;
      use KMN_Lists_AUnit;

      Changes  : Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

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
      --  Insert followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  on. It encounters another case in Insert_KMN.

      Source := new String'
        ("package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
         --  |4    |10       |20       |30       |40
         "   function Func_1 (A : in Integer) return Float;" & ASCII.LF &
         --     |50       |60       |70       |80       |90
         "end Ada_Mode.Incremental_Parse;" & ASCII.LF);
         --     |100      |110      |120
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "   function"));
         Line_3_Start :          Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "end Ada"));

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
           ((Stable_Bytes | Stable_Chars     => Buffer_Pos (Source'Last) - (Line_3_Start - 1) + 1,
             Inserted_Bytes | Inserted_Chars => 0,
             Deleted_Bytes | Deleted_Chars   => 0));

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Expected_KMN_List:");
            for KMN of Expected_KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;

         Check_Valid_KMN (Expected_KMN_List, Source.all, Expected_Source);

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

      begin
         Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Computed KMN_List:");
            for KMN of KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
      exception
      when System.Assertions.Assert_Failure =>
         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Computed KMN_List:");
            for KMN of KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
         raise;
      end;

      Check ("kmn_list", KMN_List, Expected_KMN_List);

      Check ("source_byte_last", Source_Byte_Last, Expected_Source'Last);
      Check ("source_char_last", Source_Char_Last, Expected_Source'Last);
      Check ("source", Source (1 .. Source_Byte_Last), Expected_Source);
   end Insert_Deindent;

   procedure Preceding_Comments (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Wisi;
      use WisiToken;
      use AUnit.Checks;

      Changes  : Change_Lists.List;
      KMN_List : WisiToken.Parse.KMN_Lists.List;

      Expected_Source : constant String :=
        "package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
        --  |4    |10       |20       |30       |40
        "  -- preceding comment 1 " & ASCII.LF &
        "  function Func_1 (A : in Integer) return Float;" & ASCII.LF &
        "end Ada_Mode.Incremental_Parse;" & ASCII.LF;

   begin
      --  Insert followed by Deindent, all in one change list. This
      --  happens in test/ada_mode-incremental_parse when run with font-lock
      --  on. It encounters another case in Insert_KMN.

      Source := new String'
        ("package body Ada_Mode.Incremental_Parse is" & ASCII.LF &
         --  |4    |10       |20       |30       |40
         "   -- preceding comment 1 " & ASCII.LF &
         "   function Func_1 (A : in Integer) return Float;" & ASCII.LF &
         "end Ada_Mode.Incremental_Parse;" & ASCII.LF);
      Source_Byte_Last := Source'Last;
      Source_Char_Last := Source'Last;

      declare
         use Ada.Strings.Fixed;
         Line_2_Start : constant Base_Buffer_Pos := Base_Buffer_Pos (Index (Source.all, "   -- preceding comment 1"));
         Line_3_Start :  Base_Buffer_Pos         := Base_Buffer_Pos (Index (Source.all, "   function"));
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

      begin
         Edit_Source (Trace, Source, Source_Byte_Last, Source_Char_Last, Changes, KMN_List);

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Computed KMN_List:");
            for KMN of KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
      exception
      when System.Assertions.Assert_Failure =>
         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line ("Computed KMN_List:");
            for KMN of KMN_List loop
               Ada.Text_IO.Put_Line (Parse.Image (KMN));
            end loop;
         end if;
         raise;
      end;

      if Trace_Tests > Detail then
         Ada.Text_IO.Put_Line ("edited source:");
         Ada.Text_IO.Put_Line (Source (1 .. Source_Byte_Last));
      end if;
      Check ("source_byte_last", Source_Byte_Last, Expected_Source'Last);
      Check ("source_char_last", Source_Char_Last, Expected_Source'Last);
      Check ("source", Source (1 .. Source_Byte_Last), Expected_Source);
   end Preceding_Comments;

   --  FIXME: test change start in prev kmn insert/delete
   --  FIXME: test insert at end

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
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_edit_source.adb");
   end Name;

   overriding procedure Tear_Down (T : in out Test_Case)
   --  Run after each test procedure
   is begin
      Ada.Strings.Unbounded.Free (Source);
   end Tear_Down;

end Test_Edit_Source;
