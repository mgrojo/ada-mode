--  Abstract :
--
--  Output Ada code implementing the grammar defined by Declarations,
--  Rules.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
procedure Wisi.Output
  (Output_File_Root : in String;
   Prologue         : in String_Lists.List;
   Declarations     : in Declaration_Lists.List;
   Rules            : in Rule_Lists.List)
is
   Output_File : File_Type;

   function To_Ada (Item : in String) return String
   is
      use Ada.Characters.Handling;
      Result : String := Item;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '.';
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result;
   end To_Ada;

   procedure Create_Parents (Name : in String)
   is
      use Ada.Strings.Fixed;
      Last  : Integer := -1 + Index (Pattern => "-", Source => Name);
      File  : File_Type;
   begin
      loop
         Create (File, Out_File, Name (Name'First .. Last) & ".ads");
         Put_Line (File, "package " & To_Ada (Name (Name'First .. Last)) & " is");
         Put_Line (File, "end " & To_Ada (Name (Name'First .. Last)) & ";");
         Close (File);
         exit when Last = Name'Last;
         Last := Index (Pattern => "-", Source => Name, From => Last + 2);
         if Last = 0 then
            Last := Name'Last;
         else
            Last := Last - 1;
         end if;
      end loop;
   end Create_Parents;

   Package_Name : constant String := "Wisi." & To_Ada (Output_File_Root) & ".Generate";

   Indent : Positive_Count := 1;

   procedure Indent_Line (Text : in String)
   is begin
      Set_Col (Indent);
      Put_Line (Text);
   end Indent_Line;

begin
   Create_Parents ("wisi-" & Output_File_Root);
   Create (Output_File, Out_File, "wisi-" & Output_File_Root & "-generate.adb");
   Set_Output (Output_File);
   Put_Line ("procedure " & Package_Name);
   Put_Line ("is");
   Indent := Indent + 3;

   Indent_Line ("Prologue : constant String :=");

   declare
      use String_Lists;
      Line : Cursor := Prologue.First;
   begin
      loop
         Set_Col (Indent);
         Put ('"' & Element (Line) & '"');
         Next (Line);

         if Line = No_Element then
            Put_Line (";");
            exit;
         else
            Put_Line ("&");
         end if;
      end loop;
   end;

   New_Line;
   Indent_Line ("type Token_IDs is");
   Indent_Line ("  (");
   Indent := Indent + 3;
   for Decl of Declarations loop
      case Decl.ID is
      when Keyword_ID =>
         Indent_Line (-Decl.Name & "_ID,"); -- avoid collision with Ada reserved words
      when others =>
         null;
      end case;
   end loop;
   Indent_Line ("--  non-terminals");
   declare
      use Rule_Lists;
      I : Cursor := Rules.First;
   begin
      loop
         Set_Col (Indent);
         Put (-Element (I).Left_Hand_Side);
         Next (I);
         if I = No_Element then
            Put_Line ("_ID);");
            exit;
         else
            Put_Line ("_ID,");
         end if;
      end loop;
   end;
   Indent := Indent - 3;

   Put_Line ("begin");

   Indent_Line ("null;");

   Put_Line ("end " & Package_Name & ";");
   Close (Output_File);
end Wisi.Output;
