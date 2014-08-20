--  Abstract :
--
--  Back end for Emacs Ada mode indentation engine, using GPS
--  indentation code.
--
--  Copyright (C) 2014  All Rights Reserved.
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Analyzer;
with Case_Handling;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Symbols;
with Language;
procedure Ada_Mode_GPS_Indent is

   Programmer_Error : exception;

   Prompt : constant String := "GPS_Indent> ";

   procedure Usage
   is
   begin
      Put_Line ("Prompt is '" & Prompt & "'");
      Put_Line ("commands are case sensitive");
      Put_Line ("each command starts with a two-character decimal count of bytes in command");

      Put_Line ("Commands: ");

      Put_Line ("NNcompute_indent <line> <text_byte_count><text>");
      Put_Line ("  first line is 1 (emacs convention)");
      Put_Line ("  text must be UTF8 encoded");
      Put_Line ("  outputs: <indent><newline>");
      Put_Line ("  no indent is 0 (emacs convention)");

      Put_Line ("04exit");

      --  FIXME: need protocol for indent settings

   end Usage;

   function Get_Command_Length return Integer
   is
      Temp : aliased String (1 .. 2) := "  ";
      Read_Bytes : constant Integer := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Temp'Address, 2);
   begin
      if Read_Bytes /= 2 then
         raise Programmer_Error with "2 bytes of command byte count not provided; got" &
           Integer'Image (Read_Bytes) & "'" & Temp & "'";
      end if;
      return Integer'Value (Temp);
   end Get_Command_Length;

   function Get_Integer
     (Source : in     String;
      Last   : in out Integer)
     return Integer
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      First : constant Integer := Last + 2;
   begin
      Last := Index
        (Source  => Source,
         Pattern => " ",
         From    => First);

      if Last = 0 then
         Last := Source'Last;
      else
         Last := Last - 1;
      end if;

      return Integer'Value (Source (First .. Last));
   exception
   when E : others =>
      Put_Line ("bad integer '" & Source (First .. Source'Last) & "'");
      Put_Line ("Exception : " & Exception_Name (E));
      Put_Line (Exception_Message (E));
      raise;
   end Get_Integer;

   procedure Replace_Cb
     (Line    : in Natural;
      First   : in Natural;
      Last    : in Natural;
      Replace : in String)
   is
      pragma Unreferenced (Line);
      pragma Unreferenced (Last);
   begin
      --  analyze calls replace_cb for ":", ":=" etc. We only
      --  want the leading spaces, for indentation.
      if Replace'Length > 0 and First = 1 then
         Put_Line (Natural'Image (Replace'Length));
      end if;
   end Replace_Cb;

begin

   Commands :
   loop
      Put (Prompt); Flush;

      declare
         use Ada.Strings.Fixed;
         Command_Length : constant Integer := Get_Command_Length;
         Command_Line   : aliased String (1 .. Command_Length);

         Read_Bytes : Integer := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Command_Line'Address, Command_Length);
         Last       : Integer := Index (Source => Command_Line, Pattern => " ");
      begin
         if Read_Bytes /= Command_Length then
            raise Programmer_Error with
              "Read_Bytes" & Integer'Image (Read_Bytes) & " /= Command_Length" & Integer'Image (Command_Length);
         end if;

         if Last = 0 then
            Last := Command_Line'Last;
         else
            Last := Last - 1;
         end if;

         if Command_Line (1 .. Last) = "exit" then
            exit Commands;

         elsif Command_Line (1 .. Last) = "compute_indent" then
            declare
               use Ada.Strings.Unbounded;

               Indent_Line : constant Integer      := Get_Integer (Command_Line, Last);
               Byte_Count  : constant Integer      := Get_Integer (Command_Line, Last);
               Buffer      : aliased String_Access := new String (1 .. Byte_Count);
            begin
               Read_Bytes := GNAT.OS_Lib.Read (GNAT.OS_Lib.Standin, Buffer.all'Address, Byte_Count);

               if Read_Bytes /= Byte_Count then
                  Put_Line ("Read_Bytes" & Integer'Image (Read_Bytes) & " /= Byte_Count" & Integer'Image (Byte_Count));
               else
                  Ada_Analyzer.Analyze_Ada_Source
                    (Buffer.all, GNATCOLL.Symbols.Allocate,
                     Indent_Params          =>
                       (Indent_Level        => 3, -- FIXME: get from ada-mode
                        Indent_Continue     => 2,
                        Indent_Decl         => 2,
                        Indent_Conditional  => 1,
                        Indent_Record       => 3,
                        Indent_Case_Extra   => Language.Automatic,
                        Casing_Policy       => Case_Handling.Disabled,
                        Reserved_Casing     => Case_Handling.Unchanged,
                        Ident_Casing        => Case_Handling.Unchanged,
                        Format_Operators    => True,
                        Use_Tabs            => False,
                        Align_On_Colons     => True, -- FIXME: get from ada-mdoe
                        Align_On_Arrows     => True,
                        Align_Decl_On_Colon => False,
                        Indent_Comments     => True,
                        Stick_Comments      => False),
                     From                   => Indent_Line,
                     To                     => Indent_Line,
                     Replace                => Replace_Cb'Unrestricted_Access);
               end if;
               Free (Buffer);
            exception
               when E : others =>
                  declare
                     use Ada.Exceptions;
                     use GNAT.Traceback.Symbolic;
                  begin
                     Put_Line ("analyze failed on '" & Buffer.all & "'");
                     Put_Line ("Exception : " & Exception_Name (E));
                     Put_Line (Exception_Message (E));
                     Put_Line (Symbolic_Traceback (E));
                  end;
            end;

         else
            Put_Line ("unrecognized command '" & Command_Line & "'");
            Usage;
         end if;
      exception
      when E : Programmer_Error =>
         declare
            use Ada.Exceptions;
         begin
            Put_Line (Exception_Message (E));
            Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            exit Commands;
         end;

      when E : others =>
         declare
            use Ada.Exceptions;
            use GNAT.Traceback.Symbolic;
         begin
            Put_Line ("Bad command '" & Command_Line & "'");
            Put_Line ("Exception : " & Exception_Name (E));
            Put_Line (Exception_Message (E));
            Put_Line (Symbolic_Traceback (E));
            Usage;
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            exit Commands;
         end;
      end;
   end loop Commands;

end Ada_Mode_GPS_Indent;
