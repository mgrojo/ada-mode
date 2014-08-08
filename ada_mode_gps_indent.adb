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
with Ada.Text_IO;
with Ada_Analyzer;
with Case_Handling;
with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Symbols;
with Language;
with Line_Buffers;
with UTF8_Utils;
procedure Ada_Mode_GPS_Indent is
   subtype String_Access is GNAT.Strings.String_Access;

   Symbols    : constant GNATCOLL.Symbols.Symbol_Table_Access := GNATCOLL.Symbols.Allocate;
   F          : GNAT.OS_Lib.File_Descriptor;
   Name       : constant String := Ada.Command_Line.Argument (1);
   Buffer     : String_Access;
   Length     : Integer;
   pragma Unreferenced (Length);
   New_Buffer : Line_Buffers.Extended_Line_Buffer;

   procedure Replace_Cb
     (Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String)
   is
   begin
      --  analyze calls replace_cb for ":", ":=" etc. weird. We only
      --  want the leading spaces, for indentation.
      if Replace'Length > 0 and First = 1 then
         Ada.Text_IO.Put_Line (Natural'Image (Line) & ":" & Natural'Image (Last - 1));
      end if;
   end Replace_Cb;

begin
   --  FIXME: change to read from standard_input; need protocol for line count or something
   F := GNAT.OS_Lib.Open_Read (Name, GNAT.OS_Lib.Binary);
   Buffer := new String (1 .. Integer (GNAT.OS_Lib.File_Length (F)));
   Length := GNAT.OS_Lib.Read (F, Buffer.all'Address, Buffer'Length);
   GNAT.OS_Lib.Close (F);

   --  Input text must be UTF8 encoded

   New_Buffer := Line_Buffers.To_Line_Buffer (Buffer.all);

   --  FIXME: stop at desired line (or only pass in that much text?)
   Ada_Analyzer.Analyze_Ada_Source
     (Buffer.all, Symbols,
      Indent_Params =>
        (Indent_Level        => 3, -- FIXME: get from ada-mode once, at process start
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
         Align_On_Colons     => True,
         Align_On_Arrows     => True,
         Align_Decl_On_Colon => True,
         Indent_Comments     => True,
         Stick_Comments      => False),
      Replace => Replace_Cb'Unrestricted_Access);

   Line_Buffers.Free (New_Buffer);
   GNAT.Strings.Free (Buffer);
end Ada_Mode_GPS_Indent;
