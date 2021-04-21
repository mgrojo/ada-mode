--  Abstract :
--
--  An abstract lexer interface.
--
--  Copyright (C) 2014 - 2015, 2017 - 2021 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

pragma Warnings (Off, "license of withed unit ""GNATCOLL.Mmap"" may be inconsistent");

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNATCOLL.Mmap;
package WisiToken.Lexer is

   type Token is record
      --  Information provided by the lexer.

      ID : Token_ID := Invalid_Token_ID;

      Byte_Region : Buffer_Region := Null_Buffer_Region;
      --  Index into the Lexer buffer for the token text.

      Char_Region : Buffer_Region := Null_Buffer_Region;
      --  Character position, useful for finding the token location in Emacs
      --  buffers.

      Line_Region : WisiToken.Line_Region := Null_Line_Region;
   end record;

   function Column (Token : in Lexer.Token; Line_Begin_Char_Pos : in Buffer_Pos) return Ada.Text_IO.Count;

   function Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  ID and Char_Region

   function Full_Image
     (Item       : in Token;
      Descriptor : in WisiToken.Descriptor)
     return String;
   --  All fields.

   Invalid_Token : constant Token := (others => <>);

   package Token_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Positive_Index_Type, Token, Default_Element => (others => <>));

   function Image is new Token_Arrays.Gen_Image_Aux (WisiToken.Descriptor, Trimmed_Image, Image);

   type Error is record
      Char_Pos : Buffer_Pos := Invalid_Buffer_Pos;
      --  Character at that position is not recognized as part of a token.

      Recover_Char : String (1 .. 4) := (others => ASCII.NUL);
      --  If the error was corrected, the character (in UTF-8 encoding) that
      --  was inserted; unused trailing bytes set to ASCII.NUL. Otherwise,
      --  all ASCII.Nul.
   end record;

   package Error_Lists is new Ada.Containers.Doubly_Linked_Lists (Error);

   type Instance
   is abstract new Ada.Finalization.Limited_Controlled with record
      Descriptor : WisiToken.Descriptor_Access_Constant;
      Errors     : Error_Lists.List;
   end record;

   subtype Class is Instance'Class;

   type Handle is access all Class;

   function Has_Source (Lexer : access constant Instance) return Boolean
   is (False);
   --  True if one of Reset_* has been called; lexer has source to process.

   procedure Set_Verbosity
     (Lexer     : in Instance;
      Verbosity : in Integer)
   is null;

   procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from Input.

   procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      Input_Last : in     Integer;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from Input (Input'First
   --  .. Input_Last). Input'First is Begin_Byte. File_Name is used for
   --  error messages.

   procedure Reset_With_File
     (Lexer      : in out Instance;
      File_Name  : in     String;
      Begin_Byte : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is abstract
   with Post'Class => Lexer.Has_Source;
   --  Reset Lexer to start a new parse, reading from File_Name. If
   --  Begin_Pos, End_Pos /= Invalid_Buffer_Pos, only parse that portion
   --  of the file.
   --
   --  Raises Ada.IO_Exceptions.Name_Error if File_Name cannot be opened.

   procedure Reset (Lexer : in out Instance) is abstract
   with Pre'Class => Lexer.Has_Source,
     Post'Class => Lexer.Has_Source;
   --  Reset Lexer, read from previous source.

   procedure Discard_Rest_Of_Input (Lexer : in out Instance) is abstract;
   --  If reading input from a stream, abort reading (or force it to
   --  complete); Find_Next will not be called before another Reset.

   function Buffer_Region_Byte (Lexer : in Instance) return Buffer_Region is abstract;

   function Buffer_Text (Lexer : in Instance; Byte_Region : in Buffer_Region) return String is abstract;
   --  Return text from internal buffer, given region in byte position.

   procedure Set_Position
     (Lexer         : in out Instance;
      Byte_Position : in     Buffer_Pos;
      Char_Position : in     Buffer_Pos;
      Line          : in     Line_Number_Type)
     is abstract;
   --  Set the current position in the source buffer; Find_Next will
   --  start there. Prev_Token_ID should be Descriptor.New_Line_ID or
   --  Invalid_Token_ID; it is used for First.

   function Find_Next
     (Lexer : in out Instance;
      Token :    out WisiToken.Lexer.Token)
     return Boolean is abstract;
   --  Set Token to the next token from the input stream.
   --
   --  If there is a recovered error, adds an entry to Lexer.Errors (with
   --  Recover_Token invalid). Unrecognized characters are skipped;
   --  missing quotes are inserted at the found quote. There can be more
   --  than one error entry for one call to Find_Next, if several
   --  unrecognized characters are skipped. If the recovery inserted a
   --  missing quote, it is the last entry in Errors, the returned token
   --  is an empty string literal, and Find_Next returns True.
   --
   --  If there is a non-recoverable error, raises Fatal_Error with an
   --  appropriate message.
   --
   --  Otherwise returns False.
   --
   --  Token.Char_Region, Token.Byte_Region are the character and byte
   --  position of the start and end of token, in the internal buffer,
   --  1-indexed. Char_Region and Byte_Region differ when text is UTF-8
   --  or other multi-byte encoding, and when line endings are two byte.
   --
   --  Token.Line is the line number in which recent token starts.
   --  If the underlying text feeder does not support the notion of
   --  'line', returns Invalid_Line_Number.

   function File_Name (Lexer : in Instance) return String is abstract;
   --  Return input file name; empty string if there is no file.

   procedure Begin_Pos
     (Lexer      : in     Instance;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   is abstract
   with Pre'Class => Lexer.Has_Source;
   --  Return values from Reset*.

private

   type Source_Labels is (String_Label, File_Label);

   type Source (Label : Source_Labels := Source_Labels'First) is record
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Not saved in Mapped_File, may be empty for String_Label

      Buffer_Nominal_First_Byte : Buffer_Pos       := Buffer_Pos'First;
      Buffer_Nominal_First_Char : Buffer_Pos       := Buffer_Pos'First;
      Line_Nominal_First        : Line_Number_Type := Line_Number_Type'First;
      Buffer_Last               : Natural          := 0; -- allow empty input string

      case Label is
      when String_Label =>
         Buffer      : Ada.Strings.Unbounded.String_Access;
         User_Buffer : Boolean := False;
         --  If User_Buffer is True, user provided buffer and will deallocate
         --  it. Otherwise we must deallocate it.

         --  Buffer_Nominal_First, Line_Nominal_First are 1.

      when File_Label =>

         --  The input is memory mapped from the following, which must be closed:
         File        : GNATCOLL.Mmap.Mapped_File;
         Region      : GNATCOLL.Mmap.Mapped_Region;
         --  Region always has first character at offset 0.

         --  Buffer_Nominal_First is Begin_Pos. Line_Nominal_First is
         --  Begin_Line.
      end case;
   end record;

   procedure Finalize (Object : in out Source);

   function Has_Source (Object : in Source) return Boolean;

   --  True if one of Reset_* has been called; lexer has source to process.
   function Buffer_Region_Byte (Object : in Source) return Buffer_Region;

   function Buffer (Source : in Lexer.Source) return GNATCOLL.Mmap.Str_Access;
   --  The bounds on the result are not present; 'First, 'Last are not
   --  reliable. If Source_Label is String_label, actual bounds are
   --  Source.Buffer'First, 'Last. Otherwise, actual bounds are 1 ..
   --  Source.Buffer_Last. Indexing is reliable.

   function File_Name (Source : in Lexer.Source) return String;
   function To_Char_Pos (Source : in Lexer.Source; Lexer_Char_Pos : in Integer) return Base_Buffer_Pos;
   procedure Begin_Pos
     (Object     : in     Source;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type);

end WisiToken.Lexer;
