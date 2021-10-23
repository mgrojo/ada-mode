--  Abstract:
--
--  See spec
--
--  Copyright (C) 2015, 2017 - 2021 Free Software Foundation, Inc.
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

with Ada.Characters.Latin_1;
with SAL;
package body WisiToken.Lexer.Regexp is

   function Find_Best_Match (Lexer : in out Instance) return Boolean
   is
      --  Find the longest matching character sequence in the buffer that
      --  matches a token.
      --
      --  Return True if a token is matched, False if not.

      use WisiToken.Regexp;

      Current_Char         : Integer := Lexer.Buffer_Head;
      Current_State        : Match_State;
      Current_Match_Length : Integer := 0;
      Best_Match_ID        : Token_ID;
      Best_Match_Length    : Natural := 0;
      Still_Matching       : Boolean := False;
   begin
      --  We only support Reset_With_String.

      if Current_Char > Lexer.Source.Buffer'Last then
         Lexer.ID := Lexer.Descriptor.EOI_ID;
         Lexer.Lexeme_Head := Lexer.Buffer_Head;
         Lexer.Lexeme_Tail := Lexer.Buffer_Head - 1;
         return True;
      end if;

      for I in Lexer.Syntax'Range loop
         Clear (Lexer.Syntax (I).Regexp);
      end loop;

      loop
         Still_Matching := False;

         for I in Lexer.Syntax'Range loop
            if State (Lexer.Syntax (I).Regexp) /= WisiToken.Regexp.Error then
               Current_State := Match
                 (Lexer.Syntax (I).Regexp,
                  Lexer.Source.Buffer (Lexer.Buffer_Head .. Lexer.Source.Buffer'Last),
                  Current_Char);

               case Current_State is
               when Matching =>
                  Still_Matching := True;

               when Final =>
                  Still_Matching := True;

                  Current_Match_Length := Current_Char - Lexer.Buffer_Head + 1;

                  if Best_Match_Length < Current_Match_Length then
                     Best_Match_ID  := I;
                     Best_Match_Length := Current_Match_Length;
                  end if;

               when WisiToken.Regexp.Error =>
                  null;
               end case;
            end if;
         end loop;

         exit when (not Still_Matching) or else (Current_Char = Lexer.Source.Buffer'Last);

         if Best_Match_Length = Lexer.Source.Buffer'Length then
            raise SAL.Programmer_Error with
              "token larger than buffer size of" & Integer'Image (Lexer.Source.Buffer'Length);
         end if;

         Current_Char := Current_Char + 1;
      end loop;

      if Best_Match_Length > 0 then
         Lexer.Lexeme_Head := Lexer.Buffer_Head;
         Lexer.Lexeme_Tail := Lexer.Buffer_Head + Best_Match_Length - 1;
         Lexer.ID          := Best_Match_ID;

         if Lexer.Lexeme_Head = Lexer.Source.Buffer'Last and
           Lexer.Source.Buffer (Lexer.Lexeme_Head) = Ada.Characters.Latin_1.EOT
         then
            --  matched EOF; repeat that next time
            null;
         else
            Lexer.Buffer_Head := Lexer.Lexeme_Tail + 1;
         end if;
         return True;

      elsif Current_Char = Lexer.Source.Buffer'Last then
         Lexer.ID := Lexer.Descriptor.EOI_ID;
         Lexer.Buffer_Head := Lexer.Buffer_Head + 1;
         return True;

      else
         return False;
      end if;

   end Find_Best_Match;

   ----------
   --  Public subprograms

   function Get
     (Regexp         : in String;
      Case_Sensitive : in Boolean := True;
      Report         : in Boolean := True)
      return Syntax_Item
   is begin
      return (WisiToken.Regexp.Compile (Regexp, Case_Sensitive), Report);
   end Get;

   type Instance_Access is access Instance; --  silence compiler warning

   function New_Lexer
     (Descriptor : in WisiToken.Descriptor_Access_Constant;
      Syntax     : in WisiToken.Lexer.Regexp.Syntax)
     return WisiToken.Lexer.Handle
   is
      New_Lexer : constant Instance_Access := new Instance (Syntax'Last);
   begin
      New_Lexer.Descriptor := Descriptor;
      New_Lexer.Syntax := Syntax;

      return Handle (New_Lexer);
   end New_Lexer;

   overriding function Has_Source (Lexer : access constant Instance) return Boolean
   is begin
      return Has_Source (Lexer.Source);
   end Has_Source;

   overriding procedure Finalize (Object : in out Instance)
   is begin
      Finalize (Object.Source);
   end Finalize;

   overriding procedure Reset_With_String
     (Lexer      : in out Instance;
      Input      : in     String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is begin
      Finalize (Lexer);

      Lexer.Source :=
        (Label                     => String_Label,
         File_Name                 => +"",
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer                    => new String'(Input),
         Buffer_Last               => Input'Last,
         User_Buffer               => False);

      Reset (Lexer);
   end Reset_With_String;

   overriding procedure Reset_With_String_Access
     (Lexer      : in out Instance;
      Input      : in     Ada.Strings.Unbounded.String_Access;
      Input_Last : in     Integer;
      File_Name  : in     Ada.Strings.Unbounded.Unbounded_String;
      Begin_Char : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line : in     Line_Number_Type := Line_Number_Type'First)
   is begin
      Finalize (Lexer);

      Lexer.Source :=
        (Label       => String_Label,
         File_Name   => File_Name,
         Buffer_Nominal_First_Byte => Base_Buffer_Pos (Input'First),
         Buffer_Nominal_First_Char => Begin_Char,
         Line_Nominal_First        => Begin_Line,
         Buffer      => Input,
         Buffer_Last => Input_Last,
         User_Buffer => True);

      Reset (Lexer);
   end Reset_With_String_Access;

   overriding procedure Reset_With_File
     (Lexer          : in out Instance;
      File_Name      : in     String;
      Begin_Byte_Pos : in     Buffer_Pos       := Invalid_Buffer_Pos;
      End_Byte_Pos   : in     Buffer_Pos       := Invalid_Buffer_Pos;
      Begin_Char     : in     Buffer_Pos       := Buffer_Pos'First;
      Begin_Line     : in     Line_Number_Type := Line_Number_Type'First)
   is
      pragma Unreferenced (File_Name, Begin_Byte_Pos, End_Byte_Pos, Begin_Char, Begin_Line);
   begin
      Finalize (Lexer);

      raise SAL.Not_Implemented;
   end Reset_With_File;

   overriding procedure Reset
     (Lexer : in out Instance)
   is begin
      Lexer.Lexeme_Head := Lexer.Source.Buffer'First;
      Lexer.Lexeme_Tail := Lexer.Source.Buffer'First - 1;
      Lexer.ID          := Invalid_Token_ID;
      Lexer.Buffer_Head := Lexer.Source.Buffer'First;
   end Reset;

   overriding function Find_Next
     (Lexer : in out Instance;
      Token :    out WisiToken.Lexer.Token)
     return Boolean
   is begin
      loop
         if not Find_Best_Match (Lexer) then
            if Lexer.Buffer_Head > Lexer.Source.Buffer'Last then
               raise Syntax_Error with "Unrecognized EOF";
            else
               raise Syntax_Error with "Unrecognized character '" & Lexer.Source.Buffer (Lexer.Buffer_Head) & "'";
            end if;
         end if;

         exit when Lexer.Syntax (Lexer.ID).Report;

      end loop;

      Token :=
        (ID          => Lexer.ID,
         Byte_Region => (Buffer_Pos (Lexer.Lexeme_Head), Buffer_Pos (Lexer.Lexeme_Tail)),
         Char_Region => (Buffer_Pos (Lexer.Lexeme_Head), Buffer_Pos (Lexer.Lexeme_Tail)),
         Line_Region => Null_Line_Region);

      return False;
   end Find_Next;

   overriding function Buffer_Region_Byte (Lexer : in Instance) return Buffer_Region
   is begin
      return Buffer_Region_Byte (Lexer.Source);
   end Buffer_Region_Byte;

   overriding function Buffer_Text (Lexer : in Instance; Byte_Region : in Buffer_Region) return String
   is begin
      return Lexer.Source.Buffer (Integer (Byte_Region.First) .. Integer (Byte_Region.Last));
   end Buffer_Text;

   overriding
   procedure Begin_Pos
     (Lexer      : in     Instance;
      Begin_Byte :    out Buffer_Pos;
      Begin_Char :    out Buffer_Pos;
      Begin_Line :    out Line_Number_Type)
   is begin
      Begin_Pos (Lexer.Source, Begin_Byte, Begin_Char, Begin_Line);
   end Begin_Pos;

   overriding
   function Is_Comment
     (Lexer : in Instance;
      ID    : in Token_ID)
     return Boolean
   is
      pragma Unreferenced (Lexer, ID);
   begin
      --  regexp lexer only used in unit tests
      raise SAL.Not_Implemented;
      return False;
   end Is_Comment;

   overriding
   function Find_Comment_End
     (Lexer         : in Instance;
      ID            : in Token_ID;
      Comment_Start : in Buffer_Pos)
     return Buffer_Pos
   is
      pragma Unreferenced (Lexer, ID, Comment_Start);
   begin
      --  regexp lexer only used in unit tests
      raise SAL.Not_Implemented;
      return Buffer_Pos'First;
   end Find_Comment_End;

   overriding
   function Line_Begin_Char_Pos
     (Lexer : in Instance;
      Token : in WisiToken.Lexer.Token;
      Line  : in Line_Number_Type)
     return Base_Buffer_Pos
   is
      pragma Unreferenced (Lexer, Token, Line);
   begin
      --  regexp lexer only used in unit tests
      raise SAL.Not_Implemented;
      return Invalid_Buffer_Pos;
   end Line_Begin_Char_Pos;

   overriding
   function Contains_New_Line
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region)
     return Boolean
   is begin
      return Contains_New_Line (Lexer.Source, Byte_Region);
   end Contains_New_Line;

   overriding
   function New_Line_Count
     (Lexer       : in Instance;
      Byte_Region : in Buffer_Region)
     return Base_Line_Number_Type
   is
      pragma Unreferenced (Lexer, Byte_Region);
   begin
      raise SAL.Not_Implemented;
      return Base_Line_Number_Type'First;
   end New_Line_Count;

end WisiToken.Lexer.Regexp;
