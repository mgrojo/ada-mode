--  Abstract :
--
--  See spec
--
--  Copyright (C) 2012, 2013, 2015, 2017 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with Wisi.Utils; use Wisi.Utils;
package body Wisi.Output_Elisp_Common is

   function Elisp_Name_To_Ada (Elisp_Name : in String) return String
   is
      Result : String := Elisp_Name;
   begin
      Result (Result'First) := To_Upper (Result (Result'First));
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '_';
            Result (I + 1) := To_Upper (Result (I + 1));
         elsif Result (I) = '_' then
            Result (I + 1) := To_Upper (Result (I + 1));
         end if;
      end loop;
      return Result & "_ID"; -- Some elisp names may be Ada reserved words;
   end Elisp_Name_To_Ada;

   procedure Indent_Keyword_Table_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Keywords         : in     String_Pair_Lists.List;
      EOI_Name         : in     Standard.Ada.Strings.Unbounded.Unbounded_String;
      Image            : access function (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String)
   is
      use Standard.Ada.Text_IO;
   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-keyword-table");
      Indent_Line ("  (semantic-lex-make-keyword-table");
      Indent_Line ("   '(");
      Indent := Indent + 5;
      for Pair of Keywords loop
         Indent_Line ("(" & (-Pair.Value) & " . " & Image (Pair.Name) & ")");
      end loop;
      Indent_Line ("(""$eoi"" . " & Image (EOI_Name) & ")");
      Indent_Line (")");
      Indent := Indent - 2;
      Indent_Line ("nil))");
      Indent := Indent - 3;
      New_Line;
   end Indent_Keyword_Table_Elisp;

   procedure Indent_Token_Table_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Tokens           : in     Token_Lists.List;
      Image            : access function (Name : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String)
   is
      use Standard.Ada.Strings.Unbounded;
      use Standard.Ada.Text_IO;
   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-token-table");
      Indent_Line ("  (semantic-lex-make-type-table");
      Indent_Line ("   '(");
      Indent := Indent + 5;
      for Kind of Tokens loop
         if not (-Kind.Kind = """line_comment""" or -Kind.Kind = """whitespace""") then
            Indent_Line ("(" & (-Kind.Kind));
            Indent := Indent + 1;
            for Token of Kind.Tokens loop
               if 0 = Length (Token.Value) then
                  Indent_Line ("(" & Image (Token.Name) & ")");
               else
                  if -Kind.Kind = """number""" then
                     --  allow for (<token> <number-p> <require>)
                     Indent_Line ("(" & Image (Token.Name) & " " & (-Token.Value) & ")");
                  else
                     Indent_Line ("(" & Image (Token.Name) & " . " & (-Token.Value) & ")");
                  end if;
               end if;
            end loop;
            Indent_Line (")");
            Indent := Indent - 1;
         end if;
      end loop;
      Indent_Line (")");
      Indent := Indent - 2;
      Indent_Line ("nil))");
      Indent := Indent - 3;
      New_Line;
   end Indent_Token_Table_Elisp;

   procedure Indent_Names_Elisp
     (Output_File_Root : in     String;
      Label            : in     String;
      Names            : in     String_Lists.List)
   is
      use Standard.Ada.Text_IO;
   begin
      Indent_Line ("(defconst " & Output_File_Root & "-" & Label & "-names");
      Indent_Line ("  [");
      Indent := Indent + 3;
      for Name of Names loop
         Indent_Line (Name);
      end loop;
      Indent_Line ("])");
      Indent := Indent - 3;
      New_Line;
   end Indent_Names_Elisp;

end Wisi.Output_Elisp_Common;
