--  Abstract :
--
--  root package for Wisi grammar compiler; see [3]
--
--  "wisi" is short for "Wisent Indentation engine"; the Emacs 'wisi'
--  package implements an indentation engine based on the Emacs wisent
--  parser.
--
--  "wisent" is the European bison [4].
--
--  The 'wisent' parser generator is the Gnu parser generator
--  implemented in Emacs elisp, as part of the semantic package [2].
--  The parser itself (wisent-parse) is part of Emacs, but the parser
--  compiler (wisent-compile-grammar) is not.
--
--  The input file syntax is the based on Gnu bison syntax [1] with
--  some additions, apparently not documented anywhere. See [3] for
--  the syntax accepted by wisi-compile.
--
--  Reference :
--
--  [1] http://www.gnu.org/software/bison/manual/ (info "(bison)Top")
--  [2] http://cedet.sourceforge.net/semantic.shtml
--  [3] wisi-user-manual.texi
--  [4] http://en.wikipedia.org/wiki/Wisent
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
package Wisi is

   Syntax_Error : exception;

   type Declaration_ID_Type is (Package_ID, Keyword_ID, Token_ID, Start_ID);
   type Declaration_Type (ID : Declaration_ID_Type := Declaration_ID_Type'First) is record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      case ID is
      when Package_ID =>
         null;

      when Keyword_ID =>
         Value : Ada.Strings.Unbounded.Unbounded_String;

      when Token_ID =>
         null;

      when Start_ID =>
         null;
      end case;
   end record;

   package Declaration_Lists is new Ada.Containers.Doubly_Linked_Lists (Declaration_Type);

   package Token_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Production_Lists is new Ada.Containers.Doubly_Linked_Lists (Token_Lists.List, Token_Lists."=");

   type Rule_Type is record
      Left_Hand_Side  : Ada.Strings.Unbounded.Unbounded_String;
      Right_Hand_Side : Production_Lists.List;
      Action          : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Rule_Lists is new Ada.Containers.Doubly_Linked_Lists (Rule_Type);

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Verbose : Boolean := False;
end Wisi;
