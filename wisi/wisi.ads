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
--  Copyright (C) 2012 - 2015 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
package Wisi is

   User_Error : exception; -- error in command line options or parameters

   Syntax_Error : exception; -- Error in user input file

   Not_Found : exception;
   --  something not found; should be handled and converted to Syntax_ or Programmer_Error

   Programmer_Error : exception; -- Error in Wisi Ada code

   type String_Pair_Type is record
      Name  : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Value : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package String_Pair_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (String_Pair_Type);

   type Token_Kind_Type is record
      Kind   : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Tokens : String_Pair_Lists.List;
   end record;

   package Token_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (Token_Kind_Type);

   function Count (Tokens : in Token_Lists.List) return Integer;

   procedure Add_Token
     (Tokens : in out Token_Lists.List;
      Kind   : in     String;
      Name   : in     String;
      Value  : in     String);
   --  Add Name, Value to Kind list in Tokens.

   function Is_In (Tokens : in Token_Lists.List; Kind : in String) return Boolean;
   function Is_In
     (Tokens : in Token_Lists.List;
      Kind   : in String;
      Value  : in String)
     return Boolean;

   type Conflict is record
      Action_A    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      LHS_A       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Action_B    : Standard.Ada.Strings.Unbounded.Unbounded_String;
      LHS_B       : Standard.Ada.Strings.Unbounded.Unbounded_String;
      On          : Standard.Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Conflict_Lists is new Ada.Containers.Doubly_Linked_Lists (Conflict);

   package String_Lists is new Standard.Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type RHS_Type is record
      Production : String_Lists.List; -- Tokens
      Action     : String_Lists.List; -- one string per line
   end record;
   package RHS_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (RHS_Type, "=");

   type Rule_Type is record
      Left_Hand_Side   : Standard.Ada.Strings.Unbounded.Unbounded_String;
      Right_Hand_Sides : RHS_Lists.List;
      Source_Line      : Standard.Ada.Text_IO.Positive_Count;
   end record;

   package Rule_Lists is new Standard.Ada.Containers.Doubly_Linked_Lists (Rule_Type);

   function "+" (Item : in String) return Standard.Ada.Strings.Unbounded.Unbounded_String
     renames Standard.Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (Item : in Standard.Ada.Strings.Unbounded.Unbounded_String) return String
     renames Standard.Ada.Strings.Unbounded.To_String;

   function "+" (List : in String_Lists.List; Item : in String) return String_Lists.List;

   function String_To_String_List (Item : in String) return String_Lists.List;
   function "+" (Item : in String) return String_Lists.List renames String_To_String_List;

   function RHS_To_RHS_List (Item : in RHS_Type) return RHS_Lists.List;
   function "+" (Item : in RHS_Type) return RHS_Lists.List renames RHS_To_RHS_List;

   function "+" (List : in RHS_Lists.List; Item : in RHS_Type) return RHS_Lists.List;

   Verbosity : Integer := 0;

   type Lexer_Type is (Aflex_Lexer, OpenToken_Lexer, Elisp_Lexer);

   procedure Put_Command_Line (Comment_Prefix : in String);
   --  Put command line to current output

end Wisi;
