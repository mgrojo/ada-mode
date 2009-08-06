-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
-- Copyright (C) 2000 Ted Dennison
--
-- This file is part of the OpenToken package.
--
-- The OpenToken package is free software; you can redistribute it and/or
-- modify it under the terms of the  GNU General Public License as published
-- by the Free Software Foundation; either version 3, or (at your option)
-- any later version. The OpenToken package is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for  more details.  You should have received
-- a copy of the GNU General Public License  distributed with the OpenToken
-- package;  see file GPL.txt.  If not, write to  the Free Software Foundation,
-- 59 Temple Place - Suite 330,  Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------


-----------------------------------------------------------------------------
--  This package contains the library-level objects for the
--  recursive-descent version of Example 4.46 from the Dragon Book.
--
-------------------------------------------------------------------------------

with Ada.Text_IO;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Keyword;
with OpenToken.Text_Feeder.Text_IO;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Selection;
with OpenToken.Token.Sequence;
package ASU_Example_4_46_RD is

   --  The complete list of tokens. No non-terminals in recursive descent.
   type Token_IDs is (Asterix_ID, ID_ID, Equals_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);

   Syntax : constant Tokenizer.Syntax :=
     (Asterix_ID    => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("*")),
      ID_ID         => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("id")),
      Equals_ID     => Tokenizer.Get (OpenToken.Recognizer.Keyword.Get ("=")),
      EOF_ID        => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID => Tokenizer.Get (OpenToken.Recognizer.Character_Set.Get
                                        (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   --  Define all tokens, for use in declaring the grammar. Note that
   --  these must all be variables or pointers to variables. Parsing
   --  stores intermediate results in the non-terminal tokens; the
   --  terminal tokens must also be variables because the parser
   --  doesn't really know the difference.
   --
   --  The non-terminal tokens are pointers (type Handle) to allow for
   --  mutual recursion. So the terminal ones are too, for
   --  consistency, and to reduce the number of '&' and 'or' operators
   --  we need.
   --
   --  Terminal tokens
   Asterix : constant Master_Token.Handle := Syntax (Asterix_ID).Token_Handle;
   ID      : constant Master_Token.Handle := Syntax (ID_ID).Token_Handle;
   Equals  : constant Master_Token.Handle := Syntax (Equals_ID).Token_Handle;
   EOF     : constant Master_Token.Handle := Syntax (EOF_ID).Token_Handle;

   --  Allow infix operators for building productions
   use type OpenToken.Token.Selection.Instance;
   use type OpenToken.Token.Sequence.Instance;
   use OpenToken.Token;

   --  Non-terminal tokens, which define the grammar.
   --
   --  The text in the example in the book looks something like:
   --
   --  S' -> S
   --  S  -> L = R | R
   --  L  -> * R | id
   --  R  -> L
   --
   --  L and R are mutually recursive, so we need a forward reference
   --  for one of them. R is the simplest, so we pick that for the
   --  forward reference; the actual value is set in the body.
   --
   --  This illustrates why we require Selection.New_Selection; if the
   --  "or" in the expression for S actually looked at the contents of
   --  R, it would be dereferencing a null pointer. This grammar does
   --  not have a similar situation for sequence, but it is clear that
   --  other grammars might.
   --
   --  We must use Sequence.New_Instance, not 'new Sequence.Instance',
   --  to avoid accessibility errors.
   --
   --  Note that these must be pointers to variables, not pointers to
   --  constants; the parser stores result values in them.

   R : constant Selection.Handle := new Selection.Instance;

   L : constant Selection.Handle := Selection.New_Instance (Sequence.New_Instance (Asterix & R) or ID);

   S : constant Selection.Handle := Selection.New_Instance (Sequence.New_Instance (L & Equals & R) or R);

   --  This is of type OpenToken.Token.Handle, so it can be passed to
   --  OpenToken.Token.Parse, rather than Sequence.Parse.
   S_Prime : constant OpenToken.Token.Handle := OpenToken.Token.Handle (Sequence.New_Instance (S & EOF));

   --  Create a text feeder for our Input_File.
   Input_File : aliased Ada.Text_IO.File_Type;
   Feeder     : aliased OpenToken.Text_Feeder.Text_IO.Instance :=
     OpenToken.Text_Feeder.Text_IO.Create (Input_File'Access);

   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

end ASU_Example_4_46_RD;
