-------------------------------------------------------------------------------
--
-- Copyright (C) 2009, 2013 Stephe Leake
--
--  This file is part of the OpenToken package.
--
--  The OpenToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The OpenToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the OpenToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  This example is a recursive-decent implementation of Example 5.10 from
--  [1] "Compilers Principles, Techniques, and Tools" by Aho, Sethi, and Ullman
--  (aka: "The Dragon Book"). See README.text for more discussion.
-------------------------------------------------------------------------------

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.Linked_List;
with OpenToken.Token.Selection;
with OpenToken.Token.Sequence;
package ASU_Example_5_10_RD_Commute is

   --  The complete list of tokens. No non-terminals in recursive descent.
   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_Sign_ID,
                      Multiply_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);
   package Integer_Token is new Master_Token.Integer;

   Syntax : constant Tokenizer.Syntax :=
     (Multiply_ID       => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("*")),
      Left_Paren_ID     => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("(")),
      Right_Paren_ID    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get (")")),
      Plus_Sign_ID      => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("+")),
      Integer_ID        => Tokenizer.Get
        (Recognizer     => OpenToken.Recognizer.Integer.Get
           (Allow_Signs => False),
         New_Token      => Integer_Token.Get (Integer_ID)),
      EOF_ID            => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID     => Tokenizer.Get
        (Recognizer => OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --------------------------------------------------------------------------
   --  Our custom token types. The grammar is:
   --
   --  L -> E         print (L.val)
   --  E -> E + T     E.val := E1.val + T.val
   --  E -> T
   --  T -> T * F     T.val := T1.val * F.val
   --  T -> F
   --  F -> ( E )     F.val := E.val
   --  F -> digit
   --
   --  If we implement the grammar literally, the parser enters an
   --  infinite loop.
   --
   --  The problem is that the first element of a production is the
   --  same as the result of the production.
   --
   --  So we rearrange the grammar to avoid this, taking advantage of
   --  the commutivity of + and *.
   --
   --  This grammar enforces operator precedence, but because of the
   --  multiple recursion, it requires a separate stack of operands.
   --  The sequence and selection tokens cannot provide the stack for
   --  us.
   --
   --  L -> E EOF     print (pop)
   --  E -> T + E     push (pop + pop)
   --  E -> T
   --  T -> F * T     push (pop * pop)
   --  T -> F
   --  F -> ( E )
   --  F -> integer   push (integer)
   --
   --  This grammar requires infinite lookaheads; it needs to see the
   --  + or * of the sequences for E and T, _after_ a parentheses.

   --  Since we are providing our own operand stack, we can use the
   --  root Selection and Sequence token types. Here we declare the
   --  actions we need.

   procedure Clear_Stack;

   procedure Build_Selection
     (Match : in out OpenToken.Token.Selection.Instance;
      From  : in     OpenToken.Token.Class);
   procedure Build_Print
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Plus
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Multiply
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Parens
     (Match : in out OpenToken.Token.Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Integer (Token : in out Master_Token.Instance'Class);

   --  Define all our tokens
   --  ...terminals
   Times       : constant Master_Token.Handle  := Syntax (Multiply_ID).Token_Handle;
   Left_Paren  : constant Master_Token.Handle  := Syntax (Left_Paren_ID).Token_Handle;
   Right_Paren : constant Master_Token.Handle  := Syntax (Right_Paren_ID).Token_Handle;
   Plus        : constant Master_Token.Handle  := Syntax (Plus_Sign_ID).Token_Handle;
   Int         : constant Integer_Token.Handle := Integer_Token.Handle (Syntax (Integer_ID).Token_Handle);
   EOF         : constant Master_Token.Handle  := Syntax (EOF_ID).Token_Handle;

   --  ...and nonterminals. Since we have lots of recursion, we do
   --  them all in the body.
   L : constant OpenToken.Token.Sequence.Handle  := new OpenToken.Token.Sequence.Instance;
   E : constant OpenToken.Token.Selection.Handle := new OpenToken.Token.Selection.Instance;
   T : constant OpenToken.Token.Selection.Handle := new OpenToken.Token.Selection.Instance;
   F : constant OpenToken.Token.Selection.Handle := new OpenToken.Token.Selection.Instance;

end ASU_Example_5_10_RD_Commute;
