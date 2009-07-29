-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephe Leake
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
--  (aka: "The Dragon Book"). It demonstrates handling of synthesized
--  attributes, without using the mixin packages.
-------------------------------------------------------------------------------

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.Linked_List;
with OpenToken.Token.Selection_Mixin;
with OpenToken.Token.Sequence_Mixin;
package ASU_Example_5_10_RD_No_Mixin is

   --  The complete list of tokens. No non-terminals in recursive descent.
   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_Sign_ID,
                      Multiply_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
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
      Whitespace_ID     => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
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
   --  infinite loop; see asu_example_5_10_rd_no_mixin-run_bad.adb.
   --
   --  The problem is that the first element of a production is the
   --  same as the result of the production.
   --
   --  So we rearrange the grammar to avoid this, taking advantage of
   --  the commutivity of + and *.
   --
   --  L -> E         print (L.val)
   --  E -> T + E     E.val := E1.val + T.val
   --  E -> T
   --  T -> F * T     T.val := T1.val * F.val
   --  T -> F
   --  F -> ( E )     F.val := E.val
   --  F -> digit
   --
   --  This grammar requires 2 lookaheads; it needs to see
   --  the + or * of the sequences for E and T.

   --  Create a custom selection token which has integers for
   --  components and returns an integer with the value of the
   --  selected component from a parse; used for F -> ( E ) | digit.
   package Integer_Selection is new OpenToken.Token.Selection_Mixin
     (Parent_Token    => Integer_Token.Instance,
      Component_Token => Integer_Token.Instance);
   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class);

   --  A token type for a sequence of tokens; used for:
   --  E EOF
   --  E + T
   --  T * F
   --  ( E )
   package Integer_Sequence is new OpenToken.Token.Sequence_Mixin (Integer_Token.Instance);
   procedure Build_Print
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Plus
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Multiply
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Parens
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);

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
   L : constant Integer_Sequence.Handle  := new Integer_Sequence.Instance;
   E : constant Integer_Selection.Handle := new Integer_Selection.Instance;
   T : constant Integer_Selection.Handle := new Integer_Selection.Instance;
   F : constant Integer_Selection.Handle := new Integer_Selection.Instance;

end ASU_Example_5_10_RD_No_Mixin;
