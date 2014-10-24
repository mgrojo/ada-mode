-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2013, 2014 Stephe Leake
--  Copyright (C) 2000 Ted Dennison
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

---------------------------------------------------------------------------
--  This example is a recursive-decent implementation of Example 5.10,
--  section 5.3, page 295 from [1] "Compilers Principles, Techniques,
--  and Tools" by Aho, Sethi, and Ullman (aka: "The Dragon Book"). It
--  demonstrates handling of synthesized attributes. See README.text
--  for more discussion.
---------------------------------------------------------------------------

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Integer;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.Linked_List;
with OpenToken.Token.List_Mixin;
with OpenToken.Token.Selection_Mixin;
with OpenToken.Token.Sequence_Mixin;
package ASU_Example_5_10_RD_List is

   --  The complete list of tokens. No non-terminals in recursive descent.
   type Token_IDs is
     (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_ID, Times_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated
     (Token_IDs, Token_IDs'First, Token_IDs'Last, Token_IDs'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Integer_Token is new Master_Token.Integer;

   Syntax : constant Tokenizer.Syntax :=
     (Integer_ID        => Tokenizer.Get
        (OpenToken.Recognizer.Integer.Get
           (Allow_Signs => False),
         Integer_Token.Get (Integer_ID)),
      Left_Paren_ID     => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("(")),
      Right_Paren_ID    => Tokenizer.Get (OpenToken.Recognizer.Separator.Get (")")),
      Plus_ID           => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("+")),
      Times_ID          => Tokenizer.Get (OpenToken.Recognizer.Separator.Get ("*")),
      EOF_ID            => Tokenizer.Get (OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID     => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   --------------------------------------------------------------------------
   --  Our custom token types. The grammar is:
   --
   --  L -> E  EOF      print (L.val)
   --  E -> T {+ T}     element action: E.val := E.val + T.val
   --  T -> F {* F}     element action: T.val := T.val * F.val
   --  F -> ( E )       F.val := E.val
   --  F -> integer
   --
   --  E is initialized to 0, F to 1.
   --
   --  The List token implements {}. It keeps a local copy of the
   --  result token on the CPU stack; that implements the operand
   --  stack.

   --  Create a custom selection token which has integers for
   --  components and returns an integer with the value of the
   --  selected component from a parse; used for F -> ( E ) | digit.
   package Integer_Selection is new OpenToken.Token.Selection_Mixin
     (Parent_Token    => Integer_Token.Instance,
      Component_Token => Integer_Token.Instance);
   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class);

   --  A token type for a sequence of tokens; used for ( E ), E EOF
   package Integer_Sequence is new OpenToken.Token.Sequence_Mixin (Integer_Token.Instance);
   procedure Build_Parens
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   procedure Build_Print
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);

   --  Token type for repeating lists; {+ T}, {* F}
   package Operation_List is new OpenToken.Token.List_Mixin (Integer_Token.Instance, Integer_Token.Instance);

   procedure Init_Plus (Match : in out Operation_List.Instance);
   procedure Plus_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class);
   procedure Init_Times (Match : in out Operation_List.Instance);
   procedure Times_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class);

   --  Define all our tokens
   --  ...terminals
   Int         : constant Integer_Token.Handle := new Integer_Token.Class'(Integer_Token.Get (Integer_ID));
   Left_Paren  : constant Master_Token.Handle  := Syntax (Left_Paren_ID).Token_Handle;
   Right_Paren : constant Master_Token.Handle  := Syntax (Right_Paren_ID).Token_Handle;
   Plus        : constant Master_Token.Handle  := Syntax (Plus_ID).Token_Handle;
   Times       : constant Master_Token.Handle  := Syntax (Times_ID).Token_Handle;
   EOF         : constant Master_Token.Handle  := Syntax (EOF_ID).Token_Handle;

   --  ...and nonterminals.
   use type Integer_Selection.Instance;
   use type Integer_Sequence.Instance;
   use Operation_List;

   --  Because of the recursion, we defer the full definition of E to the body.
   --  We declare F1 to allow giving it a name for debug
   E : constant Operation_List.Handle    := new Operation_List.Instance;
   F1 : constant Integer_Sequence.Handle := Left_Paren & E & Right_Paren + Build_Parens'Access;
   F : constant Integer_Selection.Handle := (F1 or Int) + Build_Selection'Access;
   T : constant Operation_List.Handle    := F ** Times * Times_Element'Access + Init_Times'Access;
   L : constant Integer_Sequence.Handle  := E & EOF + Build_Print'Access;

end ASU_Example_5_10_RD_List;
