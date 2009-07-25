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
with OpenToken.Token.Enumerated.Integer_Literal;
with OpenToken.Token.Linked_List;
with OpenToken.Token.Selection;
with OpenToken.Token.Sequence;
package ASU_Example_5_10_RD_No_Mixin is

   --  The complete list of tokens. No non-terminals in recursive descent.
   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_Sign_ID,
                      Multiply_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
   package Tokenizer is new Master_Token.Analyzer (Whitespace_ID);
   package Integer_Literal is new Master_Token.Integer_Literal;

   Syntax : constant Tokenizer.Syntax :=
     (Multiply_ID    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("*")),
      Left_Paren_ID  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("(")),
      Right_Paren_ID => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get (")")),
      Plus_Sign_ID   => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("+")),
      Integer_ID     => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Integer.Get
                                       (Allow_Signs => False),
                                       New_Token  => Integer_Literal.Get (Integer_ID)),
      EOF_ID         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace_ID  => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
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

   --  A base token type for integer-valued tokens
   type Integer_Token is abstract new OpenToken.Token.Instance with record
      Value : Integer;
   end record;
   type Integer_Token_Handle is access all Integer_Token'Class;

   --  Create a custom selection token which has integers for
   --  components and returns an integer with the value of the
   --  selected component from a parse.
   --
   --  Compare this to ASU_Example_5_10_RD. Here we just derive from
   --  Selection, rather than instantiating Selection_Mixin. That
   --  means we do more type checking at runtime (FIXME: or not? look
   --  at the Build bodies), but the declaration of the grammar is
   --  simpler.
   use OpenToken.Token;
   type Integer_Selection_Instance is new Selection.Instance with record
      Value : Integer;
      Name  : access String;
   end record;
   type Integer_Selection_Handle is access all Integer_Selection_Instance'Class;
   overriding function "or"
     (Left  : access OpenToken.Token.Class; Right : access OpenToken.Token.Class) return Integer_Selection_Instance;
   overriding function "or"
     (Left  : access OpenToken.Token.Class; Right : in Integer_Selection_Instance) return Integer_Selection_Instance;
   overriding function "or"
     (Left  : in Integer_Selection_Instance; Right : access OpenToken.Token.Class) return Integer_Selection_Instance;
   overriding function "or"
     (Left  : in Integer_Selection_Instance; Right : in Integer_Selection_Instance) return Integer_Selection_Instance;
   function New_Integer_Selection_Instance
     (Old_Instance : in Integer_Selection_Instance) return Integer_Selection_Handle;
   overriding procedure Build
     (Match : in out Integer_Selection_Instance;
      From  : in     OpenToken.Token.Instance'Class);
   overriding function Name (Item : in Integer_Selection_Instance) return String;

   --  Similarly for Sequence.
   --
   --  The grammar builds only three expression sequences:
   --
   --  E + T
   --  T * F
   --  ( E )
   --
   --  This returns in Match.Value the integer result of applying the
   --  operator, or the value between the parens.
   type Expression_Instance is new Sequence.Instance with record
      Value : Integer;
      Name  : access String;
   end record;
   type Expression_Handle is access all Expression_Instance'Class;
   overriding function "&"
     (Left  : access OpenToken.Token.Class; Right : access OpenToken.Token.Class) return Expression_Instance;
   overriding function "&"
     (Left  : access OpenToken.Token.Class; Right : in Expression_Instance) return Expression_Instance;
   overriding function "&"
     (Left  : in Expression_Instance; Right : access OpenToken.Token.Class) return Expression_Instance;
   overriding function "&"
     (Left  : in Expression_Instance; Right : in Expression_Instance) return Expression_Instance;
   function New_Expression_Instance (Name : in String; Item : in Expression_Instance) return Expression_Handle;
   overriding procedure Build
     (Match : in out Expression_Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);
   overriding function Name (Item : in Expression_Instance) return String;

   --  This type prints the value of the first element in the sequence.
   type Result_Instance is new Sequence.Instance with null record;
   type Result_Handle is access all Result_Instance'Class;
   overriding function "&"
     (Left  : access OpenToken.Token.Class; Right : access OpenToken.Token.Class) return Result_Instance;
   overriding function "&"
     (Left  : access OpenToken.Token.Class; Right : in Result_Instance) return Result_Instance;
   overriding function "&"
     (Left  : in Result_Instance; Right : access OpenToken.Token.Class) return Result_Instance;
   overriding function "&"
     (Left  : in Result_Instance; Right : in Result_Instance) return Result_Instance;
   function New_Result_Instance (Item : in Result_Instance) return Result_Handle;
   overriding procedure Build
     (Match : in out Result_Instance;
      Using : in     OpenToken.Token.Linked_List.Instance);

   --  Define all our tokens
   --  ...terminals
   Times       : constant Master_Token.Handle := Syntax (Multiply_ID).Token_Handle;
   Left_Paren  : constant Master_Token.Handle := Syntax (Left_Paren_ID).Token_Handle;
   Right_Paren : constant Master_Token.Handle := Syntax (Right_Paren_ID).Token_Handle;
   Plus        : constant Master_Token.Handle := Syntax (Plus_Sign_ID).Token_Handle;
   Int_Literal : constant Master_Token.Handle := Syntax (Integer_ID).Token_Handle;
   EOF         : constant Master_Token.Handle := Syntax (EOF_ID).Token_Handle;

   --  ...and nonterminals. Since we have lots of recursion, we do
   --  them all in the body.
   L : constant access Result_Instance            := new Result_Instance;
   E : constant access Integer_Selection_Instance := new Integer_Selection_Instance;
   T : constant access Integer_Selection_Instance := new Integer_Selection_Instance;
   F : constant access Integer_Selection_Instance := new Integer_Selection_Instance;

end ASU_Example_5_10_RD_No_Mixin;
