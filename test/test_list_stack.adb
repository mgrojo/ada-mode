-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2010, 2012 Stephen Leake
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
-----------------------------------------------------------------------------

pragma License (GPL);

with AUnit.Check;
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
package body Test_List_Stack is

   --  Just enough machinery to show that the operand stack in List is
   --  required, and works.
   --
   --  A grammar that requires a stack is:
   --
   --  L -> E  EOF      print (L.val)
   --  E -> T {+ T}     + action: E.val := E.val + T.val
   --  T -> F {* F}     * action: T.val := T.val * F.val
   --  F -> ( E )       F.val := E.val
   --  F -> integer     F.val := integer
   --
   --  E is initialized to 0, F to 1.
   --
   --  Consider the input (10 + 5) * (2 + 3). If there is no operand
   --  stack, parsing (2 +3) throws away the results of parsing (10 +
   --  5), and the wrong answer results.
   --
   --  See Examples/ASU_Example_5_10 for a version of this that has
   --  useful trace output.

   type Token_IDs is (Integer_ID, Left_Paren_ID, Right_Paren_ID, Plus_ID, Times_ID, EOF_ID, Whitespace_ID);

   package Master_Token is new OpenToken.Token.Enumerated (Token_IDs);
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
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   package Integer_Selection is new OpenToken.Token.Selection_Mixin
     (Parent_Token    => Integer_Token.Instance,
      Component_Token => Integer_Token.Instance);
   procedure Build_Selection
     (Match : in out Integer_Selection.Instance;
      From  : in     Integer_Token.Class)
   is begin
      Match.Value := From.Value;
   end Build_Selection;

   package Integer_Sequence is new OpenToken.Token.Sequence_Mixin (Integer_Token.Instance);
   procedure Build_Parens
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;
      Iterator : List_Iterator := First (Using); -- (
   begin
      Next_Token (Iterator); -- E
      Match.Value := Integer_Token.Handle (Token_Handle (Iterator)).Value;
   end Build_Parens;

   Expected_Value : Integer;
   procedure Build_Print
     (Match : in out Integer_Sequence.Instance;
      Using : in     OpenToken.Token.Linked_List.Instance)
   is
      use OpenToken.Token.Linked_List;
      Iterator : constant List_Iterator := First (Using); -- E
   begin
      Match.Value := Integer_Token.Handle (OpenToken.Token.Linked_List.Token_Handle (Iterator)).Value;
      AUnit.Check.Check ("expected result", Match.Value, Expected_Value);
   end Build_Print;

   package Operation_List is new OpenToken.Token.List_Mixin (Integer_Token.Instance, Integer_Token.Instance);

   procedure Init_Plus (Match : in out Operation_List.Instance)
   is begin
      Match.Value := 0;
   end Init_Plus;

   procedure Plus_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      Match.Value := Match.Value + Element.Value;
   end Plus_Element;

   procedure Init_Times (Match : in out Operation_List.Instance)
   is begin
      Match.Value := 1;
   end Init_Times;

   procedure Times_Element
     (Match   : in out Operation_List.Instance;
      Element : in     Integer_Token.Class)
   is begin
      Match.Value := Match.Value * Element.Value;
   end Times_Element;

   --  Terminal tokens
   Int         : constant Integer_Token.Handle := new Integer_Token.Class'(Integer_Token.Get (Integer_ID));
   Left_Paren  : constant Master_Token.Handle  := Syntax (Left_Paren_ID).Token_Handle;
   Right_Paren : constant Master_Token.Handle  := Syntax (Right_Paren_ID).Token_Handle;
   Plus        : constant Master_Token.Handle  := Syntax (Plus_ID).Token_Handle;
   Times       : constant Master_Token.Handle  := Syntax (Times_ID).Token_Handle;
   EOF         : constant Master_Token.Handle  := Syntax (EOF_ID).Token_Handle;

   use type Integer_Selection.Instance;
   use type Integer_Sequence.Instance;
   use Operation_List;

   --  Because of the recursion, we defer the full definition of E to the Set_Up_Case.
   E : constant Operation_List.Handle    := new Operation_List.Instance;
   F : constant Integer_Selection.Handle :=
     (Left_Paren & E & Right_Paren + Build_Parens'Access or Int) + Build_Selection'Access;
   T : constant Operation_List.Handle    := F ** Times * Times_Element'Access + Init_Times'Access;
   L : constant Integer_Sequence.Handle  := E & EOF + Build_Print'Access;

   ----------
   --  Test procedures

   procedure List_Stack (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Check;
   begin
      OpenToken.Text_Feeder.String.Set (Feeder, "(10 + 5) * (2 + 3)");
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer);

      Expected_Value := 75;
      Integer_Sequence.Parse (L, Analyzer);
   end List_Stack;

   ----------
   --  Public subprograms

   overriding function Name (T : in Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_List_Stack");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, List_Stack'Access, "List_Stack");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      E.all := Operation_List.Get
        (Element     => Test_List_Stack.T,
         Separator   => Plus,
         Initialize  => Init_Plus'Access,
         Add_Element => Plus_Element'Access);

   end Set_Up_Case;

end Test_List_Stack;
