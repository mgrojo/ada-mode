-------------------------------------------------------------------------------
--
--  Copyright (C) 2009 Stephen Leake
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

with AUnit.Test_Cases.Registration;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.AUnit;
with OpenToken.Token.Selection;
package body Test_Selection_Actions is

   --  Just enough machinery to allow checking that the specified
   --  Build is called.

   type Token_ID is (T0, T1, EOF, Whitespace);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID);
   package Tokenizer is new Master_Token.Analyzer;
   package Master_Token_AUnit is new Master_Token.AUnit;

   Syntax : constant Tokenizer.Syntax :=
     (T0         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T0")),
      T1         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T1")),
      EOF        => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                      (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  Build and Add_Element actions

   use OpenToken.Token;
   use type Selection.Instance;

   --  Terminal tokens
   T0_Token : constant Master_Token.Handle := Syntax (T0).Token_Handle;
   T1_Token : constant Master_Token.Handle := Syntax (T1).Token_Handle;

   --  Nonterminal tokens
   procedure Build_Selection
     (Token    : in out Selection.Instance;
      Selected : in     OpenToken.Token.Class)
   is
      pragma Unreferenced (Token);
      use Master_Token_AUnit;
   begin
      Check ("Build_Sequence", Master_Token.ID (Master_Token.Instance (Selected)), T0);
   end Build_Selection;

   A_Selection : constant Selection.Handle := (T0_Token or T1_Token) + Build_Selection'Access;

   ----------
   --  Test procedures

   procedure Selection_Action (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      OpenToken.Text_Feeder.String.Set (Feeder, "T0");
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer);

      Selection.Parse (A_Selection, Analyzer);

   end Selection_Action;

   ----------
   --  Public subprograms

   overriding function Name (T : in Test_Case) return Ada.Strings.Unbounded.String_Access
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Selection_Actions");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Selection_Action'Access, "Selection_Action");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      OpenToken.Token.Trace_Parse := T.Debug;
   end Set_Up_Case;

end Test_Selection_Actions;
