-------------------------------------------------------------------------------
--
--  Copyright (C) 2009, 2010, 2012, 2013, 2014 Stephen Leake
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

with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.AUnit;
with OpenToken.Token.Linked_List;
with OpenToken.Token.Sequence;
package body Test_Sequence_Actions is

   --  Just enough machinery to allow checking that the specified
   --  Build or Add_Element is called.

   type Token_ID is (T0, T1, T2, EOF, Whitespace);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID, Token_ID'First, Token_ID'Last, Token_ID'Image);
   package Tokenizer is new Master_Token.Analyzer;
   package Master_Token_AUnit is new Master_Token.AUnit;

   Syntax : constant Tokenizer.Syntax :=
     (T0         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T0")),
      T1         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T1")),
      T2         => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("T2")),
      EOF        => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace => Tokenizer.Get
        (OpenToken.Recognizer.Character_Set.Get
           (OpenToken.Recognizer.Character_Set.Standard_Whitespace)));

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : constant Tokenizer.Handle := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  Build and Add_Element actions

   use OpenToken.Token;
   use type Sequence.Instance;

   --  Terminal tokens
   T0_Token : constant Master_Token.Handle := Syntax (T0).Token_Handle;
   T1_Token : constant Master_Token.Handle := Syntax (T1).Token_Handle;
   T2_Token : constant Master_Token.Handle := Syntax (T2).Token_Handle;

   --  Nonterminal tokens
   procedure Build_Sequence
     (Token  : in out Sequence.Instance;
      Source : in     Linked_List.Instance)
   is
      pragma Unreferenced (Token);
      use Master_Token_AUnit;
   begin
      Check ("Build_Sequence", Source, (T0, T1, T2));
   end Build_Sequence;

   A_Sequence : constant Sequence.Handle := T0_Token & T1_Token & T2_Token + Build_Sequence'Access;

   ----------
   --  Test procedures

   procedure Sequence_Action (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

   begin
      OpenToken.Text_Feeder.String.Set (Feeder, "T0 T1 T2");
      Analyzer.Reset;
      Analyzer.Find_Next;

      Sequence.Parse (A_Sequence, Analyzer);

   end Sequence_Action;

   ----------
   --  Public subprograms

   overriding function Name (T : in Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_Sequence_Actions");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Sequence_Action'Access, "Sequence_Action");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      OpenToken.Trace_Parse := T.Debug;
   end Set_Up_Case;

end Test_Sequence_Actions;
