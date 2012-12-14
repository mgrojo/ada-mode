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
with OpenToken.Recognizer.Based_Integer;
with OpenToken.Recognizer.Character_Set;
with OpenToken.Recognizer.End_Of_File;
with OpenToken.Recognizer.Separator;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.Integer;
with OpenToken.Token.List_Mixin;
package body Test_List_Actions is

   --  Just enough machinery to allow checking that the specified
   --  Initialize, Add_Element, and Build are called.

   type Token_ID is (Int, Plus, EOF, Whitespace);

   package Master_Token is new OpenToken.Token.Enumerated (Token_ID);
   package Tokenizer is new Master_Token.Analyzer;
   package Integer_Token is new Master_Token.Integer;
   package Int_List is new OpenToken.Token.List_Mixin
     (Parent_Token    => Integer_Token.Instance,
      Component_Token => Integer_Token.Instance);

   Syntax : constant Tokenizer.Syntax :=
     (Int           => Tokenizer.Get
        (Recognizer => OpenToken.Recognizer.Based_Integer.Get,
         New_Token  => Integer_Token.Get (Int)),
      Plus          => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Separator.Get ("+")),
      EOF           => Tokenizer.Get (Recognizer => OpenToken.Recognizer.End_Of_File.Get),
      Whitespace    => Tokenizer.Get (Recognizer => OpenToken.Recognizer.Character_Set.Get
                                        (OpenToken.Recognizer.Character_Set.Standard_Whitespace))
     );

   Feeder   : aliased OpenToken.Text_Feeder.String.Instance;
   Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, Feeder'Access);

   --  Build and Add_Element actions

   use type Int_List.Instance;
   use type Int_List.List_Action;

   --  Terminal tokens
   Int_Token  : constant Integer_Token.Handle := Integer_Token.Handle (Syntax (Int).Token_Handle);
   Plus_Token : constant Master_Token.Handle  := Syntax (Plus).Token_Handle;

   --  Nonterminal tokens
   List_Count : Integer;

   Expected_List_Elements : constant array (10 .. 12) of Integer := (1, 2, 3);

   procedure Initialize_List (Token : in out Int_List.Instance)
   is begin
      Token.Value := 0;
      List_Count  := 10;
   end Initialize_List;

   procedure Add_Element
     (Token   : in out Int_List.Instance;
      Element : in     Integer_Token.Class)
   is
      use AUnit.Check;
      Label : constant String := "Add_Element (" & Integer'Image (List_Count);
   begin
      Check (Label & ").tag", Element'Tag, Integer_Token.Instance'Tag);

      Check
        (Label & ").value",
         Element.Value,
         Expected_List_Elements (List_Count));

      Token.Value := Token.Value + Element.Value;
      List_Count  := List_Count + 1;
   end Add_Element;

   procedure Build_List (Token : in out Int_List.Instance)
   is
      pragma Unreferenced (Token);
   begin
      List_Count := List_Count - 10;
   end Build_List;

   A_List : constant Int_List.Handle := Int_List."**" (Int_Token, Plus_Token) * Add_Element'Access +
     Initialize_List'Access - Build_List'Access;

   ----------
   --  Test procedures

   procedure List_Action (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Check;
   begin
      OpenToken.Text_Feeder.String.Set (Feeder, "1 + 2 + 3");
      Tokenizer.Reset (Analyzer);
      Tokenizer.Find_Next (Analyzer);

      Int_List.Parse (A_List, Analyzer);

      Check ("list value", A_List.Value, 6);
      Check ("list_count", List_Count, 3);
   end List_Action;

   ----------
   --  Public  subprograms

   overriding function Name (T : in Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("Test_List_Actions");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, List_Action'Access, "List_Action");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      OpenToken.Trace_Parse := T.Debug;
   end Set_Up_Case;

end Test_List_Actions;
