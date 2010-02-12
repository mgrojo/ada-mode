-------------------------------------------------------------------------------
--
-- Copyright (C) 2009 Stephen Leake
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
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  Test driver for the token sequence handling code.
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with OpenToken.Text_Feeder.String;
with OpenToken.Token.Sequence;
with OpenToken.Token;
procedure Token_Sequence_Test.Run is
begin

   ----------------------------------------------------------------------------
   --  Test Case 1
   --
   --  Inputs           : A valid sequence of tokens.
   --
   --  Expected Results : A Token.Sequence
   --  Purpose          : Verify that a valid sequence of tokens is properly parsed.
   Test_Case_1 :
   declare

      use OpenToken.Token.Sequence;

      Parse_String : constant String := "Do several things 200 times in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'Access);

      Sequence : aliased OpenToken.Token.Sequence.Class :=
        Do_Keyword & Several_Keyword & Things_Keyword &
        Int_Literal & Times_Keyword &
        In_Keyword & A_Keyword & Row_Keyword;

   begin

      Ada.Text_IO.Put ("Testing parsing of valid token sequence...");
      Ada.Text_IO.Flush;

      --  Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set (String_Feeder, Parse_String);

      --  Load up the first token
      Tokenizer.Find_Next (Analyzer);

      --  Perform the parse
      OpenToken.Token.Sequence.Parse (Sequence'Access, Analyzer);

      if Tokenizer.ID (Analyzer) = EOF then
         Ada.Text_IO.Put_Line ("passed");
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         Ada.Text_IO.Put_Line ("failed.");
         Ada.Text_IO.Put_Line
           ("There was an unexpected " &
              Token_IDs'Image (Tokenizer.ID (Analyzer)) &
              " left on the input stream.");
      end if;

   exception
   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed due to parse exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_1;

   ----------------------------------------------------------------------------
   --  Test Case 2
   --
   --  Inputs           : An invalid token sequence.
   --
   --  Expected Results : A parse exception.
   --  Purpose          : Verify that an invalid token sequence is correctly
   --                     diagnosed.
   --
   Test_Case_2 :
   declare
      use OpenToken.Token.Sequence;

      Parse_String : constant String := "Do several things in a row";

      Analyzer : Tokenizer.Instance := Tokenizer.Initialize (Syntax, String_Feeder'Access);

      Sequence : aliased OpenToken.Token.Sequence.Class :=
        Do_Keyword & Several_Keyword & Things_Keyword &
        Int_Literal & Times_Keyword & In_Keyword &
        A_Keyword & Row_Keyword;

   begin

      Ada.Text_IO.Put ("Testing parsing of invalid token sequence...");
      Ada.Text_IO.Flush;

      --  Put the parse string into the analyzer's text feeder.
      OpenToken.Text_Feeder.String.Set (String_Feeder, Parse_String);

      --  Load up the first token
      Tokenizer.Find_Next (Analyzer);

      --  Parse token sequence
      OpenToken.Token.Sequence.Parse (Sequence'Access, Analyzer);

      Ada.Text_IO.Put_Line ("failed.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   exception
   when OpenToken.Parse_Error =>
      Ada.Text_IO.Put_Line ("passed.");

   when Error : others =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Ada.Text_IO.Put_Line ("failed due to parse exception:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Error));
   end Test_Case_2;
end Token_Sequence_Test.Run;
