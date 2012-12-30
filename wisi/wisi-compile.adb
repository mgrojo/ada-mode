--  Abstract :
--
--  Main program to output elisp source for a compiled Wisent LALR
--  grammar, given a Wisent source file.
--
--  Copyright (C) 2012 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with OpenToken.Production.Parser.LALR;
with OpenToken.Text_Feeder.Text_IO;
with Wisi.Grammar; use Wisi.Grammar;
procedure Wisi.Compile
is
   procedure Put_Usage
   is
      use Ada.Text_IO;
   begin
      Put_Line ("wisi-compile [-t] {wisent grammar file} {output file}");
      Put_Line ("  -t : output trace of parser generation, execution");
      Put_Line ("  compile 'wisent grammar file', into 'output file'");
   end Put_Usage;

   package Parser is new Production.Parser (Production_List, Tokenizer);
   package LALR_Parser is new Parser.LALR;

   Output_File     : Ada.Text_IO.File_Type;
   Input_File_Name : Ada.Strings.Unbounded.Unbounded_String;
   Input_File      : constant access Ada.Text_IO.File_Type := new Ada.Text_IO.File_Type;

   File_Feeder : OpenToken.Text_Feeder.Text_Feeder_Ptr;
   Analyzer    : constant Tokenizer.Instance := Tokenizer.Initialize (Syntax, File_Feeder);
   File_Parser : LALR_Parser.Instance;

   procedure Use_Input_File (File_Name : in String)
   is
      use Ada.Text_IO;
   begin
      Input_File_Name := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
      Open (Input_File.all, In_File, File_Name);
      File_Feeder := OpenToken.Text_Feeder.Text_IO.Create (File_Access (Input_File));
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "input file '" & File_Name & "' could not be opened.";
   end Use_Input_File;

   procedure Use_Output_File (File_Name : in String)
   is
      use Ada.Text_IO;
   begin
      Create (Output_File, Out_File, File_Name);
   exception
   when Name_Error | Use_Error =>
      raise Name_Error with "output file '" & File_Name & "' could not be created.";
   end Use_Output_File;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 2 =>
         Use_Input_File (Argument (1));
         Use_Output_File (Argument (2));

      when 3 =>
         if Argument (1) = "-t" then
            OpenToken.Trace_Parse := True;
            Use_Input_File (Argument (2));
            Use_Output_File (Argument (3));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   end;

   File_Parser := LALR_Parser.Generate (Grammar.Grammar, Analyzer, OpenToken.Trace_Parse);

   if OpenToken.Trace_Parse then
      LALR_Parser.Print_Table (File_Parser);
   end if;

   File_Parser.Set_Text_Feeder (File_Feeder);

   if OpenToken.Trace_Parse then
      Ada.Text_IO.Put_Line ("parsing:");
   end if;

   declare
      use Ada.Exceptions;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
   begin
      File_Parser.Parse;
   exception
   when E : OpenToken.Syntax_Error =>
      Put_Line (To_String (Input_File_Name & ":" & Exception_Message (E)));
   end;

   Ada.Text_IO.Close (Input_File.all);
   Ada.Text_IO.Close (Output_File);
end Wisi.Compile;
