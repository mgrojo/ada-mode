--  Abstract :
--
--  Run Name_Token_Test
--
--  Copyright (C) 2002, 2003, 2009, 2013, 2014 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
procedure Name_Token_Test.Run
is
   Trace : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   procedure Parse_Command (Parser : in out LALR_Parser.Instance; Command : in String)
   is
      use LALR_Parser;
   begin
      Put_Line ("'" & Command & "'");
      OpenToken.Text_Feeder.String.Set (String_Feeder, Command);

      Set_Text_Feeder (Parser, String_Feeder'Access);

      --  Read and parse statements from the string until end of string
      loop
         exit when End_Of_Text (Parser);

         Parse (Parser);
      end loop;
      Put_Line ("success");
      New_Line;
   exception
   when E : others =>
      Discard_Buffered_Text (Parser);
      Put_Line ("error: " & Ada.Exceptions.Exception_Name (E) & " : " & Ada.Exceptions.Exception_Message (E));
      New_Line;
   end Parse_Command;

begin
   --  The test is that the parse trace matches the known good trace.
   OpenToken.Trace_Parse := True;

   Put_Line ("Simple Parser");
   declare
      Parser : LALR_Parser.Instance := LALR_Parser.Initialize
        (Tokenizer.Initialize (Syntax),
         LALR_Generator.Generate
           (Simple_Grammar,
            Put_Parse_Table      => Trace,
            Trace                => Trace,
            Ignore_Unused_Tokens => True));
   begin
      Parse_Command (Parser, "Module (Index)");
      Parse_Command (Parser, "Module.Component");
   end;

   New_Line;
   Put_Line ("Medium Parser");
   declare
      Parser : LALR_Parser.Instance := LALR_Parser.Initialize
        (Tokenizer.Initialize (Syntax),
         LALR_Generator.Generate
           (Medium_Grammar,
            Put_Parse_Table      => Trace,
            Trace                => Trace,
            Ignore_Unused_Tokens => True));
   begin
      Parse_Command (Parser, "Module.Symbol (Index)");
      Parse_Command (Parser, "Module.Symbol.Component");
   end;

   New_Line;
   Put_Line ("Full Parser");
   declare
      Parser : LALR_Parser.Instance := LALR_Parser.Initialize
        (Tokenizer.Initialize (Syntax),
         LALR_Generator.Generate
           (Full_Grammar,
            Put_Parse_Table      => Trace,
            Trace                => Trace,
            Ignore_Unused_Tokens => False));
   begin
      Parse_Command (Parser, "Module.Symbol");
      Parse_Command (Parser, "Module.Symbol (Index)");
      Parse_Command (Parser, "Module.Symbol.Component");
      Parse_Command (Parser, "Module.Symbol (Index).Component");
      Parse_Command (Parser, "Module.Symbol.Component (Index)");
   end;

end Name_Token_Test.Run;
