--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Checks;
with Ada.Text_IO;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Text_IO_Trace;
with WisiToken_Grammar_Runtime;
with Wisitoken_Grammar_Actions;
with Wisitoken_Grammar_Main;
package body WisiToken.Generate.Packrat.Test is

   ----------
   --  Test procedures

   procedure Ada_Lite_Involved (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Grammar_File_Name  : constant String := "../test/bnf/ada_lite.wy";
      Grammar_Parse_Data : aliased WisiToken_Grammar_Runtime.User_Data_Type;
      Grammar_Parser     : WisiToken.Parse.LR.Parser_No_Recover.Parser;
      Trace              : aliased WisiToken.Text_IO_Trace.Trace (Wisitoken_Grammar_Actions.Descriptor'Access);
   begin
      --  Test that all indirect recursion is identified

      Wisitoken_Grammar_Main.Create_Parser
        (Parser    => Grammar_Parser,
         Trace     => Trace'Unchecked_Access,
         User_Data => Grammar_Parse_Data'Unchecked_Access);

      Grammar_Parser.Lexer.Reset_With_File (Grammar_File_Name);
      Grammar_Parser.Parse;
      Grammar_Parse_Data.User_Parser := WisiToken.BNF.Packrat_Gen;
      Grammar_Parse_Data.Phase := WisiToken_Grammar_Runtime.Meta;
      Grammar_Parser.Execute_Actions;
      Grammar_Parse_Data.Phase := WisiToken_Grammar_Runtime.Other;
      Grammar_Parser.Execute_Actions;

      declare
         use AUnit.Checks;

         Generate_Data : constant WisiToken.BNF.Generate_Utils.Generate_Data := WisiToken.BNF.Generate_Utils.Initialize
           (Grammar_Parse_Data);

         Packrat_Data : constant WisiToken.Generate.Packrat.Data := WisiToken.Generate.Packrat.Initialize
           (Grammar_File_Name, Generate_Data.Grammar,
            Generate_Data.Source_Line_Map, Generate_Data.Descriptor.First_Terminal);

         --  From ada_lite_lalr.parse_table
         Name_ID               : constant Token_ID := 103;
         Selected_Component_ID : constant Token_ID := 124;

         Expected : Token_Array_Token_Set :=
           (Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index =>
              (Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index => False));
      begin
         for I in Packrat_Data.Direct_Left_Recursive'Range loop
            Expected (I, I) := Packrat_Data.Direct_Left_Recursive (I);
         end loop;

         --  These are the only indirect recursions
         Expected (Name_ID, Name_ID)                             := True;
         Expected (Name_ID, Selected_Component_ID)               := True;
         Expected (Selected_Component_ID, Name_ID)               := True;
         Expected (Selected_Component_ID, Selected_Component_ID) := True;

         if Trace_Action > Outline then
            Ada.Text_IO.Put_Line ("Direct_Left_Recursive:");
            Ada.Text_IO.Put_Line (Image (Packrat_Data.Direct_Left_Recursive, Generate_Data.Descriptor.all));
            Ada.Text_IO.New_Line (2);

            Ada.Text_IO.Put_Line ("First:");
            Put (Generate_Data.Descriptor.all, Packrat_Data.First);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_Line ("Involved:");
            Put (Generate_Data.Descriptor.all, Packrat_Data.Involved);
            Ada.Text_IO.New_Line;
         end if;

         for I in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
            for J in Generate_Data.Grammar.First_Index .. Generate_Data.Grammar.Last_Index loop
               Check (Token_ID'Image (I) & ", " & Token_ID'Image (J), Packrat_Data.Involved (I, J), Expected (I, J));
            end loop;
         end loop;
      end;
   end Ada_Lite_Involved;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Ada_Lite_Involved'Access, "Ada_Lite_Involved");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("wisitoken-generate-packrat-test.adb");
   end Name;

end WisiToken.Generate.Packrat.Test;
