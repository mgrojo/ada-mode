--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2021 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Text_IO;
with AUnit.Checks; use AUnit.Checks;
with WisiToken.AUnit; use WisiToken.AUnit;
package body Test_Skip_To_Aux is

   use all type WisiToken.Base_Buffer_Pos;

   Decl_Count : Integer := 0;

   procedure Test_Declaration_0 (Nonterm : in WisiToken.Syntax_Trees.Valid_Node_Access)
   is begin
      if Enable then
         declare
            Chars : constant WisiToken.Buffer_Region := Parser.Tree.Char_Region
              (Parser.Tree.Last_Terminal (Nonterm));
            Bytes : constant WisiToken.Buffer_Region := Parser.Tree.Byte_Region (Nonterm);
            Lines : constant WisiToken.Line_Region := Parser.Tree.Line_Region (Nonterm, Trailing_Non_Grammar => False);
         begin
            if WisiToken.Trace_Parse > WisiToken.Outline then
               Ada.Text_IO.Put_Line ("Test_Declaration_0");
            end if;

            --  File has DOS line endings and non-ASCII chars.
            --
            --  Char_Region from wisi-show-region in .input file (with point _before_ last char)
            --
            --  Byte_Region from hexl-mode in .input file, +1 to match Emacs origin.

            Decl_Count := Decl_Count + 1;
            case Decl_Count is
            when 1 =>
               Check ("declaration_0 RANGE char region", Chars, (39, 43));
               Check ("declaration_0 RANGE byte region", Bytes, (16#1E# + 1, 16#2F# + 1));
               Check ("declaration_0 RANGE line region", Lines, (6, 6));

            when 2 =>
               Check ("declaration_0 X1_Non char region", Chars, (59, 62));
               Check ("declaration_0 X1_Non byte region", Bytes, (16#32# + 1, 70));

            when 3 =>
               Check ("declaration_0 X2_Non char region", Chars, (109, 112));
               Check ("declaration_0 X2_Non byte region", Bytes, (16#67# + 1, 16#7A# + 1));
            when 4 =>
               Check ("declaration_0 X3_Non char region", Chars, (154, 157));
               Check ("declaration_0 X3_Non byte region", Bytes, (16#97# + 1, 16#A9# + 1));

            when others =>
               raise WisiToken.Fatal_Error;
            end case;
         end;
      end if;
   end Test_Declaration_0;

   procedure Test_Compilation_Unit_0 (Nonterm : in WisiToken.Syntax_Trees.Valid_Node_Access)
   is begin
      if Enable then
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("Test_Compilation_Unit_0");
         end if;

         --  See comment in Test_Declaration_0 for source of expected values.

         Check ("compilation_unit_0 PREAMBLE char region",
                Parser.Tree.Char_Region (Parser.Tree.First_Shared_Terminal (Nonterm)),
                (1, 5));
         declare
            Bytes : constant WisiToken.Buffer_Region := Parser.Tree.Byte_Region (Nonterm);
         begin
            Check ("compilation_unit_0 PREAMBLE byte region", Bytes, (0 + 1, 5 + 1));
            Check ("compilation_unit_0 PREAMBLE text",
                   Parser.Tree.Lexer.Buffer_Text (Bytes),
                   "%{" & ASCII.CR & ASCII.LF & "%}");
         end;

         Check ("compilation_unit_0 PREAMBLE line region",
                Parser.Tree.Line_Region (Nonterm, Trailing_Non_Grammar => False),
                (1, 2));
      end if;
   end Test_Compilation_Unit_0;

end Test_Skip_To_Aux;
