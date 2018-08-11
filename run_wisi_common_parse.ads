--  Abstract :
--
--  Common utilities for Gen_Run_Wisi_*_Parse
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Strings.Unbounded;
with Wisi;
with WisiToken.LR.Parser;
package Run_Wisi_Common_Parse is

   procedure Usage (Parser : in out WisiToken.LR.Parser.Parser);

   type Command_Line_Params is record
      Post_Parse_Action : Wisi.Post_Parse_Action_Type;
      Source_File_Name  : Ada.Strings.Unbounded.Unbounded_String;
      Repeat_Count      : Integer := 1;
      Lang_Params       : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Get_CL_Params (Parser : in out WisiToken.LR.Parser.Parser) return Command_Line_Params;

end Run_Wisi_Common_Parse;
