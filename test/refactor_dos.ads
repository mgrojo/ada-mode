--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe
--   --generate PACKRAT_GEN Ada re2c PROCESS ../test/bnf/body_instantiation_conflict.wy
--

--  Copyright (C) 2013, 2017 - 2020 Stephen Leake.  All Rights Reserved.

--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version.
--
--  This software is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

with WisiToken.Lexer;
with WisiToken.Parse.LR;
with WisiToken.Syntax_Trees;
package Body_Instantiation_Conflict_Packrat_Gen_Main is

   function Create_Parser
     (Descriptor : in WisiToken.Descriptor_Access;
      Trace      : in WisiToken.Trace_Access;
      User_Data  : in WisiToken.Syntax_Trees.User_Data_Access)
     return WisiToken.Parse.Base_Parser'Class;

end Body_Instantiation_Conflict_Packrat_Gen_Main;
