--  Abstract :
--
--  Parse context for one source file.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
with Wisi;
with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
package Wisi_Parse_Context is

   type Language is record
      Descriptor            : WisiToken.Descriptor_Access_Constant;
      Lexer                 : WisiToken.Lexer.Handle;
      Table                 : WisiToken.Parse.LR.Parse_Table_Ptr;
      Fixes                 : WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Matching_Begin_Tokens : WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      String_ID_Set         : WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Parse_Data_Template   : Wisi.Parse_Data_Access;
   end record;

   type Parse_Context is limited record
      Text_Buffer : Ada.Strings.Unbounded.String_Access;
      Parser      : WisiToken.Parse.LR.Parser.Parser;
   end record;
   type Parse_Context_Access is access all Parse_Context;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access;
   --  If a context for File_Name exists, return it if Language matches.
   --  Raise SAL.User_Error if language does not match. Otherwise, create
   --  a context, return it.

   procedure Clear;
   --  Delete all contexts.

end Wisi_Parse_Context;
