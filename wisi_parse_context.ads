--  Abstract :
--
--  Parse context for one source file.
--
--  Copyright (C) 2020, 2021 Free Software Foundation All Rights Reserved.
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
      Descriptor              : WisiToken.Descriptor_Access_Constant;
      Lexer                   : WisiToken.Lexer.Handle;
      Table                   : WisiToken.Parse.LR.Parse_Table_Ptr;
      Partial_Parse_Active    : access Boolean;
      Partial_Parse_Byte_Goal : access WisiToken.Buffer_Pos;
      Fixes                   : WisiToken.Parse.LR.Parser.Language_Fixes_Access;
      Matching_Begin_Tokens   : WisiToken.Parse.LR.Parser.Language_Matching_Begin_Tokens_Access;
      String_ID_Set           : WisiToken.Parse.LR.Parser.Language_String_ID_Set_Access;
      Parse_Data_Template     : Wisi.Parse_Data_Access;
   end record;

   type Parse_Context is limited record
      File_Name   : Ada.Strings.Unbounded.Unbounded_String;
      Text_Buffer : Ada.Strings.Unbounded.String_Access;
      --  Text_Buffer may hold all or part of the actual Emacs buffer
      --  content. If partial, the lexer holds the mapping from Text_Buffer
      --  index to Emacs buffer position.

      Text_Buffer_Byte_Last : Integer;
      Text_Buffer_Char_Last : Integer;
      --  For Incremental parse; after editing, there may be empty space at
      --  the end of Text_Buffer.

      Parser : WisiToken.Parse.LR.Parser.Parser;

      Root_Save_Edited_Name : Ada.Strings.Unbounded.Unbounded_String;
      --  If not "", save source text after the edit in a parse_incremental command,
      --  to <root_save_edited_name_nnn>, where 'nnn' is a three-digit number that
      --  increments.

      Save_Edited_Count : Integer := 0;

   end record;
   type Parse_Context_Access is access all Parse_Context;

   function Find_Create
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language;
      Trace     : in WisiToken.Trace_Access)
     return Parse_Context_Access;
   --  If a context for File_Name exists, return it if Language matches.
   --  Raise WisiToken.User_Error if language does not match. Otherwise,
   --  create a context, return it.

   function Find
     (File_Name : in String;
      Language  : in Wisi_Parse_Context.Language)
     return Parse_Context_Access;
   --  If a context for File_Name exists, return it if Language matches.
   --  Raise WisiToken.User_Error if language does not match. Otherwise
   --  return null.

   procedure Clear;
   --  Delete all contexts.

end Wisi_Parse_Context;
