--  Abstract :
--
--  A generalized LR parser, with no error recovery, no semantic
--  checks, no incremental parse.
--
--  This allows wisi-generate (which uses the generated wisi_grammar)
--  to not depend on wisitoken-lr-mckenzie_recover, so editing that
--  does not cause everything to be regenerated/compiled.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2021 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Lexer;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Syntax_Trees;
package WisiToken.Parse.LR.Parser_No_Recover is

   type Parser is new WisiToken.Parse.Base_Parser with record
      Table   : Parse_Table_Ptr;
      Parsers : aliased Parser_Lists.List;
   end record;

   overriding procedure Finalize (Object : in out LR.Parser_No_Recover.Parser);
   --  Deep free Object.Table.

   procedure New_Parser
     (Parser    :    out LR.Parser_No_Recover.Parser;
      Trace     : in     WisiToken.Trace_Access;
      Lexer     : in     WisiToken.Lexer.Handle;
      Table     : in     Parse_Table_Ptr;
      User_Data : in     Syntax_Trees.User_Data_Access);

   overriding procedure Parse
     (Shared_Parser : in out LR.Parser_No_Recover.Parser;
      Log_File      : in     Ada.Text_IO.File_Type;
      Edits         : in     KMN_Lists.List := KMN_Lists.Empty_List);
   --  Attempt a parse. Calls Parser.Lexer.Reset, runs lexer to end of
   --  input setting Shared_Parser.Terminals, then parses tokens.
   --
   --  If a parse error is encountered, raises Syntax_Error.
   --  Parser.Lexer_Errors and Parsers(*).Errors contain information
   --  about the errors.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.
   --
   --  Raises SAL.Programmer_Error if Edits is not empty. Log_File is
   --  ignored.

   overriding procedure Execute_Actions
     (Parser              : in out LR.Parser_No_Recover.Parser;
      Action_Region_Bytes : in     WisiToken.Buffer_Region := WisiToken.Null_Buffer_Region);
   --  Action_Region_Bytes is ignored (all nodes always processed).

end WisiToken.Parse.LR.Parser_No_Recover;
