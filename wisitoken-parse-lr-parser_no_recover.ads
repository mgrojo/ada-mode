--  Abstract :
--
--  A generalized LR parser, with no error recovery, no semantic
--  checks, no incremental parse.
--
--  This allows wisi-generate (which uses the generated wisi_grammar)
--  to not depend on wisitoken-lr-mckenzie_recover, so editing that
--  does not cause everything to be regenerated/compiled.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013 - 2015, 2017 - 2020 Free Software Foundation, Inc.
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

      First_Parser_Label   : Integer;
      Terminate_Same_State : Boolean; -- IMPROVEME: delete, never set False
   end record;

   overriding procedure Finalize (Object : in out LR.Parser_No_Recover.Parser);
   --  Deep free Object.Table.

   procedure New_Parser
     (Parser               :    out          LR.Parser_No_Recover.Parser;
      Trace                : not null access WisiToken.Trace'Class;
      Lexer                : in              WisiToken.Lexer.Handle;
      Table                : in              Parse_Table_Ptr;
      User_Data            : in              Syntax_Trees.User_Data_Access;
      First_Parser_Label   : in              Integer            := 1);

   overriding procedure Parse (Shared_Parser : in out LR.Parser_No_Recover.Parser);
   --  Attempt a parse. Calls Parser.Lexer.Reset, runs lexer to end of
   --  input setting Shared_Parser.Terminals, then parses tokens.
   --
   --  If a parse error is encountered, raises Syntax_Error.
   --  Parser.Lexer_Errors and Parsers(*).Errors contain information
   --  about the errors.
   --
   --  For other errors, raises Parse_Error with an appropriate error
   --  message.

   overriding function Any_Errors (Parser : in LR.Parser_No_Recover.Parser) return Boolean;

   overriding procedure Put_Errors (Parser : in LR.Parser_No_Recover.Parser);
   --  Put user-friendly error messages from the parse to
   --  Ada.Text_IO.Current_Error.

   overriding procedure Execute_Actions
     (Parser          : in out LR.Parser_No_Recover.Parser;
      Image_Augmented : in     Syntax_Trees.Image_Augmented := null);
   --  Execute the grammar actions in Parser.

end WisiToken.Parse.LR.Parser_No_Recover;
