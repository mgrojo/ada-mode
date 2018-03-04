--  Abstract :
--
--  A generalized LR parser.
--
--  In a child package of Parser.LR partly for historical reasons,
--  partly to allow McKenzie_Recover to be in a sibling package.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017, 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

with Ada.Finalization;
with WisiToken.LR.Parser_Lists;
with WisiToken.Semantic_State;
package WisiToken.LR.Parser is

   Default_Max_Parallel : constant := 15;

   type Parser is new Ada.Finalization.Limited_Controlled with record
      Trace          : access WisiToken.Trace'Class;
      Lexer          : WisiToken.Lexer.Handle;
      Table          : Parse_Table_Ptr;
      Semantic_State : WisiToken.Semantic_State.Semantic_State;

      Terminals : aliased Protected_Base_Token_Arrays.Vector;
      --  All terminal grammar tokens, in lexical order. Does not contain
      --  virtual tokens. Tokens past Parser.Current_Token are lookahead.
      --
      --  We use Protected, for safe multi-task access in McKensie_Recover.

      Parsers : aliased Parser_Lists.List;
      --  Each parser (normal and recover) has its own syntax tree.
      --  Terminals are added to the tree when they become the current
      --  token.

      Max_Parallel            : SAL.Base_Peek_Type;
      First_Parser_Label      : Integer;
      Terminate_Same_State    : Boolean;
      Enable_McKenzie_Recover : Boolean;
   end record;

   overriding procedure Finalize (Object : in out LR.Parser.Parser);
   --  Deep free Object.Table.

   --  'Parse' is not declared here, so wisi-generate is independent of
   --  wisitoken-lr-parser and -lr-mckenzie_recover.

   procedure New_Parser
     (Parser               :    out          LR.Parser.Parser;
      Trace                : not null access WisiToken.Trace'Class;
      Lexer                : in              WisiToken.Lexer.Handle;
      Table                : in              Parse_Table_Ptr;
      Max_Parallel         : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      First_Parser_Label   : in              Integer            := 1;
      Terminate_Same_State : in              Boolean            := True);

   procedure Parse (Shared_Parser : in out LR.Parser.Parser);
   --  Attempt a parse. Does _not_ reset Parser.Lexer on each call, to
   --  allow continuing in the same input stream.
   --
   --  If an error is encountered but a recover strategy succeeds, no
   --  exception is raised. Semantic_State contains information about the
   --  errors.
   --
   --  If recover does not succeed, raises Syntax_Error. Semantic_State
   --  contains information about the failing error and any previous
   --  recovered errors.
   --
   --  For errors where no recovery is possible, raises Parse_Error with
   --  an appropriate error message. Semantic_State contains information
   --  about previous recovered errors.

   procedure Execute_Actions
     (Parser         : in out LR.Parser.Parser;
      User_Data      : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Compute_Indent : in     Boolean);
   --  Execute the grammar actions in Parser.

end WisiToken.LR.Parser;
