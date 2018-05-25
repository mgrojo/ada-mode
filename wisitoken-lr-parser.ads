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
package WisiToken.LR.Parser is

   Default_Max_Parallel : constant := 15;

   type Language_Fixes_Access is access function
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : access constant WisiToken.Lexer.Instance'Class;
      Parser_Label      : in     Natural;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
     return Non_Success_Status;
   --  Config encountered a parse table Error action, or failed a
   --  semantic check; attempt to provide a language-specific fix,
   --  enqueuing new configs on Local_Config_Heap.
   --
   --  For a failed semantic check, Config.Stack is in the pre-reduce
   --  state, Config.Error_Token gives the nonterm token,
   --  Config.Check_Token_Count the token count for the reduce. May be
   --  called with Nonterm.Virtual = True or Tree.Valid_Indices (stack
   --  top token_count items) false. Return Continue if ignoring the
   --  error is a viable solution, Abandon otherwise.
   --
   --  For an Error action, Config.Error_Token gives the terminal that
   --  caused the error. Return Abandon if a known good solution is
   --  enqueued, Continue otherwise.

   type Language_Constrain_Terminals_Access is access function
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Table        : in     Parse_Table;
      Config       : in     Configuration)
     return Token_ID_Set;
   --  Return a token ID set that constrains McKenzie explore.
   --
   --  For example, in some cases, the best strategy is to complete the
   --  current production as quickly as possible; only insert terminals
   --  that are in the minimal terminal sequence for each production.

   type Language_String_ID_Set_Access is access function
     (Descriptor        : in WisiToken.Descriptor;
      String_Literal_ID : in Token_ID)
     return Token_ID_Set;
   --  Return a Token_ID_Set containing String_Literal_ID and
   --  nonterminals that can contain String_Literal_ID as part of an
   --  expression.

   type Post_Recover_Access is access procedure;

   type Parser is new Ada.Finalization.Limited_Controlled with record
      Trace     : access WisiToken.Trace'Class;
      Lexer     : WisiToken.Lexer.Handle;
      Table     : Parse_Table_Ptr;
      User_Data : WisiToken.Syntax_Trees.User_Data_Access;

      Language_Fixes : Language_Fixes_Access;

      Language_Constrain_Terminals : Language_Constrain_Terminals_Access;

      Language_String_ID_Set : Language_String_ID_Set_Access;

      String_Quote_Checked : Line_Number_Type := Invalid_Line_Number;
      --  Max line checked for missing string quote.

      Post_Recover : Post_Recover_Access;
      --  Gather data for tests.

      Terminals : aliased Base_Token_Arrays.Vector;
      --  All terminal grammar tokens, in lexical order. Does not contain
      --  virtual tokens. Tokens past Parser.Current_Token are lookahead.

      Line_Begin_Token : aliased Line_Begin_Token_Vectors.Vector;
      --  Index into Terminals of first grammar token on line. Last entry is
      --  index of EOF.

      Shared_Tree : aliased Syntax_Trees.Base_Tree;
      --  Each parser (normal and recover) has its own branched syntax tree,
      --  all branched from this tree. Terminals are added to the tree when
      --  they become the current token.
      --
      --  It is never the case that terminals are added to this shared tree
      --  when there is more than one task active, so we don't need a
      --  protected tree.
      --
      --  See WisiToken.LR.Parser_Lists Parser_State for more discussion of
      --  Shared_Tree.

      Parsers : aliased Parser_Lists.List;

      Max_Parallel            : SAL.Base_Peek_Type;
      First_Parser_Label      : Integer;
      Terminate_Same_State    : Boolean;
      Enable_McKenzie_Recover : Boolean;
   end record;

   overriding procedure Finalize (Object : in out LR.Parser.Parser);
   --  Deep free Object.Table.

   procedure New_Parser
     (Parser                       :    out          LR.Parser.Parser;
      Trace                        : not null access WisiToken.Trace'Class;
      Lexer                        : in              WisiToken.Lexer.Handle;
      Table                        : in              Parse_Table_Ptr;
      Language_Fixes               : in              Language_Fixes_Access;
      Language_Constrain_Terminals : in              Language_Constrain_Terminals_Access;
      Language_String_ID_Set       : in              Language_String_ID_Set_Access;
      User_Data                    : in              WisiToken.Syntax_Trees.User_Data_Access;
      Max_Parallel                 : in              SAL.Base_Peek_Type := Default_Max_Parallel;
      First_Parser_Label           : in              Integer            := 1;
      Terminate_Same_State         : in              Boolean            := True);

   procedure Parse (Shared_Parser : in out LR.Parser.Parser);
   --  Attempt a parse. Calls Parser.Lexer.Reset, runs lexer to end of
   --  input setting Shared_Parser.Terminals, then parses tokens.
   --
   --  If an error is encountered, Parser.Lexer_Errors and
   --  Parsers(*).Errors contain information about the errors. If a
   --  recover strategy succeeds, no exception is raised. If recover does
   --  not succeed, raises Syntax_Error.
   --
   --  For errors where no recovery is possible, raises Parse_Error with
   --  an appropriate error message.

   procedure Execute_Actions (Parser : in out LR.Parser.Parser);
   --  Execute the grammar actions in Parser.

   function Any_Errors (Parser : in out LR.Parser.Parser) return Boolean;
   --  Return True if any errors where encountered, recovered or not.

   procedure Put_Errors (Parser : in out LR.Parser.Parser; File_Name : in String);
   --  Put user-friendly error messages from the parse to
   --  Ada.Text_IO.Current_Error.

end WisiToken.LR.Parser;
