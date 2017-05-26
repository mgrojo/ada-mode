--  Abstract :
--
--  A generalized LR parser.
--
--  Copyright (C) 2002, 2003, 2009, 2010, 2013-2015, 2017 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
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

with Ada.Text_IO;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser_Lists;
generic
   First_Parser_Label : in Integer;

   with procedure Put_Trace (Item : in String) is Ada.Text_IO.Put;
   --  Accumulate Item in the trace buffer.

   with procedure Put_Trace_Line (Item : in String) is Ada.Text_IO.Put_Line;
   --  Accumulate Item in the trace buffer, output the trace buffer to
   --  the display.

   with package Parser_Lists is new FastToken.Parser.LR.Parser_Lists
     (First_Parser_Label, Put_Trace, Put_Trace_Line);

   with package Panic_Mode is new FastToken.Parser.LR.Panic_Mode
     (First_Parser_Label, Put_Trace, Put_Trace_Line, Parser_Lists);

   type Semantic_State_Access_Type is private;

   with procedure Push_Token
     (Token : in Token_Pkg.Grammar_ID;
      State : in Semantic_State_Access_Type;
      Lexer : in Lexer_Pkg.Handle);
   --  Token has just been read from the lexer; push an augmented
   --  token matching it on the stack.

   with procedure Merge_Tokens
     (Nonterm : in Token.Nonterminal_ID;
      Index   : in Natural;
      Tokens  : in Token_Pkg.List.Instance;
      Action  : in Semantic_Action;
      State   : in Semantic_State_Access_Type);
   --  Called when a production is used to reduce the parser stack.
   --  This should maintain a separate stack of augmented tokens, and
   --  call the semantic action. Also output a trace according to
   --  FastToken.Trace_Parse.
   --
   --  See fasttoken.token_regions.ads for an example.

package FastToken.Parser.LR.Parser is

   type Instance is new FastToken.Parser.Instance with record
      Table                : Parse_Table_Ptr;
      Max_Parallel         : Integer;
      Terminate_Same_State : Boolean;
      Semantic_State       : Semantic_State_Access_Type;
   end record;

   function Initialize
     (Lexer                : in Lexer_Pkg.Handle;
      Table                : in Parse_Table_Ptr;
      Semantic_State       : in Semantic_State_Access_Type;
      Max_Parallel         : in Integer := 15;
      Terminate_Same_State : in Boolean := False)
     return Instance;

   overriding procedure Parse (Parser : in out Instance);
   --  Trace_Parse setttings:
   --  0 - no info
   --  1 - parallel parser create, delete
   --  2 - input tokens, reduce actions
   --  3 - parse stack

end FastToken.Parser.LR.Parser;
