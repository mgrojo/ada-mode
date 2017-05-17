--  Abstract:
--
--  Main program to run a parser, using a Counted_GNAT_OS_Lib text feeder.
--
--  Copyright (C) 2015, 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with FastToken.Lexer;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Panic_Mode;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Text_Feeder;
with FastToken.Token;
generic
   type Token_ID is (<>);
   First_Terminal : in Token_ID;
   Last_Terminal  : in Token_ID;
   --  We assume this package is instantiated for a grammar generated
   --  by wisi-generate, which allows the following assumptions:
   --
   --  Last_Terminal is EOF_ID
   --  Token_ID'Succ(Last_Terminal) is Accept_ID
   with function Token_Image (Item : in Token_ID) return String;
   with procedure Put_Trace (Item : in String);
   with procedure Put_Trace_Line (Item : in String);
   with package Token_Pkg is new FastToken.Token (Token_ID, First_Terminal, Last_Terminal, Token_Image, Put_Trace);
   with package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   with package Parser_Root is new FastToken.Parser
     (Token_ID, First_Terminal, Last_Terminal, Last_Terminal, Token_ID'Succ (Last_Terminal), Token_Image, Put_Trace,
      Token_Pkg, Lexer_Root);
   First_State_Index : in Integer;
   with package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Token_Pkg.Get);
   First_Parser_Label : in Integer;
   with package Parser_Lists is new LR.Parser_Lists (First_Parser_Label, Put_Trace, Put_Trace_Line);
   with package Panic_Mode is new LR.Panic_Mode
     (First_Parser_Label, Put_Trace, Put_Trace_Line, Parser_Lists);
   with package LR_Parser is new LR.Parser (First_Parser_Label, Put_Trace, Put_Trace_Line, Parser_Lists, Panic_Mode);

   with function Create_Parser
     (Algorithm            : in FastToken.Parser_Algorithm_Type;
      Max_Parallel         : in Integer                               := 15;
      Terminate_Same_State : in Boolean                               := False;
      Text_Feeder          : in FastToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size          : in Integer                               := 1024)
     return LR_Parser.Instance;

procedure Gen_Parser_Run_Counted_GNAT_OS_Lib;
