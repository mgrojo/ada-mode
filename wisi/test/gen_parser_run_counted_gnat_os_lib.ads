--  Main program to run a parser, using a Counted_GNAT_OS_Lib text feeder.

with OpenToken.Production;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Production.Parser.LALR.Parser_Lists;
with OpenToken.Text_Feeder;
with OpenToken.Token.Nonterminal;
generic
   type Token_IDs is (<>);
   First_Terminal : in Token_IDs;
   Last_Terminal  : in Token_IDs;
   with function Token_Image (Item : in Token_IDs) return String;
   with package Tokens is new OpenToken.Token (Token_IDs, First_Terminal, Last_Terminal, Token_Image);
   with package Nonterminals is new Tokens.Nonterminal;
   with package Productions is new OpenToken.Production (Tokens, Nonterminals);
   with package Parsers is new Productions.Parser;
   First_State_Index : in Integer;
   with package LALRs is new Parsers.LALR (First_State_Index);
   First_Parser_Label : in Integer;
   with package Parser_Lists is new LALRs.Parser_Lists (First_Parser_Label);
   with package LALR_Parsers is new LALRs.Parser (First_Parser_Label, Parser_Lists);

   with function Create_Parser
     (Max_Parallel         : in Integer := 15;
      Terminate_Same_State : in Boolean := False;
      Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null;
      Buffer_Size          : in Integer                               := 1024)
     return LALR_Parsers.Instance;
procedure Gen_Parser_Run_Counted_GNAT_OS_Lib;
