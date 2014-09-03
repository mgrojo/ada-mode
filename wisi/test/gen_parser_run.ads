with OpenToken.Production;
with OpenToken.Production.Parser.LALR.Parser;
with OpenToken.Text_Feeder;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
generic
   type Token_IDs is (<>);
   First_Terminal : in Token_IDs;
   Last_Terminal  : in Token_IDs;
   with function Token_Image (Item : in Token_IDs) return String;
   with package Tokens is new OpenToken.Token.Enumerated (Token_IDs, First_Terminal, Last_Terminal, Token_Image);
   with package Analyzers is new Tokens.Analyzer;
   with package Token_Lists is new Tokens.List;
   with package Nonterminals is new Tokens.Nonterminal (Token_Lists);
   with package Productions is new OpenToken.Production (Tokens, Token_Lists, Nonterminals);
   with package Parsers is new Productions.Parser (Analyzers);
   with package LALRs is new Parsers.LALR (First_State_Index => 1);
   with package LALR_Parsers is new LALRs.Parser;

   with function Create_Parser
     (Max_Parallel         : in Integer := 15;
      Terminate_Same_State : in Boolean := False;
      Text_Feeder          : in OpenToken.Text_Feeder.Text_Feeder_Ptr := null)
     return LALR_Parsers.Instance;
procedure Gen_Parser_Run;
