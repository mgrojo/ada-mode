with Gen_Parser_Run;
with Empty_Production_3;
procedure Empty_Production_3_Run is new Gen_Parser_Run
  (Empty_Production_3.Token_IDs,
   Empty_Production_3.First_Terminal,
   Empty_Production_3.Last_Terminal,
   Empty_Production_3.Token_Image,
   Empty_Production_3.Tokens,
   Empty_Production_3.Analyzers,
   Empty_Production_3.Token_Lists,
   Empty_Production_3.Nonterminals,
   Empty_Production_3.Productions,
   Empty_Production_3.Parsers,
   Empty_Production_3.LALRs,
   Empty_Production_3.LALR_Parsers,
   Empty_Production_3.Create_Parser);
