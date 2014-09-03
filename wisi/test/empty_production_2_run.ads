with Gen_Parser_Run;
with Empty_Production_2;
procedure Empty_Production_2_Run is new Gen_Parser_Run
  (Empty_Production_2.Token_IDs,
   Empty_Production_2.First_Terminal,
   Empty_Production_2.Last_Terminal,
   Empty_Production_2.Token_Image,
   Empty_Production_2.Tokens,
   Empty_Production_2.Analyzers,
   Empty_Production_2.Token_Lists,
   Empty_Production_2.Nonterminals,
   Empty_Production_2.Productions,
   Empty_Production_2.Parsers,
   Empty_Production_2.LALRs,
   Empty_Production_2.LALR_Parsers,
   Empty_Production_2.Create_Parser);
