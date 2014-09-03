with Gen_Parser_Run;
with Empty_Production_7;
procedure Empty_Production_7_Run is new Gen_Parser_Run
  (Empty_Production_7.Token_IDs,
   Empty_Production_7.First_Terminal,
   Empty_Production_7.Last_Terminal,
   Empty_Production_7.Token_Image,
   Empty_Production_7.Tokens,
   Empty_Production_7.Analyzers,
   Empty_Production_7.Token_Lists,
   Empty_Production_7.Nonterminals,
   Empty_Production_7.Productions,
   Empty_Production_7.Parsers,
   Empty_Production_7.LALRs,
   Empty_Production_7.LALR_Parsers,
   Empty_Production_7.Create_Parser);
