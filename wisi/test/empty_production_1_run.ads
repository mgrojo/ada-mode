with Gen_Parser_Run;
with Empty_Production_1;
procedure Empty_Production_1_Run is new Gen_Parser_Run
  (Empty_Production_1.Token_IDs,
   Empty_Production_1.First_Terminal,
   Empty_Production_1.Last_Terminal,
   Empty_Production_1.Token_Image,
   Empty_Production_1.Tokens,
   Empty_Production_1.Analyzers,
   Empty_Production_1.Token_Lists,
   Empty_Production_1.Nonterminals,
   Empty_Production_1.Productions,
   Empty_Production_1.Parsers,
   Empty_Production_1.First_State_Index,
   Empty_Production_1.LALRs,
   Empty_Production_1.Parser_Lists,
   Empty_Production_1.LALR_Parsers,
   Empty_Production_1.Create_Parser);
