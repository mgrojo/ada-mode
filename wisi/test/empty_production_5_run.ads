with Gen_Parser_Run;
with Empty_Production_5;
procedure Empty_Production_5_Run is new Gen_Parser_Run
  (Empty_Production_5.Token_IDs,
   Empty_Production_5.First_Terminal,
   Empty_Production_5.Last_Terminal,
   Empty_Production_5.Token_Image,
   Empty_Production_5.Tokens,
   Empty_Production_5.Analyzers,
   Empty_Production_5.Token_Lists,
   Empty_Production_5.Nonterminals,
   Empty_Production_5.Productions,
   Empty_Production_5.Parsers,
   Empty_Production_5.First_State_Index,
   Empty_Production_5.LALRs,
   Empty_Production_5.Parser_Lists,
   Empty_Production_5.LALR_Parsers,
   Empty_Production_5.Create_Parser);
