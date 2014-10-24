with Gen_Parser_Run;
with Empty_Production_8;
procedure Empty_Production_8_Run is new Gen_Parser_Run
  (Empty_Production_8.Token_IDs,
   Empty_Production_8.First_Terminal,
   Empty_Production_8.Last_Terminal,
   Empty_Production_8.Token_Image,
   Empty_Production_8.Tokens,
   Empty_Production_8.Analyzers,
   Empty_Production_8.Token_Lists,
   Empty_Production_8.Nonterminals,
   Empty_Production_8.Productions,
   Empty_Production_8.Parsers,
   Empty_Production_8.First_State_Index,
   Empty_Production_8.LALRs,
   Empty_Production_8.Parser_Lists,
   Empty_Production_8.LALR_Parsers,
   Empty_Production_8.Create_Parser);
