with Gen_Parser_Run;
with Empty_Production_4;
procedure Empty_Production_4_Run is new Gen_Parser_Run
  (Empty_Production_4.Token_IDs,
   Empty_Production_4.First_Terminal,
   Empty_Production_4.Last_Terminal,
   Empty_Production_4.Token_Image,
   Empty_Production_4.Tokens,
   Empty_Production_4.Nonterminals,
   Empty_Production_4.Productions,
   Empty_Production_4.Parsers,
   Empty_Production_4.First_State_Index,
   Empty_Production_4.LALRs,
   Empty_Production_4.Parser_Lists,
   Empty_Production_4.LALR_Parsers,
   Empty_Production_4.Create_Parser);
