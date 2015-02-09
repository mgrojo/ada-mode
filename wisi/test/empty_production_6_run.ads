with Gen_Parser_Run_Text_IO;
with Empty_Production_6;
procedure Empty_Production_6_Run is new Gen_Parser_Run_Text_IO
  (Empty_Production_6.Token_IDs,
   Empty_Production_6.First_Terminal,
   Empty_Production_6.Last_Terminal,
   Empty_Production_6.Token_Image,
   Empty_Production_6.Tokens,
   Empty_Production_6.Nonterminals,
   Empty_Production_6.Productions,
   Empty_Production_6.Parsers,
   Empty_Production_6.First_State_Index,
   Empty_Production_6.LALRs,
   Empty_Production_6.Parser_Lists,
   Empty_Production_6.LALR_Parsers,
   Empty_Production_6.Create_Parser);
