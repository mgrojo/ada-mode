with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Empty_Production_3;
procedure Empty_Production_3_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Empty_Production_3.Token_IDs,
   Empty_Production_3.First_Terminal,
   Empty_Production_3.Last_Terminal,
   Empty_Production_3.Token_Image,
   Empty_Production_3.Tokens,
   Empty_Production_3.Nonterminals,
   Empty_Production_3.Productions,
   Empty_Production_3.Parsers,
   Empty_Production_3.First_State_Index,
   Empty_Production_3.LALRs,
   Empty_Production_3.Parser_Lists,
   Empty_Production_3.LALR_Parsers,
   Empty_Production_3.Create_Parser);
