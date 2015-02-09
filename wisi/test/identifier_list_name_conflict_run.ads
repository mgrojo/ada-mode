with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Identifier_List_Name_Conflict;
procedure Identifier_List_Name_Conflict_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Identifier_List_Name_Conflict.Token_IDs,
   Identifier_List_Name_Conflict.First_Terminal,
   Identifier_List_Name_Conflict.Last_Terminal,
   Identifier_List_Name_Conflict.Token_Image,
   Identifier_List_Name_Conflict.Tokens,
   Identifier_List_Name_Conflict.Nonterminals,
   Identifier_List_Name_Conflict.Productions,
   Identifier_List_Name_Conflict.Parsers,
   Identifier_List_Name_Conflict.First_State_Index,
   Identifier_List_Name_Conflict.LALRs,
   Identifier_List_Name_Conflict.Parser_Lists,
   Identifier_List_Name_Conflict.LALR_Parsers,
   Identifier_List_Name_Conflict.Create_Parser);
