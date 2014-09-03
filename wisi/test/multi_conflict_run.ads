with Gen_Parser_Run;
with Multi_Conflict;
procedure Multi_Conflict_Run is new Gen_Parser_Run
  (Multi_Conflict.Token_IDs,
   Multi_Conflict.First_Terminal,
   Multi_Conflict.Last_Terminal,
   Multi_Conflict.Token_Image,
   Multi_Conflict.Tokens,
   Multi_Conflict.Analyzers,
   Multi_Conflict.Token_Lists,
   Multi_Conflict.Nonterminals,
   Multi_Conflict.Productions,
   Multi_Conflict.Parsers,
   Multi_Conflict.LALRs,
   Multi_Conflict.LALR_Parsers,
   Multi_Conflict.Create_Parser);
