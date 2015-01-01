with Gen_Parser_Run;
with Subprograms;
procedure Subprograms_Run is new Gen_Parser_Run
  (Subprograms.Token_IDs,
   Subprograms.First_Terminal,
   Subprograms.Last_Terminal,
   Subprograms.Token_Image,
   Subprograms.Tokens,
   Subprograms.Nonterminals,
   Subprograms.Productions,
   Subprograms.Parsers,
   Subprograms.First_State_Index,
   Subprograms.LALRs,
   Subprograms.Parser_Lists,
   Subprograms.LALR_Parsers,
   Subprograms.Create_Parser);
