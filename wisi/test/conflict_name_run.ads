with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Conflict_Name_Process;
procedure Conflict_Name_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Conflict_Name_Process.Token_ID,
   Conflict_Name_Process.First_Terminal,
   Conflict_Name_Process.Last_Terminal,
   Conflict_Name_Process.Token_Image,
   Conflict_Name_Process.Put_Trace,
   Conflict_Name_Process.Put_Trace_Line,
   Conflict_Name_Process.Token_Pkg,
   Conflict_Name_Process.Nonterminal,
   Conflict_Name_Process.Wisi_Tokens_Pkg,
   Conflict_Name_Process.Lexer_Root,
   Conflict_Name_Process.Parser_Root,
   Conflict_Name_Process.First_State_Index,
   Conflict_Name_Process.LR,
   Conflict_Name_Process.First_Parser_Label,
   Conflict_Name_Process.Parser_Lists,
   Conflict_Name_Process.LR_Parser,
   Conflict_Name_Process.Create_Parser);
