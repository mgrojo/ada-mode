with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Identifier_List_Name_Conflict_Process;
procedure Identifier_List_Name_Conflict_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Identifier_List_Name_Conflict_Process.Token_ID,
   Identifier_List_Name_Conflict_Process.First_Terminal,
   Identifier_List_Name_Conflict_Process.Last_Terminal,
   Identifier_List_Name_Conflict_Process.Token_Image,
   Identifier_List_Name_Conflict_Process.Put_Trace,
   Identifier_List_Name_Conflict_Process.Put_Trace_Line,
   Identifier_List_Name_Conflict_Process.Token_Pkg,
   Identifier_List_Name_Conflict_Process.Nonterminal,
   Identifier_List_Name_Conflict_Process.Wisi_Tokens_Pkg,
   Identifier_List_Name_Conflict_Process.Lexer_Root,
   Identifier_List_Name_Conflict_Process.Parser_Root,
   Identifier_List_Name_Conflict_Process.First_State_Index,
   Identifier_List_Name_Conflict_Process.LR,
   Identifier_List_Name_Conflict_Process.First_Parser_Label,
   Identifier_List_Name_Conflict_Process.Parser_Lists,
   Identifier_List_Name_Conflict_Process.LR_Parser,
   Identifier_List_Name_Conflict_Process.Create_Parser);
