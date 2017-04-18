with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Multi_Conflict_Process;
procedure Multi_Conflict_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Multi_Conflict_Process.Token_ID,
   Multi_Conflict_Process.First_Terminal,
   Multi_Conflict_Process.Last_Terminal,
   Multi_Conflict_Process.Token_Image,
   Multi_Conflict_Process.Put_Trace,
   Multi_Conflict_Process.Put_Trace_Line,
   Multi_Conflict_Process.Token_Pkg,
   Multi_Conflict_Process.Nonterminal,
   Multi_Conflict_Process.Lexer_Root,
   Multi_Conflict_Process.Parser_Root,
   Multi_Conflict_Process.First_State_Index,
   Multi_Conflict_Process.LR,
   Multi_Conflict_Process.First_Parser_Label,
   Multi_Conflict_Process.Parser_Lists,
   Multi_Conflict_Process.LR_Parser,
   Multi_Conflict_Process.Create_Parser);
