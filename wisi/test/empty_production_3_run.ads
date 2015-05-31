with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Empty_Production_3_Process;
procedure Empty_Production_3_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Empty_Production_3_Process.Token_ID,
   Empty_Production_3_Process.First_Terminal,
   Empty_Production_3_Process.Last_Terminal,
   Empty_Production_3_Process.Token_Image,
   Empty_Production_3_Process.Put_Trace,
   Empty_Production_3_Process.Put_Trace_Line,
   Empty_Production_3_Process.Token_Pkg,
   Empty_Production_3_Process.Nonterminal,
   Empty_Production_3_Process.Lexer_Root,
   Empty_Production_3_Process.Parser_Root,
   Empty_Production_3_Process.First_State_Index,
   Empty_Production_3_Process.LALR,
   Empty_Production_3_Process.First_Parser_Label,
   Empty_Production_3_Process.Parser_Lists,
   Empty_Production_3_Process.LALR_Parser,
   Empty_Production_3_Process.Create_Parser);
