with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Subprograms_Process;
procedure Subprograms_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Subprograms_Process.Token_ID,
   Subprograms_Process.First_Terminal,
   Subprograms_Process.Last_Terminal,
   Subprograms_Process.Token_Image,
   Subprograms_Process.Put_Trace,
   Subprograms_Process.Put_Trace_Line,
   Subprograms_Process.Token_Pkg,
   Subprograms_Process.Nonterminal,
   Subprograms_Process.Lexer_Root,
   Subprograms_Process.Parser_Root,
   Subprograms_Process.First_State_Index,
   Subprograms_Process.LALR,
   Subprograms_Process.First_Parser_Label,
   Subprograms_Process.Parser_Lists,
   Subprograms_Process.LALR_Parser,
   Subprograms_Process.Create_Parser);
