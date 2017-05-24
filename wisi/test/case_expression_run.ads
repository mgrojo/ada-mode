with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Case_Expression_Process;
procedure Case_Expression_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Case_Expression_Process.Token_ID,
   Case_Expression_Process.First_Terminal,
   Case_Expression_Process.Last_Terminal,
   Case_Expression_Process.Token_Image,
   Case_Expression_Process.Put_Trace,
   Case_Expression_Process.Put_Trace_Line,
   Case_Expression_Process.Token_Pkg,
   Case_Expression_Process.Lexer_Root,
   Case_Expression_Process.Parser_Root,
   Case_Expression_Process.First_State_Index,
   Case_Expression_Process.LR,
   Case_Expression_Process.First_Parser_Label,
   Case_Expression_Process.Parser_Lists,
   Case_Expression_Process.Panic_Mode,
   Case_Expression_Process.LR_Parser,
   Case_Expression_Process.Create_Parser);
