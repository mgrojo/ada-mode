with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Character_Literal_Process;
procedure Character_Literal_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Character_Literal_Process.Token_ID,
   Character_Literal_Process.First_Terminal,
   Character_Literal_Process.Last_Terminal,
   Character_Literal_Process.Token_Image,
   Character_Literal_Process.Put_Trace,
   Character_Literal_Process.Put_Trace_Line,
   Character_Literal_Process.Token_Pkg,
   Character_Literal_Process.Nonterminal,
   Character_Literal_Process.Wisi_Tokens_Pkg,
   Character_Literal_Process.Lexer_Root,
   Character_Literal_Process.Parser_Root,
   Character_Literal_Process.First_State_Index,
   Character_Literal_Process.LR,
   Character_Literal_Process.First_Parser_Label,
   Character_Literal_Process.Parser_Lists,
   Character_Literal_Process.LR_Parser,
   Character_Literal_Process.Create_Parser);
