with Gen_Parser_Run_Counted_GNAT_OS_Lib;
with Case_Expression;
procedure Case_Expression_Run is new Gen_Parser_Run_Counted_GNAT_OS_Lib
  (Case_Expression.Token_ID,
   Case_Expression.First_Terminal,
   Case_Expression.Last_Terminal,
   Case_Expression.Token_Image,
   Case_Expression.Put_Trace,
   Case_Expression.Put_Trace_Line,
   Case_Expression.Token_Pkg,
   Case_Expression.Lexer_Root,
   Case_Expression.Token_Aug,
   Case_Expression.Parser_Root,
   Case_Expression.First_State_Index,
   Case_Expression.LR,
   Case_Expression.First_Parser_Label,
   Case_Expression.Parser_Lists,
   Case_Expression.Panic_Mode,
   Case_Expression.LR_Parser,
   Case_Expression.Create_Parser);
