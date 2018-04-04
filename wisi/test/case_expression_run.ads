with Gen_Parser_Run;
with Case_Expression;
with WisiToken.Syntax_Trees;
procedure Case_Expression_Run is new Gen_Parser_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Case_Expression.Descriptor, Case_Expression.Create_Parser, LR1 => True);
