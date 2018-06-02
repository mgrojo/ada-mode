with Gen_Parser_No_Recover_Run;
with Case_Expression_Actions;
with Case_Expression_Main;
with WisiToken.Syntax_Trees;
procedure Case_Expression_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Syntax_Trees.User_Data_Type, Case_Expression_Actions.Descriptor, Case_Expression_Main.Create_Parser);
