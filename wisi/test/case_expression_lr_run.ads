with Gen_LR_Parser_No_Recover_Run;
with Case_Expression_Actions;
with Case_Expression_Main;
with WisiToken.Syntax_Trees;
procedure Case_Expression_LR_Run is new Gen_LR_Parser_No_Recover_Run
  (Case_Expression_Actions.Descriptor, Case_Expression_Main.Create_Parser);
