with Gen_LR_Parser_No_Recover_Run;
with Empty_Production_2_Actions;
with Empty_Production_2_Main;
with WisiToken.Syntax_Trees;
procedure Empty_Production_2_LR_Run is new Gen_LR_Parser_No_Recover_Run
  (Empty_Production_2_Actions.Descriptor, Empty_Production_2_Main.Create_Parser);
