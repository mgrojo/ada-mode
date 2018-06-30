with Gen_LR_Parser_No_Recover_Run;
with Conflict_Name_Actions;
with Conflict_Name_Main;
with WisiToken.Syntax_Trees;
procedure Conflict_Name_LR_Run is new Gen_LR_Parser_No_Recover_Run
  (Conflict_Name_Actions.Descriptor, Conflict_Name_Main.Create_Parser);
