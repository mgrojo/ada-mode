with Gen_LR_Parser_No_Recover_Run;
with Wisi_Grammar_Actions;
with Wisi_Grammar_Main;
procedure Wisi_Grammar_Run is new Gen_LR_Parser_No_Recover_Run
  (Wisi_Grammar_Actions.Descriptor, Wisi_Grammar_Main.Create_Parser);
