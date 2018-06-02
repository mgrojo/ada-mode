with Gen_Parser_No_Recover_Run;
with Wisi_Grammar_Actions;
with Wisi_Grammar_Main;
with WisiToken.Wisi_Grammar_Runtime;
procedure Wisi_Grammar_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Wisi_Grammar_Runtime.User_Data_Type, Wisi_Grammar_Actions.Descriptor, Wisi_Grammar_Main.Create_Parser);
