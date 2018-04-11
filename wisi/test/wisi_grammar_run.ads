with Gen_Parser_No_Recover_Run;
with Wisi_Grammar;
with WisiToken.Wisi_Grammar_Runtime;
procedure Wisi_Grammar_Run is new Gen_Parser_No_Recover_Run
  (WisiToken.Wisi_Grammar_Runtime.User_Data_Type, Wisi_Grammar.Descriptor, Wisi_Grammar.Create_Parser);
