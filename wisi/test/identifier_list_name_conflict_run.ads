with Gen_Parser_Run;
with Identifier_List_Name_Conflict;
procedure Identifier_List_Name_Conflict_Run is new Gen_Parser_Run
  (Identifier_List_Name_Conflict.Descriptor, Identifier_List_Name_Conflict.Create_Parser, LR1 => True);
