with Gen_Parser_Run;
with Conflict_Name;
procedure Conflict_Name_Run is new Gen_Parser_Run (Conflict_Name.Create_Parser, LR1 => True);
