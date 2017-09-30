with Gen_Parser_Run;
with Empty_Production_7;
procedure Empty_Production_7_Run is new Gen_Parser_Run (Empty_Production_7.Create_Parser, LR1 => True);
