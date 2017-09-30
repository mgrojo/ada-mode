with Gen_Parser_Run;
with Empty_Production_2;
procedure Empty_Production_2_Run is new Gen_Parser_Run (Empty_Production_2.Create_Parser, LR1 => False);
