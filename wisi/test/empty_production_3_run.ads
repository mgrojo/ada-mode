with Gen_Parser_Run;
with Empty_Production_3;
procedure Empty_Production_3_Run is new Gen_Parser_Run
  (Empty_Production_3.Descriptor, Empty_Production_3.Create_Parser, LR1 => True);
