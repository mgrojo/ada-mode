with Gen_Parser_Run;
with Empty_Production_8;
procedure Empty_Production_8_Run is new Gen_Parser_Run
  (Empty_Production_8.Descriptor, Empty_Production_8.Create_Parser, LR1 => False);
