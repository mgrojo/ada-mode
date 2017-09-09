with Gen_Parser_Run_GNATCOLL_Mmap;
with Case_Expression;
procedure Case_Expression_Run is new Gen_Parser_Run_GNATCOLL_Mmap
  (Case_Expression.Create_Parser);
