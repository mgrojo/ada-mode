--  generated parser support file. -*- buffer-read-only:t  -*-
--  command line: wisitoken-bnf-generate.exe --generate LALR Ada_Emacs re2c Process --generate Tree_Sitter Ada_Emacs
--  Tree_Sitter Process --verbosity time=1 --output_bnf ada_annex_p.wy
--

with Interfaces.C.Extensions;
with Gen_Tree_Sitter_Parser_Run;
procedure Ada_Annex_P_Tree_Sitter_Run
is
   function Tree_Sitter_Ada_Annex_P return Interfaces.C.Extensions.void_ptr
   with Import     => True,
     External_Name => "tree_sitter_Ada_Annex_P",
     Convention    => C;
   procedure Parse_Run is new Gen_Tree_Sitter_Parser_Run
     (Tree_Sitter_Language => Tree_Sitter_Ada_Annex_P);
begin
   Parse_Run;
end Ada_Annex_P_Tree_Sitter_Run;
