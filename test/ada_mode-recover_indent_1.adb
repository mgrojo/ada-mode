--  Test indent after error correction inserts tokens with nil region.
--  Does not compile.
--
--  The test is that indent succeeds, and gives reasonable results.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
procedure Ada_Mode.Recover_Indent_1
is
begin
   Check ("v - r", Graph.Find_Path (V, (Vertex, R))
          -- syntax error here; missing ');'
          -- recover inserts '=>' (legal according to its relaxed grammar)

   Check ("1a", Graph, (5, 4, 3, 2, 1));
end Ada_Mode.Recover_Indent_1;
