--  Test indent after error correction inserts tokens with nil region.
--  Does not compile.
--
--  The test is that indent succeeds, and gives reasonable results.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
procedure Debug_Kim_Choe
is
begin
   Check ("v - r", Graph.Find_Path (V, (Vertex, R)) -- syntax error here; missing ');'; recover inserts that

   Check ("1a", Graph, (5, 4, 3, 2, 1));
end Debug_Kim_Choe;
