--  Test indent after error correction inserts tokens with nil region.
--  Does not compile.
--
--  The test is that indent succeeds, and gives reasonable results.
--
procedure Ada_Mode.Recover_Indent_1
is
begin
   Check ("v - r", Graph.Find_Path (V, (Vertex, R))
          -- syntax error here; missing ');'
          -- indent consistent with extending parameter list

         Check ("1a", Graph, (5, 4, 3, 2, 1));
end Ada_Mode.Recover_Indent_1;
