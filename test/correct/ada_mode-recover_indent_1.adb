procedure Ada_Mode.Recover_Indent_1
is
begin
   Check ("v - r", Graph.Find_Path (V, (Vertex, R)));
   Check ("1a", Graph, (5, 4, 3, 2, 1));
end Ada_Mode.Recover_Indent_1;
