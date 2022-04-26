-- Incremental parse encountered '2 parsers active' at accept, due to
-- actual ambiguity in grammar.
--
--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
--EMACSCMD:(setq skip-recase-test t)
--EMACSCMD:(setq skip-reindent-test t)
package body Ada_Mode.Interactive_12 is

   --EMACSCMD:(progn (forward-line 1)(search-forward "(@,")(forward-char -4)(wisi-replay-kbd-macro "\r"))

   function Min_Sequential_Index return Syntax_Trees.Sequential_Index
   is
      use Syntax_Trees;
   begin
      return Result : Sequential_Index := Sequential_Index'Last do
         for I in 1 .. Parser_Count loop
            Result := Sequential_Index'Min (@,  Tree.Get_Sequential_Index (Min_Sequential_Indices (I).Ref.Node));
         end loop;
      end return;
   end Min_Sequential_Index;
   --EMACSCMD:(progn (end-of-line -4)(delete-char 1))

end Ada_Mode.Interactive_12;
