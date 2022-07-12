
--EMACS_SKIP_UNLESS: wisi-incremental-parse-enable
procedure Ada_Mode.Recover_String_Quote_6
is
begin
   --EMACSCMD:(progn (forward-line 1)(forward-symbol 2)(delete-char 3)(wisi-replay-kbd-macro " of")(forward-symbol 1)(forward-char 1)(wisi-replay-kbd-macro (kbd "\C-d\C-d\C-d\C-d\C-d\C-dnonterm_top_down"))(delete-char 6))
   for Nonterm in Shared_Parser.Derivs'Range loop
      --EMACSCMD:(progn (forward-line -1)(forward-symbol 2)(delete-char 3)(wisi-replay-kbd-macro " in")(forward-symbol 1)(forward-char 1)(delete-char 16)(insert "Derivs'Range"))
      declare
         Node  : constant Valid_Node_Access := Shared_Parser.Derivs (Nonterm)(Pos).Result;
         Index : constant Node_Index        := Tree.Get_Node_Index (Node);
      begin
         if Tree.Is_Empty_Nonterm (Node) then
            Empty_Nonterm_Nodes.Append (Node);

         else
            --  FIXME: could be more than one root nonterm here. need test case.
            --  parents are not set; check if byte_region includes current entries?
            --  build partial parent map?
            if Index < Found_Nonterm_Node_Index then
               Found_Nonterm_Node_Index := Index;
               Found_Nonterm := Nonterm;
            end if;
         end if;
      end;
   end loop;
end Ada_Mode.Recover_String_Quote_6;
