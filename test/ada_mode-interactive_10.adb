package body Ada_Mode.Interactive_10 is
      --EMACSCMD:(progn (end-of-line 5)(execute-kbd-macro ";")(indent-for-tab-command) wisi-parse-failed)
      --EMACSRESULT:nil
      procedure Print_Node (Node : in Valid_Node_Access; Level : in Integer)
   is
      Need_Prefix : Boolean := True
        --EMACSCMD:(progn (end-of-line 0)(delete-char -1)(forward-symbol -1)(downcase-word 1))
   begin
         for I in 1 .. Level loop
            Trace.Put ("| ", Prefix => False);
         end loop;
         Put ("foo");

      end Print_Node;

end Ada_Mode.Interactive_10;
