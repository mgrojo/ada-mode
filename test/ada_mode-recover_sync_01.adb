--  From a real editing session. Used to get "parsers not synced".
procedure Ada_Mode.Recover_Sync_01
is begin
   declare
      --EMACSCMD:(progn (forward-line 1)(forward-symbol 4)(backward-char 5)(delete-char 1)(indent-for-tab-command))
      Update_Target : constant Boolean :=   Term.Ref.Node = Target then
         Tree.Add_Errors (Term, New_Errors, User_Data);
         Inverted_Parents := Term.Parents.Invert;
end Ada_Mode.Recover_Sync_01;
