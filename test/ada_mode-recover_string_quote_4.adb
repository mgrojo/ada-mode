--  Used to fail validate_tree in Edit_Tree.
procedure Ada_Mode.Recover_String_Quote_4
is begin
   --EMACSCMD:(progn (forward-line 1)(forward-char 25)(delete-char 1)(indent-for-tab-command))
   Test_One ("""\\377""""\\377""");
   --EMACSCMD:(progn (forward-line -1)(forward-char 25)(insert "\\"))
end Ada_Mode.Recover_String_Quote_4;
