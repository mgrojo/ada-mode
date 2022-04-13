--  Used to get error during resume
procedure Ada_Mode.Recover_String_Quote_5
is begin
   Test_One;
   -- The next line was typed in error in Recover_String_Quote_4
   --EMACSCMD:(progn (forward-line 1)(forward-char 25)(insert "\"")(indent-for-tab-command))
end Ada_Mode.Recover_String_Quote_5;
--EMACSCMD:(progn (forward-line -1)(forward-char 25)(delete-char 1))
