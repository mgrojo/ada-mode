-- From a real editing session. Same code as interactive_13, different editing actions.

--EMACSCMD:(setq skip-recase-test t)
procedure Edit_Code_16 (T : in out AUnit.Test_Cases.Test_Case'Class)
is
   pragma Unreferenced (T);
begin
   --  From ada_mode-interactive_12.adb; token extends beyond first change
   --  region, second change region inserts following non_grammar.
   Parse_Text
     (Initial   => "function A return B.C_Type;",
      --                 |1       |10       |20
      Edit_At   => 21,
      Delete    => "C_T",
      --EMACSCMD:(progn (end-of-line 4)(kill-line 1)(call-interactively 'kill-line)(delete-char -1)(sit-for 0.01))
      --EMACSCMD:(progn (end-of-line 3)(kill-line 1)(call-interactively 'kill-line)(sit-for 0.01))
      --EMACSCMD:(progn (end-of-line 2)(kill-word 3)(sit-for 0.01))
      Insert    => "c_t",
      Edit_2_At => 27,
      Delete    => "",
      Insert    => "" & Ascii.LF);
   --EMACSCMD:(progn (end-of-line 0)(forward-char -2)(wisi-replay-kbd-macro ",\rEdit_2_At => 27,\rDelete => \"\",\rInsert => \"\" & Ascii.LF")(ada-align))

end Edit_Code_16;
