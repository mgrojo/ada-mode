-- From a real editing session. Same code as interactive_13, different editing actions.

--EMACS_SKIP_UNLESS:wisi-incremental-parse-enable
--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Interactive_14 (T : in out AUnit.Test_Cases.Test_Case'Class)
is
   pragma Unreferenced (T);
begin
   Parse_Text
     (Initial   => "function A return B.C_Type;",
      --                 |1       |10       |20
      Edit_At   => 21,
      Delete    => "C_T",
      --EMACSCMD:(progn (end-of-line 4)(kill-line 1)(call-interactively 'kill-line)(delete-char -1)(wisi-parse-incremental-none))
      --EMACSCMD:(progn (end-of-line 3)(kill-line 1)(call-interactively 'kill-line)(wisi-parse-incremental-none))
      --EMACSCMD:(progn (end-of-line 2)(kill-word 3)(wisi-parse-incremental-none))
      Insert    => "c_t",
      Edit_2_At => 27,
      Delete    => "",
      Insert    => "" & Ascii.LF);
   --EMACSCMD:(progn (end-of-line 0)(forward-char -2)(insert ",\nEdit_2_At => 27,\nDelete => \"\",\nInsert => \"\" & Ascii.LF")(ada-align))

end Ada_Mode.Interactive_14;
