function Ada_Mode.Library_Function return Integer is
   -- no comment before "function"
   --EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "Ada_Mode is"))

   --EMACSCMD:(progn (forward-line -6)(forward-word 4)(forward-char 1)(ada-goto-declaration nil)(looking-at "Library_Function return Integer; -- spec"))
   --EMACSRESULT:t

begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Ada_Mode.Library_Function;
