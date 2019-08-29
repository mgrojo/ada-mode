function Ada_Mode.Library_Function return Integer is
   -- no comment before "function"
   --EMACSCMD:(wisi-prj-select-file "subdir/ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "Ada_Mode is"))

   --EMACSCMD:(progn (forward-line -6)(forward-word 4)(forward-char 1)(wisi-goto-declaration)(looking-at "Library_Function return Integer; -- spec"))
   --EMACSRESULT:t

begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Ada_Mode.Library_Function;
