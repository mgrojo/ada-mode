function Ada_Mode.Library_Function return Integer is
   -- no comment before "function"
   --EMACSCMD:(progn (forward-line -2)(ada-find-other-file t)(looking-at "Ada_Mode is"))
begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Ada_Mode.Library_Function;
