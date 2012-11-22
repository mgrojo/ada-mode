separate (Ada_Mode)
function Separate_Function return Integer is
   -- no comment before "separate"
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "function Separate_Function return"))
begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Separate_Function;
