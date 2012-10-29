separate (Ada_Mode)
function Separate_Function return Integer is
   -- no comment before "separate"
   --EMACSCMD:(progn (forward-line -3)(ada-find-other-file t)(looking-at "function Separate_Function return"))
begin
   begin  -- should be indented.
      null;
   end;
   return 0;
end Separate_Function;
