--  Abstract :
--
--  Test indent and navigate parse with syntax error recovery

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)

--EMACSCMD:(indent-region (point-min) (point-max))
--EMACSCMD:(progn (ada-show-parse-error)(looking-at "; -- error reported here"))

--EMACSCMD:(progn (wisi-validate-cache (point-max) nil 'navigate)(wisi-cache-nonterm (wisi-get-cache (line-beginning-position 3))))
--EMACSCMD:'subprogram_body
procedure Ada_Mode.Interactive_Recover
is begin
   loop
      begin
         D;
         if A then -- missing matching "end if"
            if B then
            end if;
            exit when C;
         end; -- error reported here
   end
   loop
   ;
end Ada_Mode.
  Interactive_Recover;
-- end of file
