--  Abstract :
--
--  Test syntax error recovery

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
         end; -- error 1 reported here; inserts 'if'
      end  -- error 2 reported here before 'loop'; inserts ';' for 'begin'
loop
; -- error 3 reported here before ';'; inserts 'end loop'
   end Ada_Mode.  -- error 4 reported here before '.'; fails
Interactive_Recover; 
-- FIXME: improve recover algorithm to reduce required enqueue limit
-- Local Variables:
-- wisi-mckenzie-enqueue-limit: 1400
-- End:
