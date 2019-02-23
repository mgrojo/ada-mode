--  From a real editing session. Now finds a reasonable solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)

--EMACSCMD:(progn (wisi-validate-cache (point-min) (point-max) nil 'navigate)(wisi-cache-nonterm (wisi-get-cache (line-beginning-position 3))))
--EMACSCMD:'subprogram_body
procedure Ada_Mode.Recover_1
is begin

   --EMACSCMD:(indent-region (point-min) (point-max))
   --EMACSCMD:(progn (ada-show-parse-error)(looking-at "; -- error reported here"))

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

   --  This used to give "error during resume"; fixed now.
   if File_Head.Version_Msb = 4 and
   then
      null;
   end if;

   -- This used to have a newline after the '.'; that confuses the
   -- Match_Name check. It's not a credible style, so we don't have it
   -- here any more.
end Ada_Mode.Recover_1;
-- end of file
