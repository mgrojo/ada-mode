-- This is from a real editing session. Error recovery used to take a
-- long time and then fail; now it is very fast.
--
--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
procedure Ada_Mode.Recover_18
is begin
   return Result : String := Item do
      for I in Result'Range loop
         if Result (I) = '+' then
   --  missing 'end if; end loop; end return;'
end Ada_Mode.Recover_18;
