-- Used in ada_mode-nominal.ads for test of ada-find-other-file
--
-- Also tests indentation of 'begin' when newly typed; ada-smie-cache was not being updated properly.
--EMACSCMD:(progn (forward-line 2)(kill-line)(kill-backward-chars 1)(newline-and-indent)(insert "be")(sit-for 0.1)(insert "gin")(sit-for 0.1)(indent-for-tab-command)(back-to-indentation)(current-column))
function Ada_Mode.Function_2 return Boolean is
begin
   --EMACSRESULT:0
   return True;
end Ada_Mode.Function_2;
