-- Used in ada_mode-nominal.ads for test of ada-find-other-file
--
-- Also tests indentation of 'begin' when newly typed
--
--EMACSCMD:(setq wisi-debug 0); ignore syntax errors
--EMACSCMD:(progn (forward-line 2)(kill-line)(kill-backward-chars 1)(newline-and-indent)(insert "be")(jit-lock-fontify-now)(insert "gin")(jit-lock-fontify-now)(indent-for-tab-command)(back-to-indentation)(current-column))
function Ada_Mode.Function_2 return Boolean is
begin
   --EMACSRESULT:0
   return True;
end Ada_Mode.Function_2;
--EMACSCMD:(setq wisi-debug 1); don't ignore syntax errors
