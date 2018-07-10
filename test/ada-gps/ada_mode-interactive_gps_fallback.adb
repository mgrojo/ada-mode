--  Test using ada_mode_gps_indent as a fallback indent algorithm.
--
--  doesn't compile; invalid syntax

--  The process parser has error recovery, so the fallback indent
--  algorithm is not used.
--EMACS_SKIP_UNLESS:(not (eq ada-parser 'process))

-- Since we are editing, the syntax will be illegal at times; don't fail for that.
--EMACSCMD:(setq wisi-debug 0)

procedure Ada_Mode.Interactive_Gps_Fallback
is
begin
   --  Missing "end if;"
   --EMACSCMD:(progn (forward-line 2)(indent-according-to-mode)(current-indentation))
   --EMACSCMD:3
   if A then
      --EMACSCMD:(progn (forward-line 2)(indent-according-to-mode)(current-indentation))
      --EMACSCMD:6
      B := 0;

   end Ada_Mode.Interactive_Gps_Fallback;
