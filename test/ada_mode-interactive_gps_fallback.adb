--  Test using ada_mode_gps_indent as a fall-back indent algorithm.
--
--  doesn't compile; invalid syntax

-- Since we are editing, the syntax will be illegal at times; don't fail for that.
--EMACSCMD:(setq wisi-debug 0)

procedure Ada_Mode.Interactive_GPS_Fallback
is
begin
   --  Missing "end if;"
   --EMACSCMD:(progn (forward-line 2)(indent-according-to-mode)(current-indentation)
   --EMACSCMD:3
   if A then
      --EMACSCMD:(progn (forward-line 2)(indent-according-to-mode)(current-indentation)
      --EMACSCMD:6
      B := 0;

end Ada_Mode.Interactive_GPS_Fallback;
