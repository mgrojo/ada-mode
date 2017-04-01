--  Abstract :
--
--  ada_mode_gps_indent bug #5: jumps to column zero after RET
--
procedure Bug_005 is
   --EMACSCMD:(progn (end-of-line 3)(ada-indent-newline-indent)(current-column))
   --EMACSRESULT:6
   procedure X is
   begin
   end X;
   --EMACSCMD:(progn (forward-line -3)(kill-line))

   --EMACSCMD:(progn (end-of-line 2)(ada-indent-newline-indent)(current-column))
   Version : constant String := "1.001";
   --EMACSRESULT:3
   --EMACSCMD:(progn (forward-line -2)(kill-line))
begin

end Bug_005;
