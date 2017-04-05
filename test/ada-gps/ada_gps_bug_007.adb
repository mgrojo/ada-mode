--  Abstract :
--
--  ada_mode_gps_indent bug #7: comments after last parameter and end of statement
--
procedure Bug_007 is
begin
   E (Param_1,
      -- Comment 1 in middle of statement; indented to prev line,
      Param_2);
   -- Comment 2 after end of statement; indented to start of prev
   -- statement.

   E (Param_1,
      Param_2);

   -- Comment 3 after blank line after end of statement; indented to
   -- start of prev statement.

end Bug_007;
