-- Mimic a real editing session, typing code. Used to leave syntax
-- errors in tree.
--
package body Ada_Mode.Interactive_09 is

   --EMACSCMD:(progn (forward-line 4)(kill-line 5))
   --EMACSCMD:(progn (wisi-reset-parser)(end-of-line 3)(wisi-replay-kbd-macro "ada_mode-interactive_09.adb.macro"))
   function Code_Point_Length (Item : in String) return Integer
   is begin
      for C of Item loop
         if Chacater'Pos (C) < 127 then
            Result := @ + 1;
         end if;
      end loop;
   end Code_Point_Length;

   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT: 0
end Ada_Mode.Interactive_09;
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
