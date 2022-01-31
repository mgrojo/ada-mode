-- Mimic a real editing session, typing code. Used to leave syntax
-- errors in tree.
--
procedure Ada_Mode.Interactive_09 is

   --EMACSCMD:(progn (forward-line 4)(kill-line 5))
   --EMACSCMD:(progn (end-of-line 3)(wisi-replay-kbd-macro-file "ada_mode-interactive_09.adb.macro"))
   procedure Code_Point_Length (Item : in String)
   is begin
      for C of Item loop
         if Character'Pos (C) < 127 then
            Result := @ + 1;
         end if;
      end loop;
   end Code_Point_Length;

   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT: 0
begin
   null;
end Ada_Mode.Interactive_09;
