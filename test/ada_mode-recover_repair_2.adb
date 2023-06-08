-- Test that wisi-repair-errors works when there are multiple insert/delete; requires markers.
--
procedure Ada_Mode.Recover_Repair_2
is begin
   for I in 1 .. 10 loop
      null;
      --EMACSCMD:(progn (end-of-line 3)(backward-word 1)(kill-word 1)(insert "if")(indent-for-tab-command))
      --EMACSCMD:(wisi-repair-errors)
   end loop;
end Ada_Mode.Recover_Repair_2;
