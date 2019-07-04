--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)

procedure Ada_Mode.Recover_Redundant_Solutions_1 is
begin
   if Songs.Length > Song_Count then
      if Debug then
         Debug_Log.Put (Songs.Slice (Song_Count, Songs.Length));
         Songs.Delete_Last (Songs.Length - Song_Count);
      end if;
      -- Missing 'end if'; recover finds two equivalent solutions, which are
      -- handled gracefully via duplicate state, but only at eob, which is
      -- inefficient.

end Ada_Mode.Recover_Redundant_Solutions_1;
