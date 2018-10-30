--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)

procedure Ada_Mode.Recover_Redundant_Solutions_1 is
begin
   if Songs.Length > Song_Count then
      if Debug then
         Debug_Log.Put (Songs.Slice (Song_Count, Songs.Length));
         Songs.Delete_Last (Songs.Length - Song_Count);
      end if;
      -- Missing 'end if'; recover used to find two equivalent solutions,
      -- which were handled gracefully via duplicate state, but is
      -- inefficient, and could be a problem in other situations.
      -- FIXME: error on equivalent solutions

end Ada_Mode.Recover_Redundant_Solutions_1;
