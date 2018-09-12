procedure Debug is
begin
   if Songs.Length > Song_Count then
      if Debug then
         declare -- extra 'declare'

         Debug_Log.Put (Songs.Slice (Song_Count, Songs.Length))
         Songs.Delete_Last (Songs.Length - Song_Count);
      end if;
      -- Missing 'end if'; recover used to find two equivalent solutions,
      -- which were handled gracefully via duplicate state, but is
      -- inefficient, and could be a problem in other situations.
      -- FIXME: error on equivalent solutions
end Debug;
