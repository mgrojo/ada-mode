-- This file deliberately has mixed Unix and DOS line endings, to
-- mimic a case reported by a user (they blame CM merge for the mixed
-- line endings). Previous versions hang due to character/byte count
-- mismatch.
--
-- File created with this recipe:
--  unix2dos --newfile hello_world.adb hello_world_dos.adb
--  dos2unix --newfile hello_world.adb hello_world_unix.adb
--  head --lines=3 hello_world_dos.adb >hello_world_merged.adb
--  tail --lines=3 hello_world_unix.adb >>hello_world_merged.adb

with Ada.Text_IO;

procedure Mixed_Unix_Dos_Line_Ends is
begin
   Ada.Text_IO.Put_Line ("Hello, World!");
end Mixed_Unix_Dos_Line_Ends;
