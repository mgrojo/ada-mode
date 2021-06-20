-- This file deliberately has mixed Unix and DOS line endings, to
-- mimic a case reported by a user (they blame CM merge for the mixed
-- line endings). Previous versions hang due to character/byte count
-- mismatch.
--
-- File created with this recipe (editing in Emacs cleans up the mixed endings):
--  unix2dos --newfile mixed_unix_dos_line_ends.adb mixed_unix_dos_line_ends_dos.adb
--  dos2unix --newfile mixed_unix_dos_line_ends.adb mixed_unix_dos_line_ends_unix.adb
--  head --lines=9 mixed_unix_dos_line_ends_dos.adb > mixed_unix_dos_line_ends_merged.adb
--  tail --lines=10 mixed_unix_dos_line_ends_unix.adb >> mixed_unix_dos_line_ends_merged.adb
--  mv mixed_unix_dos_line_ends_merged.adb mixed_unix_dos_line_ends.adb

with Ada.Text_Io;

procedure Mixed_Unix_Dos_Line_Ends is
begin
   Ada.Text_Io.Put_Line ("Hello, World!");
end Mixed_Unix_Dos_Line_Ends;
