-- This file deliberately has mixed Unix and DOS line endings, to
-- mimic a case reported by a user (they blame CM merge for the mixed
-- line endings). Previous versions hang due to character/byte count
-- mismatch.

with Ada.Text_IO;

procedure Hello_World is
begin
   Ada.Text_IO.Put_Line ("Hello, World!");
end Hello_World;
