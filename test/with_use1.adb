--EMACSCMD:(setq skip-recase-test t)

--  Tests the indentation of with and use statements, with regards to
--  the variable ada-indent-with, ada-indent-use

with Ada.Text_IO,
     Ada.Numerics,   --  used to be indented with ada-broken-indent
     Ada.Strings;

use Ada.Text_IO,
    Ada.Numerics,   --  used to be indented with ada-broken-indent
    Ada.Strings;

procedure With_Use1 is
   use Ada.Text_IO,
       Ada.Numerics,   --  used to be indented with ada-broken-indent
       Ada.Strings;

begin null; end;
-- Local Variables:
-- ada-indent-with: 5
-- ada-indent-use: 4
-- End:
