
--  Tests the indentation of with and use statements, with regards to
--  the variables ada-indent-with, ada-indent--use

--EMACSCMD:(setq skip-recase-test t)
--EMACSCMD: ada-indent-with
--EMACSRESULT: 3
with Ada.Text_IO,
   Ada.Numerics,   --  used to be indented with ada-broken-indent = 2
   Ada.Strings;

use Ada.Text_IO,
      Ada.Numerics,   --  used to be indented with ada-broken-indent
      Ada.Strings;

procedure With_Use2 is begin null; end With_Use2;

-- Local Variables:
-- ada-indent-with: 3
-- ada-indent-use: 6
-- End:
