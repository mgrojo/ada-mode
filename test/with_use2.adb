
--  Tests the indentation of with and use statements, with regards to
--  the variable ada-with-use-indent

--EMACSCMD: ada-indent-with
--EMACSRESULT: 3
with Ada.Text_IO,
   Ada.Numerics,   --  used to be indented with ada-broken-indent = 2
   Foo;

use Ada.Text_IO,
      Ada.Numerics,   --  used to be indented with ada-broken-indent
      Foo;

procedure With_Use2 is begin null; end;

-- Local Variables:
-- ada-indent-with: 3
-- ada-indent-use: 6
-- End:
