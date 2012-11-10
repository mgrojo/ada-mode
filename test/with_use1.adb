
--  Tests the indentation of with and use statements, with regards to
--  the variable ada-with-use-indent

with Ada.Text_IO,
     Ada.Numerics,   --  used to be indented with ada-broken-indent
     Foo;

use Ada.Text_IO,
    Ada.Numerics,   --  used to be indented with ada-broken-indent
    Foo;

procedure With_Use1 is
   use Ada.Text_IO,
       Ada.Numerics,   --  used to be indented with ada-broken-indent
       Foo;

begin null; end;
-- Local Variables:
-- ada-indent-with: 5
-- ada-indent-use: 4
-- End:
