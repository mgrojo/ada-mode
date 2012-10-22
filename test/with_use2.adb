
--  Tests the indentation of with and use statements, with regards to
--  the variable ada-with-use-indent

--EMACSCMD: (setq ada-indent-with 2)
--EMACSCMD: (setq ada-indent-use 6)

with Ada.Text_IO,
  Ada.Numerics,   --  used to be indented with ada-broken-indent
  Foo;

use Ada.Text_IO,
      Ada.Numerics,   --  used to be indented with ada-broken-indent
      Foo;

procedure With_Use2 is begin null; end;
