--  Descr: The statements after a 'null record' on a separate line
--         are incorrectly indented.
--  Date: Thu Mar 18 14:38:57 EST 1999
--  reported by: Emmanuel Briot <briot@gnat.com>
--  fixed in: ada-mode 3.2

package Indent4 is

   --  null record alone on its own line
   type A is tagged
     null record;

   type B is new Integer;


   --  null record not alone on its own line
   type C is
     tagged null record;

   type D is new Integer;

   --  null record on the same line as type
   type Client is null record;
   type E is new Integer;

end Indent4;
