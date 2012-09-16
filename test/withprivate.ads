
--  Tests the indentation of "with private" and "null record"
--  even if these are not on the same line


package Withprivate is

   type B is tagged null record;

   type A is new B with private;
   --  "with private" on the same code line

   type C is new B with
     private;
   --  "with private" not on the same code line

   type D is null record;
   --  "null record" on the same line

   type E is null
      record;
   --  "null record" not on the same line

private
   type A is new B with null record;
   type C is new B with null record;
end Withprivate;
