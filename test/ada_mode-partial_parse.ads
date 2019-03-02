--  Abstract :
--
--  Test partial parse indent in a package spec
--
package Ada_Mode.Partial_Parse is

   type A_Type is (A, B, C);

   procedure Bar;

private

   type Private_Type is
     (D, E, F);

   function Foo
     (A : in Integer;
      B : in Float)
     return Float;

end Ada_Mode.Partial_Parse;
