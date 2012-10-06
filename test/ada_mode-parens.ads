--  Explores all issues around indenting within parens
with Ada.Text_IO;
package Ada_Mode.Parens is

   --  A pathological subprogram declaration
   function Function_1
     (Param_1,
      Param_2 : in Ada.Text_IO.
        Count; -- pretending this is wrapped because of line length limit
      Param_3 : in
        out Integer; -- who would do this!?
      Param_4
        : in Float)
     return Float;

   type Array_Type_1 is array (1 .. 4) of Integer;

   function Function_2 (Left, Right : in Array_Type_1) return Array_Type_1;

end Ada_Mode.Parens;
