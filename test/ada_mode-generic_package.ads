--  This is to test the indentation of declarations in generics package declarations
with Ada.Text_IO;
generic
   type I is new Integer;
   type T is private;
   N : Integer;
   with package A is new Ada.Text_IO.Integer_IO (I);
package Ada_Mode.Generic_Package is
end Ada_Mode.Generic_Package;
