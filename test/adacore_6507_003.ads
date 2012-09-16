
--  With ada-mode3.3, this was uncorrectly indented.
--  This is to test the indentation of declarations in generics
with Ada.Text_IO;
generic
   type I is new Integer;
   type T is private;
   N : Integer;
   with package A is new Ada.Text_IO.Integer_IO (I);
package Adacore_6507_003 is
end Adacore_6507_003;


