
--  This file tests the indentation of type declarations containing "is"
--  We have to test both when "is" and the following words are on the same
--  line and when they are on different lines.

package Test_Is is

   type A is access Integer;
   type B is
     access Integer;

   type C is new Integer;
   type D is
     new Integer;

end Test_Is;
