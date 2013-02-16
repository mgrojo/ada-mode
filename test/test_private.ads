--EMACSCMD:(setq skip-recase-test t)
--  Test the indentation for the word 'private'
--  We need to test some simple cases, where private is alone
--  in a package, as well as some more complicated cases
--  where there is already a package or protected type before
with Ada.Text_IO;
package Test_Private is

   package Private_Simple is
      type A is new Integer;
   private
      type B is new Integer;
   end Private_Simple;

   package Private_Second is
      type A is new Integer;
      protected type B is
         procedure Foo;
      private
         Handle : Integer;
      end B;
   private
      type C is new Integer;
   end Private_Second;

   package Private_Third is
      type D is new Integer;
      package C is new Ada.Text_IO.Integer_IO (Integer);
   private
      type E is new Integer;
   end Private_Third;

   package Private_Only is
   private
      A : Integer;
   end Private_Only;
end Test_Private;
