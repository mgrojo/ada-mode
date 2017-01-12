--  FIXME: Does not compile; add required subprograms
--
--  Some aspects also tested in ada-nominal.ads

package Aspects is

   type Vector is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   type Date_Set is new Iterator_Interfaces.Forward_Iterator with private
   with
     Constant_Indexing => Element,
     Default_Iterator  => Iterate,
     Iterator_Element  => Ada.Calendar.Time;

   not overriding procedure Foo (X : Integer;
                                 Y : out Integer)
   with Pre => X > 10 and
               X < 50 and
               F (X),
     Post =>
       Y >= X and
       Some_Very_Verbose_Predicate (X, Y);

   procedure Bar (Y : out Integer) with
     Pre => X > 10 and
            X < 50,
     Post =>
       Y >= X and
       Some_Very_Verbose_Predicate (X, Y);

   procedure Baz (Y : out Integer)
   with
     Pre =>
       X > 10 and
       X < 50,
     Post =>
       Y >= X and
       Some_Very_Verbose_Predicate (X, Y);

   type T is tagged private with Type_Invariant'Class => False;

   type U is range 0 .. 1_000_000;
   subtype D is U range 1 .. 10;
   subtype T is U range D'Last + 1 .. D'Last + 365;
   type B is record
      The_Dog : D;
      The_Day : T;
   end record;

   function Wuff return Boolean with Pre =>
       (for all X in U =>
          (if X in D then
             (for some Y in U =>
                Y in T and (X, Y) in B)));

   subtype Integer_String is String
   with Dynamic_Predicate => Integer'Value (Integer_String) in Integer
     or else raise Constraint_Error with "not an integer string";

   type Integer_String is new String
   with Dynamic_Predicate => Integer'Value (Integer_String) in Integer or else
     raise Constraint_Error with "not an integer string";

end Aspects;
