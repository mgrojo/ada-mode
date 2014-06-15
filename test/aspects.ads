--  FIXME: Does not compile; add required subprograms

package Aspects is

   type Vector is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   procedure Foo (X : Integer;
                  Y : out Integer)
   with Pre => X > 10 and
               X < 50 and
               F (X),
     Post =>
       Y >= X and
       Some_Very_Verbose_Predicate (X, Y);

   procedure Bar (Y : out Integer)
   with
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

end Aspects;
