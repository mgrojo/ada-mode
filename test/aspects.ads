--  FIXME: Does not compile; add required subprograms

package Aspects is

   type Vector is tagged private
     with
       Constant_Indexing => Constant_Reference,
       Variable_Indexing => Reference,
       Default_Iterator  => Iterate,
       Iterator_Element  => Element_Type;

end Aspects;
