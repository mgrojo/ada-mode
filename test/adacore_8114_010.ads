--  Indentation is sometimes changed when there are comments in
--  the middle

package Adacore_8114_010 is

   type Correct_Indentation is
     (
      Value_1,
      Value_2
     );

   type Wrong_Indentation is
     (     --  The following line is now indented in the same column
      Value_1, -- FIXME: ask
      Value_2
     );    --  This is also aligned on the comment

   type Wrong_Indentation_Two is
     (  Value_0,
        Value_1,
        Value_2
     );   --  This is also aligned on the comment


   procedure Correct_Sub
     (
      Prm : Integer
     );

   procedure Incorrect_Sub
     --  This comment used to make the Prm parameter to be wrongly indented
     (
      Prm : Integer -- indentation changed
     );

end Adacore_8114_010;
